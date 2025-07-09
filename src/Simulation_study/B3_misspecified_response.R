

source("src/extremal_pp_functions.R")
# Get arguments



args = commandArgs(trailingOnly = T)

rep = as.numeric(args[1])
n = as.numeric(args[2])
case = as.numeric(args[3])

set.seed(rep)

# Set hyper-parameters


alpha = 0.5
beta = 0.5
c1 <- c2 <- 5
p_a = 0.05
p_b = 0.2
xi = 0.2

# which case?
if (case == 1) {
  q_a_1 = 0
  s_b_1 = log(1 / 2)
}
if (case == 2) {
  q_a_1 = 0
  s_b_1 = 0.5
}


load("data/df_application.Rdata")
#Standardise each feature map - Only for non-masked values

for (i in 1:dim(X)[4]) {
  tmp = X[, , , i]
  m = mean(tmp)
  s = sd(tmp)
  X[, , , i] =  (tmp - m) / s
  
}

#Block bootstrap time inds
#Generate random block sizes
mean.block.size = 2
b = rgeom(n, 1 / (mean.block.size)) + 1
b = b[1:min(which(cumsum(b) >= n))]

#Find starting indices
N = dim(Y)[1]
inds = sample(1:N, length(b), replace = T)
all_inds = c()
for (i in 1:length(b)) {
  block_inds = inds[i]:(inds[i] + b[i] - 1)
  
  #Wrap around indices
  if (sum(block_inds > N) > 0) {
    block_inds[block_inds > N] = 1:sum(block_inds > N)
  }
  all_inds = c(all_inds, block_inds)
  
}
b[length(b)] = b[length(b)] + (n - sum(b))

# Cut down to n only
time.inds = all_inds[1:n]
cov.inds = 1:10

X = X[time.inds, , , cov.inds]
if(length(time.inds)==1) dim(X) = c(1,dim(X))
dim(X) = c(length(time.inds), dim(Y)[2:3], 10)


# NN parts

nn.part.q = 10+0.1 * (
  X[,,, 5] * X[,,, 6] + X[,,, 6] * (1 - cos(pi * X[,,, 6] * X[,,, 7])) + 2 *
    sin(X[,,, 7]) / (abs(X[,,, 7] - X[,,, 8]) + 2)
  + 0.2 * (X[,,, 8] + X[,,, 8] * X[,,, 9] / 2) ^ 2 - sqrt(X[,,, 9] ^
                                                            2 + X[,,, 10] ^ 2 + 2)
  + exp(rowSums(X[,,, 5:10] / 10 - 2))
)

nn.part.s = 0.1 * (
  X[,,, 5] * X[,,, 6] * 0.7 - 10 + X[,,, 6] * (1 - cos(pi * X[,,, 6] * X[,,, 7])) +
    3 * sin(X[,,, 7]) / (abs(X[,,, 7] - X[,,, 8]) + 2)
  + 0.2 * (X[,,, 8] + X[,,, 8] * X[,,, 9] / 2 - 1) ^ 2 - exp(rowSums(X[,,, 5:10] /
                                                                       10 - 3))
)


if (case == 1) {
  mus = q_a_1 + 0.2 * nn.part.q
  
  sigs = rep(exp(s_b_1), length(mus))
  dim(sigs) = dim(mus)
} else if (case == 2) {
  
  sigs = exp(s_b_1 - 3 * nn.part.s)
  mus = rep(0, length(sigs))
  dim(mus) = dim(sigs)
  
}
if(length(time.inds)==1) dim(mus) = c(1,dim(mus))
if(length(time.inds)==1) dim(sigs) = c(1,dim(sigs))
  

# Get theoretical 0.8 exceedance quantiles
threshs = mus
if (case == 1) {
  for (i in 1:length(threshs))
    threshs[i] = qlnorm(0.8, mus[i], sigs[i])
  
} else if (case == 2) {
  for (i in 1:length(threshs))
    threshs[i] = evd::qgpd(0.8, loc = mus[i], sigs[i], shape = 0.1)
  
}

Y = threshs

# Generate response data
for (j in 1:length(Y)) {
  if (case == 1) {
    Y[j] = rlnorm(1, mus[j], sigs[j])
  } else if (case == 2) {
    Y[j] = evd::rgpd(n,
                     loc = mus[j],
                     scale = sigs[j],
                     shape = 0.1)
  }
}


# Load conda environment, and Keras/Tensorflow

library(keras)
reticulate::use_condaenv("USWildfiresExtremes", required = T)
sess = k_get_session()
sess$list_devices()
library(tensorflow)
# Set tensorflow seed
set_random_seed(1)



dim(Y) = c(dim(Y),1)
dim(threshs) = c(dim(threshs),1)
valid.inds = sample(1:length(Y), length(Y)/5, replace = F, )

Y_train <- Y_valid <-Y

Y_train[valid.inds] = -1e10
Y_valid[-valid.inds] = -1e10




# Build Keras model

# Input X_N for q and s
input_nn <- layer_input(shape = dim(X)[2:4], name = 'nn_input')


# Input exceedance threshold
input_u <- layer_input(shape = dim(threshs)[2:4], name = 'u_input')


# Model for xi

# The first layer returns a constant which is untrained. The second layer trains the constant with the initial weight being  equal to qlogis(initial shape)
init_xi = 0.1

xiBranch <- input_nn %>% layer_dense(
  units = 1 ,
  activation = 'relu',
  input_shape = dim(X)[2:4],
  trainable = F,
  weights = list(matrix(
    0, nrow = dim(X)[4], ncol = 1
  ), array(1, dim = c(1))),
  name = 'xi_dense'
) %>%
  layer_dense(
    units = 1 ,
    activation = 'sigmoid',
    use_bias = F,
    weights = list(matrix(
      qlogis(init_xi), nrow = 1, ncol = 1
    )),
    name = 'xi_activation'
  )


k1 <- 3 # kernel dimension
k2 <- 3
nunits = c(16,16,16) # CNN units


#NN location branch
init_loc = 10 #Set initial location par


locBranch <- input_nn %>%
  layer_conv_2d(
    filters = nunits[1],
    kernel_size = c(k1, k2),
    activation = 'relu',
    padding = 'same',
    input_shape = dim(X)[2:4],
    name = 'nonlin_loc_cnn1'
  )
for (i in 2:length(nunits)) {
  locBranch <- locBranch %>%
    layer_conv_2d(
      filters = nunits[i],
      kernel_size = c(k1, k2),
      activation = 'relu',
      padding = 'same',
      name = paste0("nonlin_loc_cnn", i)
    )
}
locBranch <- locBranch %>% layer_dense(
  units = 1,
  activation = "linear",
  name = paste0("nonlin_loc_dense"),
  weights = list(matrix(0, nrow = nunits[length(nunits)], ncol =
                          1), array(log(init_loc)))
)


#NN spread branch
init_spread = 10#Set initial location par
sBranch <- input_nn %>%
  layer_conv_2d(
    filters = nunits[1],
    kernel_size = c(k1, k2),
    activation = 'relu',
    padding = 'same',
    input_shape = dim(X)[2:4],
    name = 'nonlin_s_cnn1'
  )
for (i in 2:length(nunits)) {
  sBranch <- sBranch %>%
    layer_conv_2d(
      filters = nunits[i],
      kernel_size = c(k1, k2),
      activation = 'relu',
      padding = 'same',
      name = paste0("nonlin_s_cnn", i)
    )
}
sBranch <- sBranch %>% layer_dense(
  units = 1,
  activation = "linear",
  name = paste0("nonlin_s_dense"),
  weights = list(matrix(0, nrow = nunits[length(nunits)], ncol =
                          1), array(log(init_spread)))
)

#Use exponential activation so sig > 0
sBranch <- sBranch %>%
  layer_activation(activation = 'exponential')

#Use linear activation so mu \in mathbb{R}
locBranch <- locBranch %>%
  layer_activation(activation = 'linear')

output <- layer_concatenate(c(input_u, 
                              locBranch, sBranch, xiBranch))

model <- keras_model(inputs = c(input_nn, input_u),
                     outputs = c(output))
summary(model)

source("src/bGEV_loss.R")

# #Compile model
model %>% compile(optimizer = "adam",
                  loss = bGEV_loss,
                  run_eagerly = T)

#After every epoch, saves the weights if this is the best model

checkpoint <- callback_model_checkpoint(
  paste0(
    "intermediates/models/simulation_B3/sim_model_rep",
    rep,
    "_n",
    n,
    "_case",
    case
  ),
  monitor = "val_loss",
  verbose = 0,
  save_best_only = TRUE,
  save_weights_only = TRUE,
  mode = "min",
  save_freq = "epoch"
)


history <- model %>% fit(
  list( X, threshs),
  Y_train,
  epochs = 500,
  shuffle = T,
  batch_size = 16,
  callback = list(
    checkpoint,
    callback_early_stopping(
      monitor = "val_loss",
      min_delta = 0,
      patience = 20
    )
  ),
  validation_data = list(
    list(
      nn_input = X,
      u_input = threshs
    ),
    Y_valid
  )
  
)

model <- load_model_weights_tf(model,
                               filepath =  paste0(
                                 "intermediates/models/simulation_B3/sim_model_rep",
                                 rep,
                                 "_n",
                                 n,
                                 "_case",
                                 case
                               ))


predictions <- model %>% predict(list(X, threshs))

pred_xi = c(predictions[1, ,, 4])
pred_loc = c(predictions[1 ,,, 2])
pred_spread = c(predictions[1 ,,, 3])

mus = c(mus[1,,])
sigs = c(sigs[1,,])

print("Getting stLS values")
metric = 0
n_inf = 0
for (p in seq(0.99, 0.9999, length = 100)) {
  for (i in 1:length(pred_xi)) {
    if (case == 1) {
      the_q = qlnorm(p, meanlog = mus[i ], sdlog = sigs[i ])
      
    } else if (case == 2) {
      the_q = evd::qgpd(p,
                   loc = mus[i ],
                   scale = sigs[i ],
                   shape = 0.1)
      
    }
    pred_p = apply(cbind(pred_loc[i ], pred_spread[i], pred_xi[i ], the_q), 1, function(x) {
      # (
      (1 / 12) * (
        pbGEV(
          x[4],
          q_a = x[1],
          s_b = x[2],
          xi = x[3],
          alpha = 0.5,
          beta = 0.5,
          p_a = 0.05,
          p_b = 0.2,
          c1 = 5,
          c2 = 5,
          log = T
        )
      )
      # )^{1/12}
      
    })
    pred_p = abs(expm1(pred_p))
    n_inf = n_inf + sum(pred_p == 0)
    pred_p[pred_p == 0] = NA
    metric = metric + sum((log(pred_p) - log(1 - p)) ^ 2, na.rm = T)
    
  }
  
}

stLS = metric / (100 * length(pred_xi) - n_inf)
print("stLS estimate")

print(stLS)



save(stLS,
     file = paste0(
       "intermediates/scores/simulation_B3/rep",
       rep,
       "_n",
       n,
       "_case",
       case,
       ".Rdata"
     ))
