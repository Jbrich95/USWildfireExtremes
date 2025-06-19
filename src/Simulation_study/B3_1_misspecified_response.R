

Tmax = 100
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
  q_a_1 = 6
  s_b_1 = log(1 / 2)
}
if (case == 2) {
  q_a_1 = 10
  s_b_1 = 0.5
}

Cor = matrix(0.5, nrow = 10, ncol = 10)
diag(Cor) = 1
cov = mvnfast::rmvn(n, rep(0, 10), sigma = Cor)


# NN parts
nn.part.q = 0.1 * (
  cov[, 5] * cov[, 6] + cov[, 6] * (1 - cos(pi * cov[, 6] * cov[, 7])) + 2 *
    sin(cov[, 7]) / (abs(cov[, 7] - cov[, 8]) + 2)
  + 0.2 * (cov[, 8] + cov[, 8] * cov[, 9] / 2) ^ 2 - sqrt(cov[, 9] ^
                                                            2 + cov[, 10] ^ 2 + 2)
  + exp(rowSums(cov[, 5:10] / 10 - 2))
)
nn.part.s = 0.1 * (
  cov[, 5] * cov[, 6] * 0.7 - 5 + cov[, 6] * (1 - cos(pi * cov[, 6] * cov[, 7])) +
    3 * sin(cov[, 7]) / (abs(cov[, 7] - cov[, 8]) + 2)
  + 0.2 * (cov[, 8] + cov[, 8] * cov[, 9] / 2 - 1) ^ 2 - exp(rowSums(cov[, 5:10] /
                                                                       10 - 3))
)


if (case == 1) {
  mu = q_a_1 + 0.2 * nn.part.q
  
  s = rep(exp(s_b_1), length(mu))
  
  
  threshs = rep(NA, n)
  for (i in 1:length(threshs))
    threshs[i] = qlnorm(0.99, mu[i], s[i])
  
  
  Y = rlnorm(n, mu, s)
} else if (case == 2) {
  mu = rep(0, n)
  s = exp(s_b_1 - 3 * nn.part.s)
  
  
  threshs = rep(NA, n)
  for (i in 1:length(threshs))
    threshs[i] = evd::qgpd(0.99, loc = mu[i], s[i], shape = 0.1)
  
  
  Y = evd::rgpd(n, loc = mu, s, shape = 0.1)
}


# Load conda environment, and Keras/Tensorflow

library(keras)
reticulate::use_condaenv("USWildfiresExtremes", required = T)
sess = k_get_session()
sess$list_devices()
library(tensorflow)
# Set tensorflow seed
set_random_seed(1)





Y_train = as.matrix(Y)
u_train = as.matrix(threshs)
#Split up linear and GAM inputs for spread and location
X_train_lin = cov[, 1:2]
X_train_add = cov[, 3:4]

X_train_nn = cov[, -c(1, 2, 3, 4)]





# Build Keras model

# Input X_N for q and s
input_nn <- layer_input(shape = dim(X_train_nn)[2], name = 'nn_input')


# Input exceedance threshold
input_u <- layer_input(shape = dim(u_train)[2], name = 'u_input')



try(init.fit <- ismev::pp.fit(
  c(Y_train)[1:min(100000, n)],
  threshold = c(u_train[, 1])[1:min(100000, n)],
  npy = 12,
  siginit = 10,
  muinit = 10,
  shinit = 0.2
))


if (!exists("init.fit")) {
  init.fit = c()
  init.fit$mle = c(0, 200, 0.2)
}
fit = init.fit



get_q = function(s_beta, xi, alpha, beta, mu) {
  s_beta * (l(alpha, xi) - 1) / (l(1 - beta / 2, xi) - l(beta / 2, xi)) +
    mu
}
get_s = function(xi, beta, sigma) {
  sigma * (l(1 - beta / 2, xi) - l(beta / 2, xi)) / xi
}


init_spread = get_s(xi = fit$mle[3],
                    beta = 0.5,
                    sigma = fit$mle[2])
init_loc = get_q(
  init_spread,
  xi = fit$mle[3],
  alpha = 0.5,
  beta = 0.5,
  mu = fit$mle[1]
)




# Model for xi

#The first layer returns a constant which is untrained. The second layer trains the constant with the initial weight being  equal to qlogis(initial shape)
init_xi = max(0.2, fit$mle[3])

xiBranch <- input_nn %>% layer_dense(
  units = 1 ,
  activation = 'relu',
  input_shape = dim(X_train_nn)[2],
  trainable = F,
  weights = list(matrix(
    0, nrow = dim(X_train_nn)[2], ncol = 1
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


#NN location branch
nunits = c(8, 6, 4, 2, 1)
locBranch <- input_nn %>%
  layer_dense(
    units = nunits[1],
    activation = 'relu',
    input_shape = dim(X_train_nn)[2],
    name = 'nonlin_loc_dense1'
  ) %>%
  layer_dense(units = nunits[2],
              activation = 'relu',
              name = 'nonlin_loc_dense2') %>%
  layer_dense(units = nunits[3],
              activation = 'relu',
              name = 'nonlin_loc_dense3') %>%
  layer_dense(units = nunits[4],
              activation = 'relu',
              name = 'nonlin_loc_dense4') %>%
  layer_dense(
    units = nunits[5],
    activation = "linear",
    name = 'nonlin_loc_dense6',
    weights = list(matrix(0, nrow = nunits[4], ncol = 1), array(init_loc))
  )


#NN spread branch
nunits = c(8, 6, 4, 2, 1)
spreadBranch <- input_nn %>%
  layer_dense(
    units = nunits[1],
    activation = 'relu',
    input_shape = dim(X_train_nn)[2],
    name = 'nonlin_spread_dense1'
  ) %>%
  layer_dense(units = nunits[2],
              activation = 'relu',
              name = 'nonlin_spread_dense2') %>%
  layer_dense(units = nunits[3],
              activation = 'relu',
              name = 'nonlin_spread_dense3') %>%
  layer_dense(units = nunits[4],
              activation = 'relu',
              name = 'nonlin_spread_dense4') %>%
  layer_dense(
    units = nunits[5],
    activation = "linear",
    name = 'nonlin_spread_dense6',
    weights = list(matrix(0, nrow = nunits[4], ncol = 1), array(log(init_spread)))
  )



#Use exponential activation so sig > 0
spreadBranch <- spreadBranch %>%
  layer_activation(activation = 'exponential')

#Use linear activation so mu \in mathbb{R}
locBranch <- locBranch %>%
  layer_activation(activation = 'linear')

output <- layer_concatenate(c(input_u, 
                              locBranch, spreadBranch, xiBranch))

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
    "intermediates/models/simulation_B3_1/sim_model_rep",
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
  list( X_train_nn, u_train),
  Y_train,
  epochs = 250,
  batch_size = 1024,
  validation_split = 0.2,
  callback = list(
    checkpoint,
    callback_early_stopping(
      monitor = "val_loss",
      min_delta = 0,
      patience = 3
    )
  ),
)

model <- load_model_weights_tf(model,
                               filepath =  paste0(
                                 "intermediates/models/simulation_B3_1/sim_model_rep",
                                 rep,
                                 "_n",
                                 n,
                                 "_case",
                                 case
                               ))



predictions <- model %>% predict(list(X_train_nn, u_train))


pred_xi = predictions[,  4]
pred_u = predictions[,  1]
pred_loc = predictions[ , 2]
pred_spread = predictions[ , 3]


metric = 0
n_inf = 0
for (p in seq(0.99, 0.9999, length = 200)) {
  for (i in 1:length(pred_xi)) {
    if (case == 1) {
      the_q = qlnorm(p, meanlog = mu[i ], sdlog = s[i ])
      
    } else if (case == 2) {
      the_q = qgpd(p,
                   loc = mu[i ],
                   scale = s[i ],
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

stLS = metric / (200 * prod(dim(pred_xi)) - n_inf)
print("stLS estimate")

print(stLS)



save(stLS,
     file = paste0(
       "intermediates/scores/simulation_B3_1/rep",
       rep,
       "_n",
       n,
       "_case",
       case,
       ".Rdata"
     ))
