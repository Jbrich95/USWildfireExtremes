
Tmax = 100
source("src/extremal_pp_functions.R")


# Get arguments

args = commandArgs(trailingOnly = T)

rep = as.numeric(args[1])
n = as.numeric(args[2])

set.seed(rep)

# Set hyper-parameters
alpha = 0.5
beta = 0.5
c1 <- c2 <- 5
p_a = 0.05
p_b = 0.2
xi = 0.2

q_a_1 = 1
q_a_2 = 0
q_a_3 = -0.5
s_b_1 = -0.5
s_b_2 = 0
s_b_3 = 0.3

Cor = matrix(0.5, nrow = 10, ncol = 10)
diag(Cor) = 1
cov = mvnfast::rmvn(n, rep(0, 10), sigma = Cor)

# Additive parts
gam.part.q1 = 0
gam.part.q2 = cov[, 4]
gam.part.s1 = 0
gam.part.s2 = 0.2 * (0.1 * cov[, 4] ^ 3 - 0.3 * cov[, 4] ^ 2 - cov[, 4])

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

# Get full PINN parameters
q_a = q_a_1 + q_a_2 * cov[, 1] + q_a_3 * cov[, 2] + gam.part.q1 + gam.part.q2 +
  nn.part.q
s_b = exp(s_b_1 + s_b_2 * cov[, 1] + s_b_3 * cov[, 2] + gam.part.s1 + gam.part.s2 +
            nn.part.s)

mus = q_a - s_b * (l(alpha, xi) - 1) / (l(1 - beta / 2, xi) - l(beta / 2, xi))
sigs = xi * s_b / ((l(1 - beta / 2, xi) - l(beta / 2, xi)))

# Get theoretical 0.99 exceedance quantiles
threshs = rep(NA, n)
for (i in 1:length(threshs))
  threshs[i] = Fthreshinv(0.99, mus[i], sigs[i], xi)


X <- U <- as.matrix(runif(n))

X[U < 0.99] = threshs[U < 0.99] - 1

exceed.inds = which(U >= 0.99)
for (j in 1:length(exceed.inds)) {
  exceedance = Ftinv(U[exceed.inds[j]], mus[exceed.inds[j]], sigs[exceed.inds[j]], xi, threshs[exceed.inds[j]])
  X[exceed.inds[j]] = exceedance + threshs[exceed.inds[j]]
  
}


# Load conda environment, and Keras/Tensorflow

library(keras)
reticulate::use_condaenv("USWildfiresExtremes", required = T)
sess = k_get_session()
sess$list_devices()
library(tensorflow)
# Set tensorflow seed
set_random_seed(1)



Y_train = as.matrix(X)
u_train = as.matrix(threshs)
#Split up linear and GAM inputs for spread and location
X_train_lin = cov[, 1:2]
X_train_add = cov[, 3:4]

X_train_nn = cov[, -c(1, 2, 3, 4)]




# Reshape additives
n.knot = 20 # number of knots. Same for each predictor?
X_I_basis = array(dim = c(dim(X_train_add), n.knot))

knots = matrix(nrow = dim(X_train_add)[2], ncol = n.knot)
for (i in 1:dim(X_train_add)[2]) {
  temp = c(X_train_add[, i])
  
  knots[i, ] = quantile(temp, probs = seq(0, 1, length = n.knot))
  
}

#Get basis functions
rad = function(x, c) {
  out = abs(x - c) ^ 2 * log(abs(x - c))
  out[(x - c) == 0] = 0
  return(out)
}
bases_min <- bases_range <- matrix(nrow = dim(X_train_add)[2], ncol = n.knot)
for (i in 1:dim(X_train_add)[2]) {
  for (k in 1:n.knot) {
    X_I_basis[,  i, k] = rad(x = X_train_add[ , i], c = knots[i, k])
    #Scale radial bases to aid training
    
    temp = c(X_I_basis[,  i, k])
    bases_min[i, k] = min(temp)
    bases_range[i, k] = max(temp) - min(temp)
    
    X_I_basis[,  i, k] = (X_I_basis[,  i, k] -
                             bases_min[i, k]) / bases_range[i, k]
  }
}



# Build Keras model

# Input X_N for q and s
input_nn <- layer_input(shape = dim(X_train_nn)[2], name = 'nn_input')

# Additive input for GAM model

input_add <- layer_input(shape = dim(X_I_basis)[2:3], name = 'additive_input')

#Linear input
input_lin <- layer_input(shape = dim(X_train_lin)[2], name = 'linear_input')


# Input exceedance threshold
input_u <- layer_input(shape = dim(u_train)[2], name = 'u_input')


# Model for xi

#The first layer returns a constant which is untrained. The second layer trains the constant with the initial weight being  equal to qlogis(initial shape)
init_xi = 0.1

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
init_loc = max(q_a) #Set initial location par
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
init_spread = max(s_b)#Set initial location par
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

#Use linear activation - Weights for this layer give regression coefficients + bias = intercept
linBranchSpread <- input_lin %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    input_shape = dim(X_train_lin)[2],
    name = 'lin_spread',
    weights = list(matrix(
      0, nrow = dim(X_train_lin)[2], ncol = 1
    )),
    use_bias = F
  )


linBranchLoc <- input_lin %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    input_shape = dim(X_train_lin)[2],
    name = 'lin_loc',
    weights = list(matrix(
      0, nrow = dim(X_train_lin)[2], ncol = 1
    )),
    use_bias = F
  )


#Additive layers
addBranchloc <- input_add %>%
  layer_reshape(target_shape = prod(dim(X_I_basis)[2:3])) %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    name = 'add_loc',
    weights = list(matrix(
      0, nrow = prod(dim(X_I_basis)[2:3]), ncol = 1
    )),
    use_bias = F
  )

#Additive layers
addBranchspread <- input_add %>%
  layer_reshape(target_shape = prod(dim(X_I_basis)[2:3])) %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    name = 'add_spread',
    weights = list(matrix(
      0, nrow = prod(dim(X_I_basis)[2:3]), ncol = 1
    )),
    use_bias = F
  )

#Add linear branch to nonlinear branches
spreadBranchjoined <- layer_add(inputs = c(linBranchSpread, addBranchspread, spreadBranch))
#spreadBranchjoined<- spreadBranch
locBranchjoined <- layer_add(inputs = c(linBranchLoc, addBranchloc, locBranch))

#Use exponential activation so s > 0
spreadBranchjoined <- spreadBranchjoined %>%
  layer_activation(activation = 'exponential')

#Use linear activation so mu \in mathbb{R}
locBranchjoined <- locBranchjoined %>%
  layer_activation(activation = 'linear')

# Combine input threshold u, and models for q, s, and xi

output <- layer_concatenate(c(input_u, locBranchjoined, spreadBranchjoined, xiBranch))

model <- keras_model(
  inputs = c(input_lin, input_add, input_nn, input_u),
  outputs = c(output)
)
summary(model)

source("src/bGEV_loss.R")

# #Compile model
model %>% compile(optimizer = "adam",
                  loss = bGEV_loss,
                  run_eagerly = T)



#After every epoch, saves the weights if this is the best model

checkpoint <- callback_model_checkpoint(
  paste0(
    "intermediates/models/simulation_B2/sim_model_rep",
    rep,
    "_n",
    n
  ),
  monitor = "val_loss",
  verbose = 0,
  save_best_only = TRUE,
  save_weights_only = TRUE,
  mode = "min",
  save_freq = "epoch"
)

history <- model %>% fit(
  list(X_train_lin, X_I_basis, X_train_nn, u_train),
  Y_train,
  epochs = 100,
  batch_size = 64,
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
                               filepath = paste0(
                                 "intermediates/models/simulation_B2/sim_model_rep",
                                 rep,
                                 "_n",
                                 n
                               ))



predictions <- model %>% predict(list(X_train_lin, X_I_basis, X_train_nn, u_train))

# Evaluate performance
metrics = matrix(nrow = 2, ncol = 4)

linear_coeffs = model$get_layer("lin_spread")$get_weights()

print(paste0("Spread Regression coefficients:"))
print(paste0("Estimated:"))
print(c(linear_coeffs[[1]]))
print(paste0("True:"))
print(c(s_b_2, s_b_3))
metrics[2, 1:2] = (c(linear_coeffs[[1]]) - c(s_b_2, s_b_3)) ^ 2


linear_coeffs = model$get_layer("lin_loc")$get_weights()

print(paste0("Loc Regression coefficients:"))
print(paste0("Estimated:"))
print(c(linear_coeffs[[1]]))
print(paste0("True:"))
print(c(q_a_2, q_a_3))
metrics[1, 1:2] = (c(linear_coeffs[[1]]) - c(q_a_2, q_a_3)) ^ 2


#GAM parts
gam_weights_loc <- matrix(nrow = dim(knots)[1], ncol = n.knot)
gam_weights_spread <- matrix(nrow = dim(knots)[1], ncol = n.knot)
temp1 = t(model$get_layer("add_loc")$get_weights()[[1]])
temp2 = t(model$get_layer("add_spread")$get_weights()[[1]])
for (i in 1:dim(knots)[1]) {
  gam_weights_spread[i, ] = temp2[(1 + (i - 1) * n.knot):(i * n.knot)]
}
for (i in 1:dim(knots)[1]) {
  gam_weights_loc[i, ] = temp1[(1 + (i - 1) * n.knot):(i * n.knot)]
}


for (i in 1:dim(knots)[1]) {
  plt.x = quantile(X_train_add[,  i], prob = seq(0, 1, length = 5000))
  temp = matrix(nrow = length(plt.x), ncol = n.knot)
  for (j in 1:n.knot) {
    temp[, j] = rad(plt.x, knots[i, j])
    temp[, j] = (temp[, j] - bases_min[i, j]) / bases_range[i, j]
  }
  plt.y = temp %*% gam_weights_loc[i, ]
  
  temp = matrix(nrow = 1, ncol = n.knot)
  for (j in 1:n.knot) {
    temp[, j] = rad(0, knots[i, j])
    temp[, j] = (temp[, j] - bases_min[i, j]) / bases_range[i, j]
  }
  y.zero = as.numeric(temp %*% gam_weights_loc[i, ])
  
  plt.y = plt.y - y.zero #subtract the value of the spline at the zero
  if (i == 1) {
    gam.part.q = rep(0, length(plt.x))
    plot(
      plt.x,
      plt.y,
      type = "l",
      main = paste0("Loc GAM ", i),
      xlab = "x",
      ylab = "f(x)",
      ylim = range(gam.part.q, plt.y)
    )
    points(knots[i, ],
           rep(mean(plt.y), n.knot),
           col = "red",
           pch = 2)
    points(plt.x, gam.part.q, col = "blue", type = 'l')
  } else if (i == 2) {
    gam.part.q = plt.x
    plot(
      plt.x,
      plt.y,
      type = "l",
      main = paste0("Loc GAM ", i),
      xlab = "x",
      ylab = "f(x)",
      ylim = range(gam.part.q, plt.y)
    )
    points(knots[i, ],
           rep(mean(plt.y), n.knot),
           col = "red",
           pch = 2)
    points(plt.x, gam.part.q, col = "blue", type = 'l')
  }
  print(mean((gam.part.q - plt.y) ^ 2))
  metrics[1, i + 2] = mean((gam.part.q - plt.y) ^ 2)
  
}

for (i in 1:dim(knots)[1]) {
  plt.x = quantile(X_train_add[,  i], prob = seq(0, 1, length = 5000))
  
  temp = matrix(nrow = length(plt.x), ncol = n.knot)
  for (j in 1:n.knot) {
    temp[, j] = rad(plt.x, knots[i, j])
    
    temp[, j] = (temp[, j] - bases_min[i, j]) / bases_range[i, j]
  }
  plt.y = temp %*% gam_weights_spread[i, ]
  
  temp = matrix(nrow = 1, ncol = n.knot)
  for (j in 1:n.knot) {
    temp[, j] = rad(0, knots[i, j])
    temp[, j] = (temp[, j] - bases_min[i, j]) / bases_range[i, j]
  }
  y.zero = as.numeric(temp %*% gam_weights_spread[i, ])
  plt.y = plt.y - y.zero #subtract the value of the spline at the zero
  if (i == 1) {
    gam.part.s = rep(0, length(plt.x))
    plot(
      plt.x,
      plt.y,
      type = "l",
      main = paste0("Spread GAM ", i),
      xlab = "x",
      ylab = "f(x)",
      ylim = range(gam.part.s, plt.y)
    )
    points(knots[i, ],
           rep(mean(plt.y), n.knot),
           col = "red",
           pch = 2)
    points(plt.x, gam.part.s, col = "blue", type = 'l')
  } else if (i == 2) {
    gam.part.s = 0.2 * (0.1 * plt.x ^ 3 - 0.3 * plt.x ^ 2 - plt.x)
    plot(
      plt.x,
      plt.y,
      type = "l",
      main = paste0("Spread GAM ", i),
      xlab = "x",
      ylab = "f(x)",
      ylim = range(gam.part.s, plt.y)
    )
    points(knots[i, ],
           rep(mean(plt.y), n.knot),
           col = "red",
           pch = 2)
    points(plt.x, gam.part.s, col = "blue", type = 'l')
  }
  print(mean((gam.part.s - plt.y) ^ 2))
  metrics[2, i + 2] = mean((gam.part.s - plt.y) ^ 2)
}

save(metrics,
     file = paste0(
       "intemediates/scores/simulation_B2/rep",
       rep,
       "_n",
       n,
       ".Rdata"
     ))
