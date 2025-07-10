
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

X_orig = X
X = X[time.inds, , , cov.inds]
if(length(time.inds)==1) dim(X) = c(1,dim(X))
dim(X) = c(length(time.inds), dim(Y)[2:3], 10)



# Linear coefficients

if(case == 1){
  q_a_1 = 0.8
  q_a_2 = 2
  s_b_1 = 0.4
  s_b_2 = -0.2
  
  # Additive parts
  gam.part.q1 = 0.2*(0.1*X[,,,3]^3-X[,,,3]^2+X[,,,3])
  gam.part.q2 = 0.2*(-0.4*X[,,,4]^3-2*X[,,,4])
  gam.part.s1 = 0.2*(0.1*X[,,,3]^3-0.1*X[,,,3]^2+X[,,,3])
  gam.part.s2 = 0.2*(-0.1*X[,,,4]^3+0.2*X[,,,4]^2-0.5*X[,,,4])
}
if(case == 2){
q_a_1 = 0
q_a_2 = -2
s_b_1 = 0
s_b_2 = 0.3

# Additive parts
gam.part.q1 = 0
gam.part.q2 = X[,,, 4]
gam.part.s1 = 0
gam.part.s2 = 0.5 * (0.1 * X[,,, 4] ^ 3 - 0.3 * X[, ,,4] ^ 2 - X[,,, 4])
}

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

# Get full PINN parameters
q_a =  q_a_1 * X[,,, 1] + q_a_2 * X[,,, 2] + gam.part.q1 + gam.part.q2 +
  nn.part.q
s_b = exp( s_b_1 * X[,,, 1] + s_b_2 * X[,,, 2] + gam.part.s1 + gam.part.s2 +
            nn.part.s)

# Map back to the mu and sigma of the usual PP parameterisation
mus = q_a - s_b * (l(alpha, xi) - 1) / (l(1 - beta / 2, xi) - l(beta / 2, xi))
sigs = xi * s_b / ((l(1 - beta / 2, xi) - l(beta / 2, xi)))

if(length(time.inds)==1) dim(mus) = c(1,dim(mus))
if(length(time.inds)==1) dim(sigs) = c(1,dim(sigs))

# Get theoretical 0.8 exceedance quantiles
threshs = mus
for (i in 1:length(threshs))
  threshs[i] = Fthreshinv(0.8, mus[i], sigs[i], xi)


U <- as.matrix(runif(prod(dim(mus))))
dim(U) = dim(mus)

Y <- U

Y[U < 0.8] = threshs[U < 0.8] - 1

exceed.inds = which(U >= 0.8)
for (j in 1:length(exceed.inds)) {
  exceedance = Ftinv(U[exceed.inds[j]], mus[exceed.inds[j]], sigs[exceed.inds[j]], xi, threshs[exceed.inds[j]])
  Y[exceed.inds[j]] = exceedance + threshs[exceed.inds[j]]
  
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


#Split up linear and GAM inputs for spread and location
X_L = X[, , , 1:2]
X_A = X[, , , 3:4]
X_A_orig = X_orig[, , , 3:4]
X_N = X[, , , -c(1, 2, 3, 4)]

if(length(time.inds)==1)
  dim(X_L) = c(1, dim(X_L))
if (length(time.inds) == 1)
  dim(X_A) = c(1, dim(X_A))
if (length(time.inds) == 1)
  dim(X_N) = c(1, dim(X_N)
  )



# Get knot evaluations
n.knot = 10 # number of knots.
X_A_basis  <- array(dim = c(dim(X_A), n.knot))


temp = c()
knots = matrix(nrow = dim(X_A)[4], ncol = n.knot)
for (i in 1:dim(X_A)[4]) {
  #Get knots? Just equally spaced quantiles
  temp = X_A[, , , i]
  knots[i, ] = quantile(temp, probs = seq(0, 1, length = n.knot)) #equally spaced quantiles
}

# basis function
rad = function(x, c) {
  out = abs(x - c) ^ 2 * log(abs(x - c))
  out[(x - c) == 0] = 0
  return(out)
}


bases_min <- bases_range <- matrix(nrow = dim(X_A)[4], ncol =
                                     n.knot)
for (i in 1:dim(X_A)[4]) {
  for (k in 1:n.knot) {
    X_A_basis[, , , i, k] = rad(x = X_A[, , , i], c =
                                  knots[i, k])
    #Scale radial bases to aid training
    
    temp = X_A_basis[, , , i, k]
    bases_min[i, k] = mean(temp)
    bases_range[i, k] = sd(temp)
    
    X_A_basis[, , , i, k] = (temp - bases_min[i, k]) / bases_range[i, k]
    
    
  }
}




# Build Keras model

# Input X_N for q and s
input_nn <- layer_input(shape = dim(X_N)[2:4], name = 'nn_input')


# Input exceedance threshold
input_u <- layer_input(shape = dim(threshs)[2:4], name = 'u_input')


# Additive input for GAM model
input_additive <- layer_input(shape = dim(X_A_basis)[2:5], name = 'additive_input')

#Linear input
input_linear <- layer_input(shape = dim(X_L)[2:4], name = 'linear_input')

# Model for xi

# The first layer returns a constant which is untrained. The second layer trains the constant with the initial weight being  equal to qlogis(initial shape)
init_xi = 0.3

xiBranch <- input_nn %>% layer_dense(
  units = 1 ,
  activation = 'relu',
  input_shape = dim(X_N)[2:4],
  trainable = F,
  weights = list(matrix(
    0, nrow = dim(X_N)[4], ncol = 1
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
    input_shape = dim(X_N)[2:4],
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
init_spread = 10 #Set initial spread par
sBranch <- input_nn %>%
  layer_conv_2d(
    filters = nunits[1],
    kernel_size = c(k1, k2),
    activation = 'relu',
    padding = 'same',
    input_shape = dim(X_N)[2:4],
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


#Use linear activation - Weights for this layer give regression coefficients + bias = intercept
linBranchSpread <- input_linear %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    input_shape = dim(X_L)[2:4],
    name = 'lin_spread',
    weights = list(matrix(
      0, nrow = dim(X_L)[4], ncol = 1
    )),
    use_bias = F
  )


linBranchLoc <- input_linear %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    input_shape = dim(X_L)[2:4],
    name = 'lin_loc',
    weights = list(matrix(
      0, nrow = dim(X_L)[4], ncol = 1
    )),
    use_bias = F
  )


#Additive layers
addBranchloc <- input_additive %>%
  layer_reshape(target_shape = c(dim(X_A_basis)[2:3], prod(dim(X_A_basis)[4:5]))) %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    name = 'add_loc',
    weights = list(matrix(
      0, nrow = prod(dim(X_A_basis)[4:5]), ncol = 1
    )),
    use_bias = F
  )

#Additive layers
addBranchspread <- input_additive %>%
  layer_reshape(target_shape = c(dim(X_A_basis)[2:3], prod(dim(X_A_basis)[4:5]))) %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    name = 'add_spread',
    weights = list(matrix(
      0, nrow = prod(dim(X_A_basis)[4:5]), ncol = 1
    )),
    use_bias = F
  )

#Add linear branch to nonlinear branches
spreadBranchjoined <- layer_add(inputs = c(linBranchSpread, addBranchspread, sBranch))
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
  inputs = c(input_linear, input_additive, input_nn, input_u),
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
  list(X_L,X_A_basis, X_N, threshs),
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
      linear_input = X_L,
      additive_input = X_A_basis,
      nn_input = X_N,
      u_input = threshs
    ),
    Y_valid
  )
  
)


model <- load_model_weights_tf(model,
                               filepath = paste0(
                                 "intermediates/models/simulation_B2/sim_model_rep",
                                 rep,
                                 "_n",
                                 n,
                                 "_case",
                                 case
                               ))



predictions <- model %>% predict(list(X_L, X_A_basis, X_N, threshs))

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

print("MISE estimates")
for (i in 1:dim(knots)[1]) {
  plt.x = quantile(X_A_orig[, , , i], prob = seq(0, 1, length = 5000))
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
    if (case == 1)
      gam.part.q = 0.2 * (0.1 * plt.x ^ 3 - plt.x ^ 2 + plt.x)
    
    if (case == 2)
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
    if (case == 2)
      gam.part.q = plt.x
    if (case == 1)
      gam.part.q = 0.2 * (-0.4 * plt.x ^ 3 - 2 * plt.x)

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
  plt.x = quantile(X_A_orig[, , , i], prob = seq(0, 1, length = 5000))
  
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
    if (case == 2)
      gam.part.s = rep(0, length(plt.x))
    if (case == 1)
      gam.part.s = 0.2 * (0.1 * plt.x ^ 3 - 0.1 * plt.x ^ 2 + plt.x)
    
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
    if (case == 2)
      gam.part.s = 0.2 * (0.1 * plt.x ^ 3 - 0.3 * plt.x ^ 2 - plt.x)
    if (case == 1)
      gam.part.s = 0.2 * (-0.1 * plt.x ^ 3 + 0.2 * plt.x ^ 2 - 0.5 * plt.x)
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
       "intermediates/scores/simulation_B2/rep",
       rep,
       "_n",
       n,
       "_case",
       case,
       ".Rdata"
     ))
