# Load conda environment, and Keras/Tensorflow

library(keras)
reticulate::use_condaenv("USWildfiresExtremes", required = T)
sess = k_get_session()
sess$list_devices()
library(tensorflow)
# Set tensorflow seed
set_random_seed(1)


nunits = c(32, 32, 32) # set number of CNN units
load("data/df_application.Rdata")

# Extract interpreted covariates, t2m and spi
X_I = X[, , , c(which(cov_names == "t2m"), which(cov_names == "spi"))]
X_N = X[, , , -c(which(cov_names == "t2m"), which(cov_names == "spi"))]


# Extract command line arguments
args = commandArgs(trailingOnly = T)
boot.num = as.numeric(args[1])
set.seed(boot.num)


# Bootstrap data

# Stationary bootstrap with average block length of mean.block.size
mean.block.size = 2
N = dim(Y)[1]

#Generate random block sies
b = rgeom(N, 1 / (mean.block.size)) + 1
b = b[1:min(which(cumsum(b) >= N))]

#Find starting indices
inds = sample(1:N, length(b))
all_inds = c()
for (i in 1:length(b)) {
  block_inds = inds[i]:(inds[i] + b[i] - 1)
  
  #Wrap around indices
  if (sum(block_inds > N) > 0) {
    block_inds[block_inds > N] = 1:sum(block_inds > N)
  }
  all_inds = c(all_inds, block_inds)
  
}
b[length(b)] = b[length(b)] + (N - sum(b))

# Cut down to N only
all_inds = all_inds[1:N]


#Standardise each feature map - Only for non-masked values

for (i in 1:dim(X_N)[4]) {
  tmp = X_N[, , , i]
  m = mean(tmp)
  s = sd(tmp)
  X_N[, , , i] =  (tmp - m) / s
  
}

Y_boot = Y[all_inds, , ]
X_N_boot = X_N[all_inds, , , ]

# Load test, train, validation indices
load(
  file = paste0(
    "intermediates/indices/train_validation_test_indices_boot",
    boot.num,
    ".Rdata"
  )
)




# Get knot evaluations
n.knot = 10 # number of knots.
X_I_basis  <- array(dim = c(dim(X_I), n.knot))


temp = c()
knots = matrix(nrow = dim(X_I)[4], ncol = n.knot)
for (i in 1:dim(X_I)[4]) {
  #Get knots? Just equally spaced quantiles
  temp = X_I[, , , i]
  knots[i, ] = quantile(temp, probs = seq(0, 1, length = n.knot)) #equally spaced quantiles
}

# basis function
rad = function(x, c) {
  out = abs(x - c) ^ 2 * log(abs(x - c))
  out[(x - c) == 0] = 0
  return(out)
}


bases_min <- bases_range <- matrix(nrow = dim(X_I)[4], ncol =
                                     n.knot)
for (i in 1:dim(X_I)[4]) {
  for (k in 1:n.knot) {
    X_I_basis[, , , i, k] = rad(x = X_I[, , , i], c =
                                  knots[i, k])
    #Scale radial bases to aid training
    
    temp = X_I_basis[, , , i, k]
    bases_min[i, k] = mean(temp)
    bases_range[i, k] = sd(temp)
    
    X_I_basis[, , , i, k] = (temp - bases_min[i, k]) / bases_range[i, k]
    
    
  }
}

X_I_basis_boot <- X_I_basis[all_inds, , , , ]




#Binary encoding of zeroes and ones
Y_train <- Y_boot

Y_train[Y_train > 0] = 1
Y_boot[Y_boot > 0] = 1


Y_test <- Y_valid <- Y_train

# Create validation and test data
for (i in 1:length(test_inds)) {
  Y_train[obs_inds[test_inds[i], 1], obs_inds[test_inds[i], 2], obs_inds[test_inds[i], 3]] =
    -1e5
  Y_valid[obs_inds[test_inds[i], 1], obs_inds[test_inds[i], 2], obs_inds[test_inds[i], 3]] =
    -1e5
}
for (i in 1:length(validation_inds)) {
  Y_train[obs_inds[validation_inds[i], 1], obs_inds[validation_inds[i], 2], obs_inds[validation_inds[i], 3]] =
    -1e5
  Y_test[obs_inds[validation_inds[i], 1], obs_inds[validation_inds[i], 2], obs_inds[validation_inds[i], 3]] =
    -1e5
}
for (i in 1:length(train_inds)) {
  Y_test[obs_inds[train_inds[i], 1], obs_inds[train_inds[i], 2], obs_inds[train_inds[i], 3]] =
    -1e5
  Y_valid[obs_inds[train_inds[i], 1], obs_inds[train_inds[i], 2], obs_inds[train_inds[i], 3]] =
    -1e5
}

# Build Keras model




# Choose initial starting value - not used if boot.num > 0
init_p = mean(Y_train[Y_train >= 0] > 0)

k1 <- 3 #kernel dimension
k2 <- 3


#NN model
input_nn <- layer_input(shape = dim(X_N)[2:4], name = 'nn_input')

NNp0Branch <- input_nn %>%
  layer_conv_2d(
    filters = nunits[1],
    kernel_size = c(k1, k2),
    activation = 'relu',
    padding = 'same',
    input_shape = dim(X_N)[2:4],
    name = 'nonlin_p0_cnn1'
  )

for (i in 2:length(nunits)) {
  NNp0Branch <- NNp0Branch %>%
    layer_conv_2d(
      filters = nunits[i],
      kernel_size = c(k1, k2),
      activation = 'relu',
      padding = 'same',
      name = paste0("nonlin_p0_cnn", i)
    )
}

NNp0Branch <- NNp0Branch %>% layer_dense(
  units = 1,
  activation = "linear",
  name = paste0("nonlin_p0_dense"),
  weights = list(matrix(0, nrow = nunits[length(nunits)], ncol =
                          1), array(log(init_p / (
                            1 - init_p
                          ))))
)



#GAM model
input_additive <- layer_input(shape = dim(X_I_basis)[2:5], name = 'additive_input')

#Additive layers
additiveBranchp0 <- input_additive %>%
  layer_reshape(target_shape = c(dim(X_I_basis)[2:3], prod(dim(X_I_basis)[4:5]))) %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    name = 'additive_p0',
    weights = list(matrix(
      0, nrow = prod(dim(X_I_basis)[4:5]), ncol = 1
    ), array(log(init_p / (
      1 - init_p
    ))))
  )


#Add GAM branch to NN branches
pBranchjoined <- layer_add(inputs = c(additiveBranchp0, NNp0Branch))

#Use sigmoid activation so p0 in (0,1)
output <- pBranchjoined %>%
  layer_activation(activation = 'sigmoid')


model <- keras_model(inputs = c(input_additive, input_nn),
                     outputs = c(output))
summary(model)

# Define binary cross entropy loss
bce_loss <- function(y_true, y_pred) {
  K <- backend()
  p = y_pred
  
  obsInds = K$sign(K$relu(y_true + 1e4))
  
  #This will change the predicted p to 0.5 where there are no observations. Will fix likelihood evaluation issues!
  p = p - 3 * (1 - obsInds)
  p = K$relu(p) + 0.5 * (1 - obsInds)
  
  pc = 1 - p
  
  zeroInds = 1 - K$sign(K$abs(y_true))
  
  #This will change the predicted p to 0.5 where there are zero values in y_true. Stops issues multiplying infinity with 0 which can occur for log(p) if p very small
  p = p - 3 * (zeroInds)
  p = K$relu(p) + 0.5 * (zeroInds)
  
  
  
  #This will change the predicted 1-p to 0.5 where there are one values in y_true. Stops issues multiplying infinity with 0 which can occur for log(1-p) if p close to one
  pc = pc - 3 * (1 - zeroInds)
  pc = K$relu(pc) + 0.5 * (1 - zeroInds)
  
  out <- K$abs(y_true) * K$log(p) + K$abs(1 - y_true) * K$log(pc)
  out <- -K$sum(out * obsInds) / K$sum(obsInds)
  
  return(out)
}



# #Compile model
model %>% compile(optimizer = "adam",
                  loss = bce_loss,
                  run_eagerly = T)

#After every epoch, saves the weights if this is the best model

checkpoint <- callback_model_checkpoint(
  paste0("intermediates/models/p0_global_PINN_fit/boot_", boot.num),
  monitor = "val_loss",
  verbose = 0,
  save_best_only = TRUE,
  save_weights_only = TRUE,
  mode = "min",
  save_freq = "epoch"
)


if (boot.num > 1)
  model <- load_model_weights_tf(model,
                                 filepath = paste0("intermediates/models/p0_global_PINN_fit/boot_", 1))


history <- model %>% fit(
  list(X_I_basis_boot, X_N_boot),
  Y_train,
  epochs = 100,
  shuffle = T,
  batch_size = 1,
  callback = list(
    checkpoint,
    callback_early_stopping(
      monitor = "val_loss",
      min_delta = 0,
      patience = 5
    )
  ),
  validation_data = list(
    list(additive_input = X_I_basis_boot , nn_input = X_N_boot),
    Y_valid
  )
  
)



model <- load_model_weights_tf(model,
                               filepath = paste0("intermediates/models/p0_global_PINN_fit/boot_", boot.num))



# Get predictions

pred_p0 <- model %>% predict(list(X_I_basis, X_N))
pred_p0_boot <- model %>% predict(list(X_I_basis_boot, X_N_boot))


st = "intermediates/predictions/p0_global_PINN_fit"
dir.create(st)
save(
  pred_p0,
  pred_p0_boot,
  file = paste0(
    "intermediates/predictions/p0_global_PINN_fit/boot_",
    boot.num,
    ".Rdata"
  )
)



print("Test AUC")
temp = Y_test
temp[temp < 0] = NA

auc.test = pROC::auc(temp[!is.na(temp)], pred_p0_boot[!is.na(temp)])
print(auc.test)

st = "intermediates/scores/p0_global_PINN_fit"
dir.create(st)
save(
  auc.test,
  file = paste0(
    "intermediates/scores/p0_global_PINN_fit/AUC_boot_",
    boot.num,
    ".Rdata"
  )
)




# Save GAM weights
gam_weights <- matrix(nrow = dim(knots)[1], ncol = n.knot)
tmp = t(model$get_layer("additive_p")$get_weights()[[1]])

for (i in 1:dim(knots)[1]) {
  gam_weights[i, ] = tmp[(1 + (i - 1) * n.knot):(i * n.knot)]
}
save(
  knots,
  gam_weights,
  file = paste0(
    "intermediates/models/p0_global_PINN_fit/additivecoeffs_boot_",
    boot.num,
    ".Rdata"
  )
)
