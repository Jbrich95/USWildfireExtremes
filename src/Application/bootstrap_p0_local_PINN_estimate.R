# Load conda environment, and Keras/Tensorflow

library(keras)
reticulate::use_condaenv("USWildfiresExtremes", required = T)
sess = k_get_session()
sess$list_devices()
library(tensorflow)
# Set tensorflow seed
set_random_seed(1)


nunits = c(32, 32, 32) # set number of CNN units
load("data/df_application_statewise.Rdata")


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



for (i in 1:dim(X_I_1)[4]) {
  temp = X_I_1[, , , i]
  m = mean(temp[temp != 0])
  s = sd(temp[temp != 0])
  temp = (temp - m) / s
  X_I_1[, , , i][X_I_1[, , , i] != 0] = temp[X_I_1[, , , i] !=
                                               0]
  
  
  temp = X_I_2[, , , i]
  m = mean(temp[temp != 0])
  s = sd(temp[temp != 0])
  temp = (temp - m) / s
  X_I_2[, , , i][X_I_2[, , , i] != 0] = temp[X_I_2[, , , i] !=
                                               0]
}


Y_boot = Y[all_inds, , ]
X_N_boot = X_N[all_inds, , , ]

X_I_1_boot <- X_I_1[all_inds, , , ]
X_I_2_boot <- X_I_2[all_inds, , , ]

# Load test, train, validation indices
load(
  file = paste0(
    "intermediates/indices/train_validation_test_indices_boot",
    boot.num,
    ".Rdata"
  )
)


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

#Linear models
input_lin1 <- layer_input(shape = dim(X_I_1)[2:4], name = 'lin_input1')
input_lin2 <- layer_input(shape = dim(X_I_2)[2:4], name = 'lin_input2')


#NN model
input_nn <- layer_input(shape = dim(X_N)[2:4], name = 'nn_input')



# Choose initial starting value - not used if boot.num > 0
init_p = mean(Y_train[Y_train >= 0] > 0)

k1 <- 3 #kernel dimension
k2 <- 3




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




#Linear layers
linBranchp1 <- input_lin1 %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    input_shape = dim(X_I_1)[2:4],
    name = 'lin_p1',
    weights = list(matrix(
      0, nrow = dim(X_I_1)[4], ncol = 1
    )),
    use_bias = F
  )

linBranchp2 <- input_lin2 %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    input_shape = dim(X_I_2)[2:4],
    name = 'lin_p2',
    weights = list(matrix(
      0, nrow = dim(X_I_2)[4], ncol = 1
    )),
    use_bias = F
  )

#Add linear branch to nonlinear branches
pBranchjoined <- layer_add(inputs = c(linBranchp1, linBranchp2, NNp0Branch))

#Use sigmoid activation so p0 in (0,1)
output <- pBranchjoined %>%
  layer_activation(activation = 'sigmoid')

model <- keras_model(inputs = c(input_lin1, input_lin2, input_nn),
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
  paste0("intermediates/models/p0_local_PINN_fit/boot_", boot.num),
  monitor = "val_loss",
  verbose = 0,
  save_best_only = TRUE,
  save_weights_only = TRUE,
  mode = "min",
  save_freq = "epoch"
)


if (boot.num > 1)
  model <- load_model_weights_tf(model,
                                 filepath = paste0("intermediates/models/p0_local_PINN_fit/boot_", 1))


history <- model %>% fit(
  list(X_I_1_boot, X_I_2_boot, X_N_boot),
  Y_train,
  epochs = 1000,
  batch_size = 161,
  callback = list(checkpoint),
  validation_data = list(
    list(
      lin_input1 = X_I_1_boot,
      lin_input2 = X_I_2_boot,
      nn_input = X_N_boot
    ),
    Y_valid
  )
  
)

model <- load_model_weights_tf(model,
                               filepath = paste0("intermediates/models/p0_local_PINN_fit/boot_", boot.num))


# Get predictions
pred_p0 <- model %>% predict(list(X_I_1,X_I_2,X_N))
pred_p0_boot <- model %>% predict(list(X_I_1_boot,X_I_2_boot,X_N_boot))


save(
  pred_p,
  pred_p_boot,
  file = paste0(
    "intermediates/predictions/p0_local_PINN_fit/boot_",
    boot.num,
    ".Rdata"
  )
)



print("Test AUC")
temp = Y_test
temp[temp < 0] = NA

auc.test = pROC::auc(temp[!is.na(temp)], pred_p0_boot[!is.na(temp)])
print(auc.test)

save(
  auc.test,
  file = paste0(
    "intermediates/scores/p0_local_PINN_fit/AUC_boot_",
    boot.num,
    ".Rdata"
  )
)


# Save SVC estimates


linear_coeffs1 = model$get_layer("lin_p1")$get_weights()[[1]]
linear_coeffs2 = model$get_layer("lin_p2")$get_weights()[[1]]
save(
  linear_coeffs1,
  linear_coeffs2,
  file = paste0(
    "intermediates/models/p0_local_PINN_fit/linearcoeffs_boot_",
    boot.num,
    ".Rdata"
  )
)
