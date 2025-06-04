



# Load conda environment, and Keras/Tensorflow

library(keras)
reticulate::use_condaenv("USWildfiresExtremes", required = T)
sess = k_get_session()
sess$list_devices()
library(tensorflow)
# Set tensorflow seed
set_random_seed(1)



tau = 0.8  # set quantile level, tau
nunits = c(12, 12, 12) # set number of CNN units
load("data/df_application.Rdata")



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

for (i in 1:dim(X)[4]) {
  tmp = X[, , , i]
  m = mean(tmp)
  s = sd(tmp)
  X[, , , i] =  (tmp - m) / s
  
}

Y_boot = Y[all_inds, , ]
X_boot = X[all_inds, , , ]






# Load test, train, validation indices
load(
  file = paste0(
    "intermediates/indices/train_validation_test_indices_boot",
    boot.num,
    ".Rdata"
  )
)

Y_train <- Y_boot
# Fit to non-zero values only
Y_train[Y_train == 0] = -1e5
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
input_nn <- layer_input(shape = dim(X)[2:4], name = 'nn_input')


# Choose initial starting value - not used if boot.num > 0
init_u = (quantile(Y_train[Y_train > 0], tau))


k1 <- 3 # kernel dimension
k2 <- 3

uBranch <- input_nn %>%
  layer_conv_2d(
    filters = nunits[1],
    kernel_size = c(k1, k2),
    activation = 'relu',
    padding = 'same',
    input_shape = dim(X)[2:4],
    name = 'nonlin_u_cnn1'
  )
for (i in 2:length(nunits)) {
  uBranch <- uBranch %>%
    layer_conv_2d(
      filters = nunits[i],
      kernel_size = c(k1, k2),
      activation = 'relu',
      padding = 'same',
      name = paste0("nonlin_u_cnn", i)
    )
}

uBranch <- uBranch %>% layer_dense(
  units = 1,
  activation = "linear",
  name = paste0("nonlin_u_dense"),
  weights = list(matrix(0, nrow = nunits[length(nunits)], ncol =
                          1), array(log(init_u)))
)

# Use exponential activation so u > 0

output <- uBranch %>%
  layer_activation(activation = 'exponential')
model <- keras_model(inputs = c(input_nn), outputs = c(output))
summary(model)

# Define quantile loss
tilted_loss <- function(y_true, y_pred) {
  K <- backend()
  
  # Find inds of non-missing obs.  Remove missing obs, i.e., -1e5. This is achieved by adding an
  # arbitrarily large (<1e5) value to y_true and then taking the sign-ReLU  to set the contribution of these values to zero overall.
  obsInds = K$sign(K$relu(y_true + 1e4))
  
  error = y_true - y_pred
  return(K$sum(K$maximum(tau * error, (tau - 1) * error) * obsInds) /
           K$sum(obsInds))
}

# Compile model
model %>% compile(optimizer = "adam",
                  loss = tilted_loss,
                  run_eagerly = T)

#After every epoch, save the weights if this is the best model

checkpoint <- callback_model_checkpoint(
  paste0("intermediates/models/quantile_fit/boot_", boot.num),
  monitor = "val_loss",
  verbose = 0,
  save_best_only = TRUE,
  save_weights_only = TRUE,
  mode = "min",
  save_freq = "epoch"
)

if (boot.num > 1)
  model <- load_model_weights_tf(model,
                                 filepath = paste0("intermediates/models/quantile_fit/boot_", 1))

history <- model %>% fit(
  list(X_boot),
  Y_train,
  shuffle = T,
  epochs = 100,
  batch_size = 1,
  callback = list(
    checkpoint,
    callback_early_stopping(
      monitor = "val_loss",
      min_delta = 0,
      patience = 5
    )
  ),
  validation_data = list(list(nn_input = X_boot), Y_valid)
  
)


model <- load_model_weights_tf(model,
                               filepath = paste0("intermediates/models/quantile_fit/boot_", boot.num))

# Get predictions
pred_u <- model %>% predict(list(X))
pred_u_boot <- model %>% predict(list(X_boot))

st = "intermediates/predictions/quantile"
dir.create(st)
save(
  pred_u,
  pred_u_boot,
  file = paste0(
    "intermediates/predictions/quantile/boot_",
    boot.num,
    ".Rdata"
  )
)
