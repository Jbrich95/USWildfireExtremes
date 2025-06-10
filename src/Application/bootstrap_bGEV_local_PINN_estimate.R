# Load conda environment, and Keras/Tensorflow

library(keras)
reticulate::use_condaenv("USWildfiresExtremes", required = T)
sess = k_get_session()
sess$list_devices()
library(tensorflow)
# Set tensorflow seed
set_random_seed(1)


xi.nunits = c(8, 8, 8) # set number of MLP units for xi
q.nunits = c(24, 24, 24) # set number of CNN units for q
s.nunits = c(10, 10, 10) # set number of CNN units for s
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
b[b > 10] = 10 # Set max block size to 10 to mitigate RAM issues
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

# Create coordinates for modelling xi

X_lonlat <- array(dim = c(nrow(Y), dim(lonlat)))
for (t in 1:nrow(X_lonlat))
  X_lonlat[t, , , ] = lonlat
for (i in 1:dim(X_lonlat)[4]) {
  temp = X_lonlat[, , , i]
  m = mean(temp)
  s = sd(temp)
  temp = (temp - m) / s
  X_lonlat[, , , i] = temp
  
}


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

# Load predicted threshold
load(paste0(
  "intermediates/predictions/quantile/boot_",
  boot.num,
  ".Rdata"
))



# Build Keras model

#Linear models
input_lin1 <- layer_input(shape = dim(X_I_1)[2:4], name = 'lin_input1')
input_lin2 <- layer_input(shape = dim(X_I_2)[2:4], name = 'lin_input2')



# Input X_N for q and s
input_nn <- layer_input(shape = dim(X_N)[2:4], name = 'nn_input')

# Input exceedance threshold
input_u <- layer_input(shape = dim(pred_u)[2:4], name = 'u_input')

# Input latlon for xi
input_coords <- layer_input(shape = dim(X_lonlat)[2:4], name = 'coord_input')


# Model for xi

#The first layer returns a constant which is untrained. The second layer trains the constant with the initial weight being  equal to qlogis(initial shape)
# Choose initial starting value - not used if boot.num > 0
init_xi = 0.1

xiBranch <- input_coords %>%
  layer_dense(
    units = xi.nunits[1],
    activation = 'relu',
    input_shape = dim(X_lonlat)[2:4],
    name = 'nonlin_xi_dense1'
  )
for (i in 2:length(xi.nunits)) {
  xiBranch <- xiBranch %>%
    layer_dense(
      units = xi.nunits[i],
      activation = 'relu',
      name = paste0("nonlin_xi_dense", i)
    )
}

#Use sigmoid activation so 0 \leq xi \leq 1
xiBranch <- xiBranch %>% layer_dense(
  units = 1,
  activation = "sigmoid",
  name = paste0("nonlin_xi_dense"),
  weights = list(matrix(0, nrow = xi.nunits[length(xi.nunits)], ncol =
                          1), array(qlogis(init_xi)))
)


k1 <- 3 # kernel dimension
k2 <- 3


# Model for location q

# Choose initial starting value - not used if boot.num > 0
init_loc = 30

locBranch <- input_nn %>%
  layer_conv_2d(
    filters = q.nunits[1],
    kernel_size = c(k1, k2),
    activation = 'relu',
    padding = 'same',
    input_shape = dim(X_N)[2:4],
    name = 'nonlin_loc_cnn1'
  )
for (i in 2:length(q.nunits)) {
  locBranch <- locBranch %>%
    layer_conv_2d(
      filters = q.nunits[i],
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
  weights = list(matrix(0, nrow = q.nunits[length(q.nunits)], ncol =
                          1), array(log(init_loc)))
)




#Linear layers
linBranchLoc1 <- input_lin1 %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    input_shape = dim(X_I_1)[2:4],
    name = 'lin_loc1',
    weights = list(matrix(
      0, nrow = dim(X_I_1)[4], ncol = 1
    )),
    use_bias = F
  )

linBranchLoc2 <- input_lin2 %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    input_shape = dim(X_I_2)[2:4],
    name = 'lin_loc2',
    weights = list(matrix(
      0, nrow = dim(X_I_2)[4], ncol = 1
    )),
    use_bias = F
  )


#Add linear branch to nonlinear branches
locBranch <- layer_add(inputs = c(linBranchLoc1, linBranchLoc2, locBranch))

# Use exponential activation so q > 0
locBranch <- locBranch %>%
  layer_activation(activation = 'exponential')



# Model for spread s

# Choose initial starting value - not used if boot.num > 0
init_spread = 40#

spreadBranch <- input_nn %>%
  layer_conv_2d(
    filters = s.nunits[1],
    kernel_size = c(k1, k2),
    activation = 'relu',
    padding = 'same',
    input_shape = dim(X_N)[2:4],
    name = 'nonlin_s_cnn1'
  )
for (i in 2:length(s.nunits)) {
  spreadBranch <- spreadBranch %>%
    layer_conv_2d(
      filters = s.nunits[i],
      kernel_size = c(k1, k2),
      activation = 'relu',
      padding = 'same',
      name = paste0("nonlin_s_cnn", i)
    )
}
spreadBranch <- spreadBranch %>% layer_dense(
  units = 1,
  activation = "linear",
  name = paste0("nonlin_s_dense"),
  weights = list(matrix(0, nrow =
                          s.nunits[length(s.nunits)], ncol = 1), array(log(init_spread)))
)


#Linear layers for s
linBranchSpread1 <- input_lin1 %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    input_shape = dim(X_I_1)[2:4],
    name = 'lin_spread1',
    weights = list(matrix(
      0, nrow = dim(X_I_1)[4], ncol = 1
    )),
    use_bias = F
  )
linBranchSpread2 <- input_lin2 %>%
  layer_dense(
    units = 1,
    activation = 'linear',
    input_shape = dim(X_I_2)[2:4],
    name = 'lin_spread2',
    weights = list(matrix(
      0, nrow = dim(X_I_2)[4], ncol = 1
    )),
    use_bias = F
  )

#Add linear branch to nonlinear branches

spreadBranch <- layer_add(inputs = c(linBranchSpread1, linBranchSpread2, spreadBranch))

#Use exponential activation so s > 0
spreadBranch <- spreadBranch %>%
  layer_activation(activation = 'exponential')


# Combine input threshold u, and models for q, s, and xi
output <- layer_concatenate(c(input_u, locBranch, spreadBranch, xiBranch))


model <- keras_model(
  inputs = c(input_lin1, input_lin2, input_nn, input_u, input_coords),
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
  paste0("intermediates/models/bGEV_local_PINN_fit/boot_", boot.num),
  monitor = "val_loss",
  verbose = 0,
  save_best_only = TRUE,
  save_weights_only = TRUE,
  mode = "min",
  save_freq = "epoch"
)

if (boot.num > 1)
  model <- load_model_weights_tf(model,
                                 filepath = paste0("intermediates/models/bGEV_local_PINN_fit/boot_", 1))



history <- model %>% fit(
  list(X_I_1_boot, X_I_2_boot, X_N_boot, pred_u_boot, X_lonlat),
  Y_train,
  epochs = 200,
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
      lin_input1 = X_I_1_boot,
      lin_input2 = X_I_2_boot,
      nn_input = X_N_boot,
      u_input = pred_u_boot,
      coord_input = X_lonlat
    ),
    Y_valid
  )
  
)

model <- load_model_weights_tf(model,
                               filepath = paste0("intermediates/models/bGEV_local_PINN_fit/boot_", boot.num))


# Get predictions
pred_bGEV <- model %>% predict(list(X_I_1, X_I_2, X_N, pred_u, X_lonlat))
pred_bGEV_boot <- model %>% predict(list(X_I_1_boot, X_I_2_boot,X_N_boot, pred_u_boot, X_lonlat))

st = "intermediates/predictions/bGEV_local_PINN_fit"
dir.create(st)
save(
  pred_bGEV,
  pred_bGEV_boot,
  file = paste0(
    "intermediates/predictions/bGEV_local_PINN_fit/boot_",
    boot.num,
    ".Rdata"
  )
)




nll <- k_get_value(bGEV_loss(k_constant(Y_test), k_constant(pred_bGEV_boot)))
print("Test loss")
print(nll)

st = "intermediates/scores/bGEV_local_PINN"
dir.create(st)
save(nll,
     file = paste0(
       "intermediates/scores/bGEV_local_PINN/test_loss_",
       boot.num,
       ".Rdata"
     ))




# Save model
model %>% save_model_tf(paste0("intermediates/models/bGEV_local_PINN/boot_", boot.num))





dat = c(Y[Y > 0])
pred_tall = matrix(ncol = 3, nrow = length(dat))
for (i in 1:3)
  pred_tall[, i] = c(pred_bGEV[, , , i + 1][Y > 0])



exp_dat = apply(cbind(dat, pred_tall), 1, function(x) {
  (pbGEV(
    x[1],
    q_a = x[2],
    s_b = x[3],
    xi = x[4],
    alpha = 0.5,
    beta = 0.5,
    p_a = 0.05,
    p_b = 0.2,
    c1 = 5,
    c2 = 5
  )) ^ {
    1 / 12
  }
  
})

exp_dat = qexp(exp_dat)
p_min = 0.95
n_p = length(exp_dat) * (1 - p_min)
ps = p_min + (1:n_p) / (n_p + 1) * (1 - p_min)


sMAD = mean(abs(quantile(exp_dat, ps) - qexp(ps)))
print("sMAD")
print(sMAD)
save(sMAD,
     file = paste0(
       "intermediates/scores/bGEV_local_PINN/sMAD_",
       boot.num,
       ".Rdata"
     ))


# Get twCRPS score
source("src/twCRPS.R")

validation_inds = which(Y_test > 0 , arr.ind = T) #Take positive values only
valid_values = Y_test[validation_inds]
xi.test = pred_bGEV_boot[cbind(validation_inds, rep(4, length(valid_values)))]
u.test = pred_bGEV_boot[cbind(validation_inds, rep(1, length(valid_values)))]
q.test = pred_bGEV_boot[cbind(validation_inds, rep(2, length(valid_values)))]
s.test = pred_bGEV_boot[cbind(validation_inds, rep(3, length(valid_values)))]

probs = array(dim = c(length(valid_values), length(u_ba)))

for (i in 1:length(u_ba)) {
  probs[, i] = apply(cbind(q.test, s.test, xi.test), 1, function(x) {
    (
      pbGEV(
        u_ba[i],
        q_a = x[1],
        s_b = x[2],
        xi = x[3],
        alpha = 0.5,
        beta = 0.5,
        p_a = 0.05,
        p_b = 0.2,
        c1 = 5,
        c2 = 5
      )
    ) ^ {
      1 / 12
    }
    
  })
  
}


twcrps = get_twcrps(probs, valid_values, u_ba, weights_ba)
print("twCRPS")
print(twcrps)
save(twcrps,
     file = paste0(
       "intermediates/scores/bGEV_local_PINN/twCRPS_",
       boot.num,
       ".Rdata"
     ))



# Save SVC estimates


linear_coeffs_loc1 = model$get_layer("lin_loc1")$get_weights()[[1]]
linear_coeffs_loc2 = model$get_layer("lin_loc2")$get_weights()[[1]]
linear_coeffs_spread1 = model$get_layer("lin_spread1")$get_weights()[[1]]
linear_coeffs_spread2 = model$get_layer("lin_spread2")$get_weights()[[1]]

save(
  linear_coeffs_loc1,
  linear_coeffs_loc2,
  linear_coeffs_spread1,
  linear_coeffs_spread2,
  file = paste0(
    "intermediates/models/bGEV_local_PINN_fit/linearcoeffs_boot_",
    boot.num,
    ".Rdata"
  )
)
