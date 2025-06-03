

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


# Create validation and testing data by removing sapce-time clusters

obs_inds = which(Y_boot >= 0, arr.ind = T)

# Reorder obs_inds
obs_inds = obs_inds[order(obs_inds[, 1]), ]
#Subset into training and test data using space time GP # Split into yearly blocks and simulate upon each
Z = rep(NA, nrow(obs_inds))



obs_inds_sub = obs_inds[obs_inds[, 1] == 1, ]
coords = matrix(0, ncol = 2, nrow = nrow(obs_inds_sub))
for (j in 1:nrow(obs_inds_sub))
  coords[j, ] = c(lonlat[obs_inds_sub[j, 2], obs_inds_sub[j, 3], 1], lonlat[obs_inds_sub[j, 2], obs_inds_sub[j, 3], 2])



tmp = vector("list", length(unique(b)))
for (i in 1:length(unique(b))) {
  all_coords = c()
  for (j in 1:unique(b)[i])
    all_coords = rbind(all_coords, coords)
  Cor_mat = fields::Exponential(rdist.earth(all_coords), range = 100) * fields::Exponential(rdist(rep(1:unique(b)[i], each =
                                                                                                        nrow(coords))), range = 5)
  
  tmp[[i]] = mvnfast::rmvn(sum(b == unique(b)[i]), mu = rep(0, nrow(Cor_mat)), sigma =
                             Cor_mat)
  
}

unique_int = rep(1, length(unique(b)))
Z <- c()
for (j in 1:length(b)) {
  b_int = b[j]
  
  Z_sub = tmp[[which(unique(b) == b_int)]][unique_int[b_int], ]
  Z <- c(Z, Z_sub)
  unique_int[b_int] = unique_int[b_int] + 1
}


rm(Cor_mat)
cs = quantile(Z, probs = c(0.2, 0.8), na.rm = T)
NA_inds = matrix(ncol = 2, nrow = length(Z))
NA_inds[, 1] = Z < cs[1]
NA_inds[, 2] = Z >= cs[2]





#Standardise each feature map - Only for non-masked values

for (i in 1:dim(X)[4]) {
  tmp = X[, , , i]
  m = mean(tmp)
  s = sd(tmp)
  X[, , , i] =  (tmp - m) / s
  
}

Y_boot = Y[all_inds, , ]
X_boot = X[all_inds, , , ]





train_inds = which(NA_inds[, 1] == 0 & NA_inds[, 2] == 0)
validation_inds = which(NA_inds[, 1] == 1)
test_inds = which(NA_inds[, 2] == 1)



save(
  file = paste0(
    "intermediates/indices/train_validation_test_indices_boot",
    boot.num,
    ".Rdata"
  ),
  train_inds,
  validation_inds,
  test_inds,
  obs_inds
)
