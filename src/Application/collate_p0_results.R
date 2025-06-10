setwd("intermediates/scores/p0_linear_fit")
st.list <- list.files()
tmp <- c()

print("----p0 linear fit----")
print(paste0("Found ", length(st.list), " bootstrap sample estimates"))

tmp <- rep(0, length(st.list))


for (j in 1:length(st.list)) {
  load(st.list[j])
  tmp[j] = auc.test
  
}

print(paste0("Median test AUC ", 100 * round(median(tmp), 4)))

setwd("../p0_GAM_fit")
st.list <- list.files()
tmp <- c()

print("----p0 GAM fit----")
print(paste0("Found ", length(st.list), " bootstrap sample estimates"))

tmp <- rep(0, length(st.list))


for (j in 1:length(st.list)) {
  load(st.list[j])
  tmp[j] = auc.test
  
}

print(paste0("Median test AUC ", 100 * round(median(tmp), 4)))


setwd("../p0_local_PINN_fit")
st.list <- list.files()
tmp <- c()


print("----p0 local PINN fit----")
print(paste0("Found ", length(st.list), " bootstrap sample estimates"))

tmp <- rep(0, length(st.list))


for (j in 1:length(st.list)) {
  load(st.list[j])
  tmp[j] = auc.test
  
}

print(paste0("Median test AUC ", 100 * round(median(tmp), 4)))

setwd("../p0_global_PINN_fit")
st.list <- list.files()
tmp <- c()


print("----p0 global PINN fit----")
print(paste0("Found ", length(st.list), " bootstrap sample estimates"))

tmp <- rep(0, length(st.list))


for (j in 1:length(st.list)) {
  load(st.list[j])
  tmp[j] = auc.test
  
}

print(paste0("Median test AUC ", 100 * round(median(tmp), 4)))

setwd("../p0_NN_fit")
st.list <- list.files()
tmp <- c()


print("----p0 CNN fit----")
print(paste0("Found ", length(st.list), " bootstrap sample estimates"))

tmp <- rep(0, length(st.list))


for (j in 1:length(st.list)) {
  load(st.list[j])

  tmp[j] = auc.test
  
}

print(paste0("Median test AUC ", 100 * round(median(tmp), 4)))