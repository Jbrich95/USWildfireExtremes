setwd("intermediates/scores/bGEV_linear")
st.list <- list.files()
tmp <- c()

median.sMAD.list <- median.twCRPS.list <- median.loss.list <- vector("list", 6)

print("----bGEV linear fit----")
print(paste0("Found ", length(st.list) / 3, " bootstrap sample estimates"))

tmp1 <- tmp2 <- tmp3 <- rep(0, length(st.list) / 3)

twCRPS.inds <- which(grepl("twCRPS", st.list))

for (j in 1:length(twCRPS.inds)) {
  load(st.list[twCRPS.inds[j]])
  
  tmp1[j] = twcrps
  
}

sMAD.inds <- which(grepl("sMAD", st.list))

for (j in 1:length(sMAD.inds)) {
  load(st.list[sMAD.inds[j]])
  
  tmp2[j] = sMAD
  
}
loss.inds <- which(grepl("test_loss", st.list))

for (j in 1:length(loss.inds)) {
  load(st.list[loss.inds[j]])
  
  tmp3[j] = nll
  
}

median.sMAD.list[[1]] = median(tmp2)
median.twCRPS.list[[1]] = median(tmp1)
median.loss.list[[1]] = median(tmp3)
print(paste0("Median test loss ", median(tmp3)))
print(paste0("Median sMAD ", median(tmp2)))
print(paste0("Median twCRPS ", median(tmp1)))


setwd("../bGEV_GAM")
st.list <- list.files()
tmp <- c()


print("----bGEV GAM fit----")
print(paste0("Found ", length(st.list) / 3, " bootstrap sample estimates"))

tmp1 <- tmp2 <- tmp3 <- rep(0, length(st.list) / 3)

twCRPS.inds <- which(grepl("twCRPS", st.list))

for (j in 1:length(twCRPS.inds)) {
  load(st.list[twCRPS.inds[j]])
  
  tmp1[j] = twcrps
  
}

sMAD.inds <- which(grepl("sMAD", st.list))

for (j in 1:length(sMAD.inds)) {
  load(st.list[sMAD.inds[j]])
  
  tmp2[j] = sMAD
  
}
loss.inds <- which(grepl("test_loss", st.list))

for (j in 1:length(loss.inds)) {
  load(st.list[loss.inds[j]])
  
  tmp3[j] = nll
  
}

median.sMAD.list[[2]] = median(tmp2)
median.twCRPS.list[[2]] = median(tmp1)
median.loss.list[[2]] = median(tmp3)
print(paste0("Median test loss ", median(tmp3)))
print(paste0("Median sMAD ", median(tmp2)))
print(paste0("Median twCRPS ", median(tmp1)))

setwd("../bGEV_local_PINN")
st.list <- list.files()
tmp <- c()


print("----bGEV local PINN fit----")
print(paste0("Found ", length(st.list) / 3, " bootstrap sample estimates"))

tmp1 <- tmp2 <- tmp3 <- rep(0, length(st.list) / 3)

twCRPS.inds <- which(grepl("twCRPS", st.list))

for (j in 1:length(twCRPS.inds)) {
  load(st.list[twCRPS.inds[j]])
  
  tmp1[j] = twcrps
  
}

sMAD.inds <- which(grepl("sMAD", st.list))

for (j in 1:length(sMAD.inds)) {
  load(st.list[sMAD.inds[j]])
  
  tmp2[j] = sMAD
  
}
loss.inds <- which(grepl("test_loss", st.list))

for (j in 1:length(loss.inds)) {
  load(st.list[loss.inds[j]])
  
  tmp3[j] = nll
  
}

median.sMAD.list[[3]] = median(tmp2)
median.twCRPS.list[[3]] = median(tmp1)
median.loss.list[[3]] = median(tmp3)
print(paste0("Median test loss ", median(tmp3)))
print(paste0("Median sMAD ", median(tmp2)))
print(paste0("Median twCRPS ", median(tmp1)))

setwd("../bGEV_local_homo_xi_PINN")
st.list <- list.files()
tmp <- c()

print("----bGEV local PINN (homogeneous xi) fit----")
print(paste0("Found ", length(st.list) / 3, " bootstrap sample estimates"))

tmp1 <- tmp2 <- tmp3 <- rep(0, length(st.list) / 3)

twCRPS.inds <- which(grepl("twCRPS", st.list))

for (j in 1:length(twCRPS.inds)) {
  load(st.list[twCRPS.inds[j]])
  
  tmp1[j] = twcrps
  
}

sMAD.inds <- which(grepl("sMAD", st.list))

for (j in 1:length(sMAD.inds)) {
  load(st.list[sMAD.inds[j]])
  
  tmp2[j] = sMAD
  
}
loss.inds <- which(grepl("test_loss", st.list))

for (j in 1:length(loss.inds)) {
  load(st.list[loss.inds[j]])
  
  tmp3[j] = nll
  
}

median.sMAD.list[[4]] = median(tmp2)
median.twCRPS.list[[4]] = median(tmp1)
median.loss.list[[4]] = median(tmp3)
print(paste0("Median test loss ", median(tmp3)))
print(paste0("Median sMAD ", median(tmp2)))
print(paste0("Median twCRPS ", median(tmp1)))

setwd("../bGEV_global_PINN")
st.list <- list.files()
tmp <- c()

print("----bGEV global PINN fit----")
print(paste0("Found ", length(st.list) / 3, " bootstrap sample estimates"))

tmp1 <- tmp2 <- tmp3 <- rep(0, length(st.list) / 3)

twCRPS.inds <- which(grepl("twCRPS", st.list))

for (j in 1:length(twCRPS.inds)) {
  load(st.list[twCRPS.inds[j]])
  
  tmp1[j] = twcrps
  
}

sMAD.inds <- which(grepl("sMAD", st.list))

for (j in 1:length(sMAD.inds)) {
  load(st.list[sMAD.inds[j]])
  
  tmp2[j] = sMAD
  
}
loss.inds <- which(grepl("test_loss", st.list))

for (j in 1:length(loss.inds)) {
  load(st.list[loss.inds[j]])
  
  tmp3[j] = nll
  
}

median.sMAD.list[[5]] = median(tmp2)
median.twCRPS.list[[5]] = median(tmp1)
median.loss.list[[5]] = median(tmp3)
print(paste0("Median test loss ", median(tmp3)))
print(paste0("Median sMAD ", median(tmp2)))
print(paste0("Median twCRPS ", median(tmp1)))


setwd("../bGEV_NN")
st.list <- list.files()
tmp <- c()


print("----bGEV CNN fit----")
print(paste0("Found ", length(st.list) / 3, " bootstrap sample estimates"))

tmp1 <- tmp2 <- tmp3 <- rep(0, length(st.list) / 3)

twCRPS.inds <- which(grepl("twCRPS", st.list))

for (j in 1:length(twCRPS.inds)) {
  load(st.list[twCRPS.inds[j]])
  
  tmp1[j] = twcrps
  
}

sMAD.inds <- which(grepl("sMAD", st.list))

for (j in 1:length(sMAD.inds)) {
  load(st.list[sMAD.inds[j]])
  
  tmp2[j] = sMAD
  
}
loss.inds <- which(grepl("test_loss", st.list))

for (j in 1:length(loss.inds)) {
  load(st.list[loss.inds[j]])
  
  tmp3[j] = nll
  
}

median.sMAD.list[[6]] = median(tmp2)
median.twCRPS.list[[6]] = median(tmp1)
median.loss.list[[6]] = median(tmp3)
print(paste0("Median test loss ", median(tmp3)))
print(paste0("Median sMAD ", median(tmp2)))
print(paste0("Median twCRPS ", median(tmp1)))


print("Centred values")
min.smad = min(unlist(median.sMAD.list))
min.twcrps = min(unlist(median.twCRPS.list))
min.loss = min(unlist(median.loss.list))







setwd("../bGEV_linear")
st.list <- list.files()
tmp <- c()

median.sMAD.list <- median.twCRPS.list <- median.loss.list <- vector("list", 6)

print("----bGEV linear fit----")
tmp1 <- tmp2 <- tmp3 <- rep(0, length(st.list) / 3)

twCRPS.inds <- which(grepl("twCRPS", st.list))

for (j in 1:length(twCRPS.inds)) {
  load(st.list[twCRPS.inds[j]])
  
  tmp1[j] = twcrps
  
}

sMAD.inds <- which(grepl("sMAD", st.list))

for (j in 1:length(sMAD.inds)) {
  load(st.list[sMAD.inds[j]])
  
  tmp2[j] = sMAD
  
}
loss.inds <- which(grepl("test_loss", st.list))

for (j in 1:length(loss.inds)) {
  load(st.list[loss.inds[j]])
  
  tmp3[j] = nll
  
}

median.sMAD.list[[1]] = median(tmp2)
median.twCRPS.list[[1]] = median(tmp1)
median.loss.list[[1]] = median(tmp3)
print(paste0("Median test loss ", round(median(tmp3) - min.loss, 1)))
print(paste0("Median sMAD ", round(100 * (
  median(tmp2) - min.smad
), 1)))
print(paste0("Median twCRPS ", round(median(tmp1) - min.twcrps, 1)))


setwd("../bGEV_GAM")
st.list <- list.files()
tmp <- c()


print("----bGEV GAM fit----")

tmp1 <- tmp2 <- tmp3 <- rep(0, length(st.list) / 3)

twCRPS.inds <- which(grepl("twCRPS", st.list))

for (j in 1:length(twCRPS.inds)) {
  load(st.list[twCRPS.inds[j]])
  
  tmp1[j] = twcrps
  
}

sMAD.inds <- which(grepl("sMAD", st.list))

for (j in 1:length(sMAD.inds)) {
  load(st.list[sMAD.inds[j]])
  
  tmp2[j] = sMAD
  
}
loss.inds <- which(grepl("test_loss", st.list))

for (j in 1:length(loss.inds)) {
  load(st.list[loss.inds[j]])
  
  tmp3[j] = nll
  
}

median.sMAD.list[[2]] = median(tmp2)
median.twCRPS.list[[2]] = median(tmp1)
median.loss.list[[2]] = median(tmp3)
print(paste0("Median test loss ", round(median(tmp3) - min.loss, 1)))
print(paste0("Median sMAD ", round(100 * (
  median(tmp2) - min.smad
), 1)))
print(paste0("Median twCRPS ", round(median(tmp1) - min.twcrps, 1)))

setwd("../bGEV_local_PINN")
st.list <- list.files()
tmp <- c()


print("----bGEV local PINN fit----")

tmp1 <- tmp2 <- tmp3 <- rep(0, length(st.list) / 3)

twCRPS.inds <- which(grepl("twCRPS", st.list))

for (j in 1:length(twCRPS.inds)) {
  load(st.list[twCRPS.inds[j]])
  
  tmp1[j] = twcrps
  
}

sMAD.inds <- which(grepl("sMAD", st.list))

for (j in 1:length(sMAD.inds)) {
  load(st.list[sMAD.inds[j]])
  
  tmp2[j] = sMAD
  
}
loss.inds <- which(grepl("test_loss", st.list))

for (j in 1:length(loss.inds)) {
  load(st.list[loss.inds[j]])
  
  tmp3[j] = nll
  
}

median.sMAD.list[[3]] = median(tmp2)
median.twCRPS.list[[3]] = median(tmp1)
median.loss.list[[3]] = median(tmp3)
print(paste0("Median test loss ", round(median(tmp3) - min.loss, 1)))
print(paste0("Median sMAD ", round(100 * (
  median(tmp2) - min.smad
), 1)))
print(paste0("Median twCRPS ", round(median(tmp1) - min.twcrps, 1)))

setwd("../bGEV_local_homo_xi_PINN")
st.list <- list.files()
tmp <- c()

print("----bGEV local PINN (homogeneous xi) fit----")

tmp1 <- tmp2 <- tmp3 <- rep(0, length(st.list) / 3)

twCRPS.inds <- which(grepl("twCRPS", st.list))

for (j in 1:length(twCRPS.inds)) {
  load(st.list[twCRPS.inds[j]])
  
  tmp1[j] = twcrps
  
}

sMAD.inds <- which(grepl("sMAD", st.list))

for (j in 1:length(sMAD.inds)) {
  load(st.list[sMAD.inds[j]])
  
  tmp2[j] = sMAD
  
}
loss.inds <- which(grepl("test_loss", st.list))

for (j in 1:length(loss.inds)) {
  load(st.list[loss.inds[j]])
  
  tmp3[j] = nll
  
}

median.sMAD.list[[4]] = median(tmp2)
median.twCRPS.list[[4]] = median(tmp1)
median.loss.list[[4]] = median(tmp3)
print(paste0("Median test loss ", round(median(tmp3) - min.loss, 1)))
print(paste0("Median sMAD ", round(100 * (
  median(tmp2) - min.smad
), 1)))
print(paste0("Median twCRPS ", round(median(tmp1) - min.twcrps, 1)))

setwd("../bGEV_global_PINN")
st.list <- list.files()
tmp <- c()

print("----bGEV global PINN fit----")

tmp1 <- tmp2 <- tmp3 <- rep(0, length(st.list) / 3)

twCRPS.inds <- which(grepl("twCRPS", st.list))

for (j in 1:length(twCRPS.inds)) {
  load(st.list[twCRPS.inds[j]])
  
  tmp1[j] = twcrps
  
}

sMAD.inds <- which(grepl("sMAD", st.list))

for (j in 1:length(sMAD.inds)) {
  load(st.list[sMAD.inds[j]])
  
  tmp2[j] = sMAD
  
}
loss.inds <- which(grepl("test_loss", st.list))

for (j in 1:length(loss.inds)) {
  load(st.list[loss.inds[j]])
  
  tmp3[j] = nll
  
}

median.sMAD.list[[5]] = median(tmp2)
median.twCRPS.list[[5]] = median(tmp1)
median.loss.list[[5]] = median(tmp3)
print(paste0("Median test loss ", round(median(tmp3) - min.loss, 1)))
print(paste0("Median sMAD ", round(100 * (
  median(tmp2) - min.smad
), 1)))
print(paste0("Median twCRPS ", round(median(tmp1) - min.twcrps, 1)))


setwd("../bGEV_NN")
st.list <- list.files()
tmp <- c()


print("----bGEV CNN fit----")

tmp1 <- tmp2 <- tmp3 <- rep(0, length(st.list) / 3)

twCRPS.inds <- which(grepl("twCRPS", st.list))

for (j in 1:length(twCRPS.inds)) {
  load(st.list[twCRPS.inds[j]])
  
  tmp1[j] = twcrps
  
}

sMAD.inds <- which(grepl("sMAD", st.list))

for (j in 1:length(sMAD.inds)) {
  load(st.list[sMAD.inds[j]])
  
  tmp2[j] = sMAD
  
}
loss.inds <- which(grepl("test_loss", st.list))

for (j in 1:length(loss.inds)) {
  load(st.list[loss.inds[j]])
  
  tmp3[j] = nll
  
}

median.sMAD.list[[6]] = median(tmp2)
median.twCRPS.list[[6]] = median(tmp1)
median.loss.list[[6]] = median(tmp3)
print(paste0("Median test loss ", round(median(tmp3) - min.loss, 1)))
print(paste0("Median sMAD ", round(100 * (
  median(tmp2) - min.smad
), 1)))
print(paste0("Median twCRPS ", round(median(tmp1) - min.twcrps, 1)))
