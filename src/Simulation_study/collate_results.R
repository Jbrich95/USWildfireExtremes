setwd("intermediates/scores/simulation_B2")
st.list <- list.files()
tmp <- c()

n.seq <- c(1)

for(n in n.seq){
inds <- which(grepl(paste0("n",n,"."), st.list, fixed = T))
st.list.sub = st.list[inds]
print("----B2 parameter estimation----")
print(paste0("n = ", n))
print(paste0("Found ", length(st.list.sub), " scores "))


tmp <- array(dim = c(length(st.list.sub),2,4))

for (j in 1:length(st.list.sub)) {
  load(st.list.sub[j])
  tmp[j,,] = metrics
  
}

print("median estimates")
print(paste0("q_\alpha MSE (eta_1/eta_2): ", round(
  1000 * apply(tmp, c(2, 3), function(x)
    quantile(x, prob = c(0.5)))[1, 1:2], 3
)))

print(paste0("s_\beta MSE (eta_1/eta_2): ", round(
  1000 * apply(tmp, c(2, 3), function(x)
    quantile(x, prob = c(0.5)))[2, 1:2], 3
)))  

print(paste0("q_\alpha MISE (m_1/m_2): ", round(
  1000 * apply(tmp, c(2, 3), function(x)
    quantile(x, prob = c(0.5)))[1, 3:4], 3
)))

print(paste0("s_\beta MISE (m_1/m_2): ", round(
  1000 * apply(tmp, c(2, 3), function(x)
    quantile(x, prob = c(0.5)))[2, 3:4], 3
)))  

print("standard deviation estimates")
print(paste0("q_\alpha MSE (eta_1/eta_2): ", round(
  1000 * apply(tmp, c(2, 3), function(x)
    sd(x))[1, 1:2], 3
)))

print(paste0("s_\beta MSE (eta_1/eta_2): ", round(
  1000 * apply(tmp, c(2, 3), function(x)
    sd(x))[2, 1:2], 3
)))  

print(paste0("q_\alpha MISE (m_1/m_2): ", round(
  1000 * apply(tmp, c(2, 3), function(x)
    sd(x))[1, 3:4], 3
)))

print(paste0("s_\beta MISE (m_1/m_2): ", round(
  1000 * apply(tmp, c(2, 3), function(x)
    sd(x))[2, 3:4], 3
)))  
}
