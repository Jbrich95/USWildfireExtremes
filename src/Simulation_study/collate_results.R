setwd("intermediates/scores/simulation_B2")
st.list <- list.files()
tmp <- c()
print("----B2 parameter estimation----")
n.seq <- c(1,10,100,150)

for(n in n.seq){
inds <- which(grepl(paste0("n",n,"."), st.list, fixed = T))
st.list.sub = st.list[inds]

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


setwd("../simulation_B3")
st.list <- list.files()
tmp <- c()
print("----B3 response misspecification----")

n.seq <- c(1,10,100,150)
cases <- c(1,2)
for(case in cases){
for(n in n.seq){
  
  inds <- which(grepl(paste0("n",n,"_case",case,"."), st.list, fixed = T))
  st.list.sub = st.list[inds]

  print(paste0("n = ", n))
  print(paste0("Found ", length(st.list.sub), " scores "))
  
  
  tmp <- array(dim = c(length(st.list.sub)))
  
  for (j in 1:length(st.list.sub)) {
    load(st.list.sub[j])
    tmp[j] = stLS
    
  }
  

  print(paste0("Case ",case, ", n = ",n, ", mean stLS: ", round(
   mean(tmp), 3
  ), ". sd: ", round(
    sd(tmp), 3
  )))

  
}
}
