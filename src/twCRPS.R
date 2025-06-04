
s = function(prediction_ba, obs, u_ba, weights_ba){
  distr_obs = c()
  for(k in 1:length(u_ba)){
    distr_obs = cbind(distr_obs, ifelse(u_ba[k] < obs, 0, 1))
  }
  weights_mat = matrix(weights_ba, ncol = length(weights_ba), nrow = length(obs), byrow = TRUE)
  score_ba = sum(weights_mat * (distr_obs - prediction_ba)^2,na.rm=T)
  score_ba
} 

u_ba = sqrt(c(30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250, 
         300, 400, 500, 1000, 1500, 2000, 5000, 10000, 20000, 30000, 40000, 
         50000, 1e+05))

weights_ba = 1-(1+(u_ba+1)^2/1000)^{-1/4}
weights_ba = weights_ba/weights_ba[length(weights_ba)]

