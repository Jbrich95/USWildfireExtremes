



l = function(a, xi) {
  K <- backend()
  
  (-K$log(a)) ^ (-xi)
}
l0 = function(a) {
  K <- backend()
  
  K$log(-K$log(a))
}

Finverse = function(x, q_a, s_b, xi, alpha, beta) {
  K <- backend()
  
  ((-K$log(x)) ^ (-xi) - l(alpha, xi)) * s_b / (l(1 - beta / 2, xi) - l(beta /
                                                                          2, xi)) + q_a
}

logH = function(y,
                q_a,
                s_b,
                xi,
                alpha,
                beta,
                a,
                b,
                p_a,
                p_b,
                c1,
                c2,
                obsInds) {
  K <- backend()
  
  #Upper tail
  z1 = (y - q_a) / (s_b / (l(1 - beta / 2, xi) - l(beta / 2, xi))) + l(alpha, xi)
  z1 = K$relu(z1)
  
  zeroz1_inds = 1 - K$sign(z1)
  t1 = (z1 + (1 - obsInds) + zeroz1_inds) ^ (-1 / xi)
  
  #Weight
  
  temp = (y - a) / (b - a) * obsInds #Need to set values <0 and >1 to 0 and 1, otherwise function breaks
  temp = K$relu(temp)
  temp = 1 - temp
  temp = K$relu(temp)
  temp = 1 - temp
  p = tf$math$betainc(c1, c2, temp)
  
  #Lower tail
  q_a_tilde = a - (b - a) * (l0(alpha) - l0(p_a)) / (l0(p_a) - l0(p_b))
  s_b_tilde = (b - a) * (l0(beta / 2) - l0(1 - beta / 2)) / (l0(p_a) - l0(p_b))
  s_b_tilde = s_b_tilde + (1 - obsInds)
  
  z2 = (y - q_a_tilde) / (s_b_tilde / (l0(beta / 2) - l0(1 - beta / 2))) -
    l0(alpha)
  z2 = z2 * obsInds
  
  t2 = K$exp(-z2)
  
  
  
  
  return((p * (-t1) * (1 - zeroz1_inds) + (1 - p) * (-t2)) * obsInds)
}

lambda = function(y,
                  q_a,
                  s_b,
                  xi,
                  alpha,
                  beta,
                  a,
                  b,
                  p_a,
                  p_b,
                  c1,
                  c2,
                  obsInds,
                  exceedInds) {
  K <- backend()
  #Upper tail
  z1 = ((y - q_a) / (s_b / (l(1 - beta / 2, xi) - l(beta / 2, xi)))) + l(alpha, xi)
  z1 = K$relu(z1)
  z1 = z1 * exceedInds
  zeroz1_inds = 1 - K$sign(z1)
  t1 = (z1 + (1 - obsInds) + zeroz1_inds) ^ (-1 / xi)
  
  #Weight
  
  temp = (y - a) / (b - a) * obsInds #Need to set values <0 and >1 to 0 and 1, otherwise function breaks
  temp = K$relu(temp)
  temp = 1 - temp
  temp = K$relu(temp)
  temp = 1 - temp
  p = tf$math$betainc(c1, c2, temp)
  
  pprime = temp ^ (c1 - 1) * (1 - temp) ^ (c2 - 1) / beta(c1, c2)
  pprime = pprime / (b - a) * exceedInds
  
  
  #Lower tail
  q_a_tilde = a - (b - a) * (l0(alpha) - l0(p_a)) / (l0(p_a) - l0(p_b))
  s_b_tilde = (b - a) * (l0(beta / 2) - l0(1 - beta / 2)) / (l0(p_a) - l0(p_b))
  s_b_tilde = s_b_tilde + (1 - obsInds)
  z2 = (y - q_a_tilde) / (s_b_tilde / (l0(beta / 2) - l0(1 - beta / 2))) -
    l0(alpha)
  z2 = z2 * exceedInds
  t2 = K$exp(-z2)
  
  
  z1prime = (l(1 - beta / 2, xi) - l(beta / 2, xi)) / s_b
  
  z2prime = (l0(beta / 2) - l0(1 - beta / 2)) / s_b_tilde
  
  out = ((pprime * (-t1)
          + p * (1 / xi) * t1 / (z1 + zeroz1_inds) * z1prime) * (1 - zeroz1_inds)
         - pprime * (-t2)
         + (1 - p) * t2 * z2prime)
  return(out * exceedInds)
}

bGEV_loss <- function(y_true, y_pred) {
  alpha = 0.5
  beta = 0.5
  
  p_a = 0.05 #Need alpha >= p_b and beta/2 >= p_b
  p_b = 0.2
  
  c1 = 5
  c2 = 5
  
  library(tensorflow)
  K <- backend()
  
  u = y_pred[all_dims(), 1]
  q_a = y_pred[all_dims(), 2]
  s_b = y_pred[all_dims(), 3]
  xi = y_pred[all_dims(), 4]
  
  y <- y_true[all_dims(), 1]
  
  
  # Find inds of non-missing obs.  Remove missing obs, i.e., -1e10. This is achieved by adding an
  # arbitrarily large (<1e10) value to y and then taking the sign ReLu
  obsInds = K$sign(K$relu(y + 1e4))
  
  #Find exceedance inds
  exceed = y - u
  exceedInds = K$sign(K$relu(exceed))
  
  
  a = Finverse(p_a, q_a, s_b, xi, alpha, beta)
  b = Finverse(p_b, q_a, s_b, xi, alpha, beta)
  b = b + (1 - obsInds)
  s_b = s_b + (1 - obsInds)
  
  #Use exceedance only only
  lam = lambda(y,
               q_a,
               s_b,
               xi,
               alpha,
               beta,
               a,
               b,
               p_a,
               p_b,
               c1,
               c2,
               obsInds,
               exceedInds)
  loglam = K$log(lam + (1 - exceedInds)) * exceedInds
  
  
  
  #Use all values of y i.e., non-exceedances + exceedances.
  
  LAM = -logH(u, q_a, s_b, xi, alpha, beta, a, b, p_a, p_b, c1, c2, obsInds) #1/12 as 12 obs per year
  return(-(K$sum(loglam)
           - (1 / 12) * K$sum(LAM)))
}


# Distrubtion function
pbGEV = function(y,
                 q_a,
                 s_b,
                 xi,
                 alpha,
                 beta,
                 p_a,
                 p_b,
                 c1,
                 c2,
                 log = F) {
  #Not using tensors
  l_r = function(a, xi) {
    (-log(a)) ^ (-xi)
  }
  l0_r = function(a) {
    log(-log(a))
  }
  
  Finverse_r = function(x, q_a, s_b, xi, alpha, beta) {
    ((-log(x)) ^ (-xi) - l_r(alpha, xi)) * s_b / (l_r(1 - beta / 2, xi) - l_r(beta /
                                                                                2, xi)) + q_a
  }
  
  
  a = Finverse_r(p_a, q_a, s_b, xi, alpha, beta)
  b = Finverse_r(p_b, q_a, s_b, xi, alpha, beta)
  
  #Upper tail
  z1 = (y - q_a) / (s_b / (l_r(1 - beta / 2, xi) - l_r(beta / 2, xi))) +
    l_r(alpha, xi)
  z1 = max(z1, 0)
  
  t1 = (z1) ^ (-1 / xi)
  
  #Weight
  
  
  p = pbeta((y - a) / (b - a), c1, c2)
  
  #Lower tail
  q_a_tilde = a - (b - a) * (l0_r(alpha) - l0_r(p_a)) / (l0_r(p_a) - l0_r(p_b))
  s_b_tilde = (b - a) * (l0_r(beta / 2) - l0_r(1 - beta / 2)) / (l0_r(p_a) -
                                                                   l0_r(p_b))
  
  z2 = (y - q_a_tilde) / (s_b_tilde / (l0_r(beta / 2) - l0_r(1 - beta /
                                                               2))) - l0_r(alpha)
  
  
  t2 = exp(-z2)
  
  if (log == F) {
    out = exp(p * (-t1) + (1 - p) * (-t2))
    if (p == 0) {
      out = exp((1 - p) * (-t2))
    } else if (p == 1) {
      out = exp(p * (-t1))
    }
  } else if (log == T) {
    out = p * (-t1) + (1 - p) * (-t2)
    if (p == 0) {
      out = (1 - p) * (-t2)
    } else if (p == 1) {
      out = p * (-t1)
    }
  }
  return(out)
}

qbGEV=function(prob,q_a,s_b,xi,alpha,beta,p_a,p_b,c1,c2){
  
  if(prob < p_b){
    func=function(x){
      prob-pbGEV(x,q_a,s_b,xi,alpha,beta,p_a,p_b,c1,c2)
    }
    return( uniroot(f=func,interval=c(-1e8,1e8),tol=1e-5 )$root)
  }else{
    #Not using tensors
    l_r=function(a,xi){
      
      (-log(a))^(-xi)
    }
    l0_r = function(a){
      
      
      log(-log(a))
    }
    Finverse = function(x,q_a,s_b,xi,alpha,beta){
      
      
      ( (-log(x))^(-xi)-l_r(alpha,xi))*s_b/(l_r(1-beta/2,xi)-l_r(beta/2,xi))+q_a
    }
    return(Finverse(prob,q_a,s_b,xi,alpha,beta))
    
  }
}
