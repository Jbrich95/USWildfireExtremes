
l = function(a, xi) {
  (-log(a)) ^ (-xi)
}
l0 = function(a) {
  log(-log(a))
}


Fthreshinv = function(u, mu, sig, xi) {
  a = -Tmax
  b = Tmax
  for (j in 1:50) {
    if (1 + xi * ((a + b) / 2 - mu) / sig <= 0) {
      bsup = b
      binf = (a + b) / 2
    } else{
      if (dpois(0, Lambda((a + b) / 2, mu, sig, xi)) <= u) {
        binf = (a + b) / 2
        bsup = b
      }
      if (dpois(0, Lambda((a + b) / 2, mu, sig, xi)) >= u) {
        bsup = (a + b) / 2
        binf = a
      }
    }
    a = binf
    b = bsup
  }
  return((a + b) / 2)
}
Ft = function(x, mu, sig, xi, thresh)
  1 - exp(Lambda(x + thresh, mu, sig, xi) - Lambda(thresh, mu, sig, xi)) +
  dpois(0, Lambda(thresh, mu, sig, xi))
Ftinv = function(u, mu, sig, xi, thresh) {
  a = 0
  b = Tmax
  for (j in 1:30) {
    if (Ft((a + b) / 2, mu, sig, xi, thresh) <= u) {
      binf = (a + b) / 2
      bsup = b
    }
    if (Ft((a + b) / 2, mu, sig, xi, thresh) >= u) {
      bsup = (a + b) / 2
      binf = a
    }
    
    a = binf
    b = bsup
  }
  return((a + b) / 2)
}
Lambda = function(x, mu, sig, xi) {
  1 / 12 * (1 + xi * (x - mu) / sig) ^ (-1 / xi)
}