################################
## Black-Scholes Option Value ##
################################


# S: Spot price of the underlying assets  --IV
# X: Strike Price  --IV
# T: Time to maturity    --IV
# rf: risk free rate
# sigma: Volatility of returns of the underlying assets

blackscholes <- function(S, X, rf, T, sigma) {
  values <- c(2)
  
  d1 <- (log(S/X)+(rf+sigma^2/2)*T)/sigma*sqrt(T)
  d2 <- d1 - sigma * sqrt(T)
  
  values[1] <- S*pnorm(d1) - X*exp(-rf*T)*pnorm(d2)    # Call value 
  values[2] <- X*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1)   #put value
  
  values[1]
}


## Values of FARs
V <- array(0:0,c(I,J,J,2))

for (h in 1: H) {
  for (j in 1: J) {
    for (i in 1: I) {
      for (k in 1: K[h]) {
        V[i,j,k,h] <- blackscholes(NAV[i,j], SP[i,j,k,h], r[i,j], t[i,j], 0.1)
      }
    }
  }
}


## Values of Orphaned FARs
o.V <- array(0:0,c(I,J,L))

  for (j in 1: J) {
    for (i in 1: I) {
      for (l in L) {
        o.Value[i,j,l] <- blackscholes(NAV[i,j], o.SP[i,j,l], r[i,j], t[i,j], 0.1)
      }
    }
  }
