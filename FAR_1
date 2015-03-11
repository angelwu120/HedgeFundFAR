########################################
#### ONE INVESTOR, ONE CONTRIBUTION ####
########################################

## Inputs ##
I <- 12
J <- 5   #IV
G <- I * (J-1)

n <- 0.2

sub <- 10000   #IV
NAV <- array(0:0,c(I,J))    #IV

r <- array(0.1:0.1, c(I,J))   #IV


## Parameters for Exercise and Redemption ##

#adjIndex <- 1:4   
redeemIndex <- array(0:0,c(I,J))   #IV           
         #redeemIndex[i,j] = 0;    --no redemption
         #redeemIndex[i,j] = 1;    --redeem from both common shares and restriced shares
         #redeemIndex[i,j] = 2;    --redeem from common shares
         #redeemIndex[i,j] = 3;    --redeem from restricted shares
#redeemIndex[1,2] <- 1

vexIndex <- array(0:0,c(I,J))     #IV
#vexIndex[4,1] <- 1

mexIndex <- array(0:0, c(I,J))     #IV

exercise <- array(0:0, c(I,J))    #IV
#exercise[4,1] <- 200

redeem <- array(0:0, c(I,J))    #IV
#redeem[1,2] <- 500

## Declare initials -- FLAG >>

j <- 1
K <- 1

VRS <- array(0:0, c(I,J))
vrs <- array(0:0,c(I,J,G))
VCS <- array(0:0, c(I,J))
S <- array(0:0, c(I,J))
s <- array(0:0, c(I,J,G))

cs <- array(0:0, c(I,J))
rs <- array(0:0, c(I,J,G))
maxNAV <- array(0:0, c(I,J,G))

f <- array(0:0, c(I,J,G))
SP <- array(0:0, c(I,J,G))

reduction <- array(0:0, c(I,J))
growth <- array(0:0, c(J))


## START >> 

while (j <= J) {

  for (i in 1 : I) {
    if (i == 1) { 
      if (j == 1) {   # <-------------------------------------------------------- contribution >>
        NAV[i,j] <- 100
        shares <- sub/NAV[1,1]
        cs[i,j] <- shares * (1-n)
        VCS[i,j] <- cs[i,j] * NAV[i,j]
        for (k in 1 : K) {
          rs[i,j,k] <- shares - cs [1,1]
          maxNAV [i,j,k] <- NAV[i,j]
          vrs [i,j,k] <- min(NAV[i,j],maxNAV[i,j,k]) * rs[i,j,k]
          VRS[i,j] <- VRS[i,j] + vrs[i,j,k]
          f[i,j,k] <- shares * n
          SP[i,j,k] <- NAV[i,j]
          s[i,j,k] <- max(NAV[i,j] - SP[1,j,k], 0) * f[i,j,k]
          S[i,j] <- S[i,j] + s[i,j,k]
        }
      } else {   # <---------------------------------------------------------------- Valuation >> 
          NAV[i,j] <- NAV[12,j-1] * (1 + r[i,j])
          cs[i,j] <- cs[12,j-1]
          VCS[i,j] <- cs[i,j] * NAV[i,j]
          for (k in 1 : K) {
            rs[i,j,k] <- rs[12,j-1,k]
            maxNAV [i,j,k] <- maxNAV[12,j-1,k]
            vrs [i,j,k] <- min(NAV[i,j],maxNAV[i,j,k]) * rs[i,j,k]
            VRS[i,j] <- VRS[i,j] + vrs[i,j,k]
            
            f[i,j,k] <- f[12,j-1,k]
            SP[i,j,k] <- SP[12,j-1,k]
            s[i,j,k] <- max(NAV[i,j] - SP[i,j,k], 0) * f[i,j,k]
            S[i,j] <- S[i,j] + s[i,j,k]
          }
        }
    } else {
        NAV[i,j] <- NAV[i-1,j] * (1 + r[i,j])
        cs[i,j] <- cs[i-1,j]
        VCS[i,j] <- cs[i,j] * NAV[i,j]
        for (k in 1 : K) {
          rs[i,j,k] <- rs[i-1,j,k]
          maxNAV [i,j,k] <- maxNAV[i-1,j,k]
          vrs [i,j,k] <- min(NAV[i,j],maxNAV[i,j,k]) * rs[i,j,k]
          VRS[i,j] <- VRS[i,j] + vrs[i,j,k]
          
          f[i,j,k] <- f[i-1,j,k]
          SP[i,j,k] <- SP[i-1,j,k]
          s[i,j,k] <- max(NAV[i,j] - SP[i,j,k], 0) * f[i,j,k]
          S[i,j] <- S[i,j] + s[i,j,k]
        }
    }
    #<------------------------------------------------------------------------Voluntary Exercise >>
    
    if (vexIndex[i,j] == 1) {   
      K <- K + 1
      for (k in 1 : (K-1)) {
        f[i,j,k] <- f[i,j,k] - exercise[i,j]/ S[i,j] * f[i,j,k]
        s[i,j,k] <- max(NAV[i,j] - SP[i,j,k], 0) * f[i,j,k]
        S[i,j] <- S[i,j] - exercise[i,j]
        
        rs[i,j,k] <- rs[i,j,k] - exercise[i,j]/ S[i,j] * f[i,j,k]
        vrs[i,j,k] <- min(NAV[i,j],maxNAV[i,j,k]) * rs[i,j,k]
        
        reduction[i,j] <- reduction[i,j] + exercise[i,j]/ S[i,j] * f[i,j,k] * SP[i,j,k]
      }
      SP[i,j,K] <- NAV[i,j]
      f[i,j,K] <- reduction[i,j] / NAV[i,j]
      s[i,j,K] <- max(NAV[i,j] - SP[i,j,K], 0) * f[i,j,K]
      S[i,j] <- S[i,j] + s[i,j,K]
      
      maxNAV[i,j,K] <- NAV[i,j]
      rs[i,j,K] <- reduction [i,j] / NAV[i,j]
      vrs[i,j,K] <- min(NAV[i,j],maxNAV[i,j,K]) * rs[i,j,K]
      
      cs[i,j] <- cs[i,j] + exercise[i,j] / NAV[i,j]
      VCS[i,j] <- cs[i,j] * NAV[i,j] 
    }
    #<----------------------------------------------------------------------------------Redemption >>
    
    if (redeemIndex[i,j] == 1) {
      cs[i,j] <- cs[i,j] - redeem[i,j] / (VCS[i,j] + VRS[i,j]) * cs[i,j]
      VCS[i,j] <- cs[i,j] * NAV[i,j]
      for (k in 1:K) {
        rs[i,j,k] <- rs[i,j,k] - redeem[i,j] / (VCS[i,j] + VRS[i,j]) * rs[i,j,k]
        vrs[i,j,k] <- min(NAV[i,j],maxNAV[i,j,k]) * rs[i,j,k]
        VRS[i,j] <- VRS[i,j] - redeem[i,j] / (VCS[i,j] + VRS[i,j]) * rs[i,j,k] * min(NAV[i,j],maxNAV[i,j,k])
        if (mexIndex[i,j] == 1) {
          f[i,j,k] <- f[i,j,k] - redeem[i,j] / (VCS[i,j] + VRS[i,j]) * rs[i,j,k]
          s[i,j,k] <- max(NAV[i,j] - SP[i,j,k], 0) * f[i,j,k]
          S[i,j] <- S[i,j] - max(NAV[i,j] - SP[i,j,k], 0) * redeem[i,j] / (VCS[i,j] + VRS[i,j]) * rs[i,j,k]
        }
      }
    } else {
      if (redeemIndex[i,j] == 2) {
        cs[i,j] <- cs[i,j] - redeem[i,j] / VCS[i,j] * cs[i,j]
        VCS[i,j] <- cs[i,j] * NAV[i,j]
      } else {
        if (redeemIndex[i,j] == 3) {
          for (k in 1: K) {
            rs[i,j,k] <- rs[i,j,k] - redeem[i,j] / VRS[i,j] * rs[i,j,k]
            vrs[i,j,k] <- min(NAV[i,j],maxNAV[i,j,k]) * rs[i,j,k]
            VRS[i,j] <- VRS[i,j] - redeem[i,j] / VRS[i,j] * rs[i,j,k] * min(NAV[i,j],maxNAV[i,j,k])
            if (mexIndex == 1) {
              f[i,j,k] <- f[i,j,k] - redeem[i,j] / (VCS[i,j] + VRS[i,j]) * rs[i,j,k]
              s[i,j,k] <- max(NAV[i,j] - SP[i,j,k], 0) * f[i,j,k]
              S[i,j] <- S[i,j] - max(NAV[i,j] - SP[i,j,k], 0) * redeem[i,j] / (VCS[i,j] + VRS[i,j]) * rs[i,j,k]
            }
          }
        }
      }
    }
    #<--------------------------------------------------------------------------------------Growth Grants >>
    
    if (j > 1 & i == 1 ) {
      growth[j] <- VRS[i,j] + VCS[i,j] - max(pmax(VRS[1,]+VCS[1,])[1:j-1])
      if (growth[j] > 0 & vexIndex[i,j] == 0) {
        K <- K + 1
        cs[i,j] <- cs[i,j] - growth[j] * n / NAV[i,j]
        VCS[i,j] <- cs[i,j] * NAV[i,j]
        
        rs[i,j,K] <- growth[j] * n / NAV[i,j]
        maxNAV[i,j,K] <- NAV[i,j]
        vrs[i,j,K] <- min(NAV[i,j],maxNAV[i,j,K]) * rs[i,j,K]
        VRS[i,j] <- VRS[i,j] + vrs[i,j,K]
        
        f[i,j,K] <- growth[j] * n / NAV[i,j]
        SP[i,j,K] <- NAV[i,j]
        s[i,j,K] <- max(NAV[i,j] - SP[i,j,K], 0) * f[i,j,K]
        S[i,j] <- S[i,j] + s[i,j,K]
      } else {
        if (growth[j] > 0 & vexIndex [i,j] == 1)
        cs[i,j] <- cs[i,j] - growth[j] * n / NAV[i,j]
        VCS[i,j] <- cs[i,j] * NAV[i,j]
        
        rs[i,j,K] <- rs[i,j,K] + growth[j] * n / NAV[i,j]
        vrs[i,j,K] <- min(NAV[i,j],maxNAV[i,j,K]) * rs[i,j,K]
        VRS[i,j] <- VRS[i,j] + growth[j] * n / NAV[i,j] * min(NAV[i,j],maxNAV[i,j,K])
        
        f[i,j,K] <- growth[j] * n / NAV[i,j]
        s[i,j,K] <- s[i,j,K] + max(NAV[i,j] - SP[i,j,K], 0) * f[i,j,K]
        S[i,j] <- S[i,j] + growth[j] * n / NAV[i,j] * max(NAV[i,j] - SP[i,j,K], 0)
      }
    }
    
  }
  j <- j + 1
}

