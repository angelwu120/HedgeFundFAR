##############################################
##  Multi-contributions and orphaned FARs   ##
##############################################


I <- 12
J <- 5
G <- I * J

n <- 0.2

r <- array(0.1:0.1, c(I,J))   #IV
r[2,1] <- 1
r[3,1] <- 1

## Parameters for Exercise and Redemption ##

#adjIndex <- 1:4  

redeemS <- array(0:0,c(I,J,2))
redeemIndex <- array(0:0,c(I,J,2))    # redeemIndex[i,j,h]
redeemIndex[2,1,1] <- 1
#vexIndex <- array(0:0,c(I,J,2))      # vexIndex[i,j,h]
mexIndex <- array(0:0, c(I,J,2))      # mexIndex[i,j,h]

#exercise <- array(0:0, c(I,J,2))     # exercise[i,j,h]
redeem <- array(0:0, c(I,J,2))        # redeem[i,j,h]
redeem[2,1,1] <- 90

## Declare Initials -- FLAG >>
j <- 1
K <- array(1:1,c(2))             # K[H]
H <- 0  # contributions
L <- 0  # orphaned FARs

NAV <- array(0:0, c(I,J))        # NAV[i,j]
shares <- array(0:0, c(I,J))     # shares[i,j]

sub <- array(0:0, c(I,J))
sub[1,1] <- 100
sub[2,1] <- 40

VRS <- array(0:0, c(I,J,2))      # VRS[i,j,h]
vrs <- array(0:0,c(I,J,J,2))     # vrs[i,j,k[h],h]
VCS <- array(0:0, c(I,J,2))      # VCS[i,j,h]
S <- array(0:0, c(I,J,2))        # S[i,j,h]
s <- array(0:0, c(I,J,J,2))      # s[i,j,k[h],h]
SS <- array(0:0,c(I,J))          # ss[I,J]

cs <- array(0:0, c(I,J,2))       # cs[i,j,h]
rs <- array(0:0, c(I,J,J,2))     # rs[i,j,k[h],h]
maxNAV <- array(0:0, c(I,J,J,2)) # maxNAV[i,j,k[h],h]

f <- array(0:0, c(I,J,J,2))      # f[i,j,k[h],h]   
SP <- array(0:0, c(I,J,J,2))     # SP[i,j,k[h],h]
ASP <- array(0:0, c(I,J,J,2))

o.f <- array(0:0, c(I,J,G))
o.SP <- array(0:0,c(I,J,G))
o.ASP <- array(0:0, c(I,J,G))
o.s <- array(0:0, c(I,J,G))
o.S <- array(0:0, c(I,J))

growth <- array(0:0, c(J,2))     # growth[j,h]


## START >>

while (j <= J) {
  for (i in 1 : I) {
    if (i == 1) {
      if (j == 1) {   # -----------------------contribution as of [1,1]
        NAV[i,j] <- 1   
        shares[i,j] <- sub[i,j] / NAV[i,j]   
        H <- H + 1
        cs[i,j,H] <- shares[i,j] * (1-n)
        VCS[i,j,H] <- cs[i,j,H] * NAV[i,j]         
        rs[i,j,K[H],H] <- shares[i,j] - cs[i,j,H]
        maxNAV[i,j,K[H],H] <- NAV[i,j]
        vrs[i,j,K[H],H] <- min(NAV[i,j], maxNAV[i,j,K[H],H]) * rs[i,j,K[H],H]
        VRS[i,j,H] <- VRS[i,j,H] + vrs[i,j,K[H],H]         
        f[i,j,K[H],H] <- shares[i,j] * n
        SP[i,j,K[H],H] <- NAV[i,j]
        ASP[i,j,K[H],H] <- SP[i,j,K[H],H] * f[i,j,K[H],H]
        s[i,j,K[H],H] <- max(NAV[i,j]-SP[i,j,K[H],H],0) * f[i,j,K[H],H]
        S[i,j,H] <- S[i,j,H] + s[i,j,K[H],H]
        SS[i,j] <- SS[i,j] + S[i,j,H]          
      } else {   # ------------------------------------[1,j]
        NAV[i,j] <- NAV[12,j-1] * (1 + r[i,j])
        if (L > 0) {  #---------------------------strike prices for orphaned FARs
          for (l in 1: L){
            o.SP[i,j,l] <- o.SP[12,j-1,l] * (1+r[i,j])
            o.f[i,j,l] <- o.f[12,j-1,l]
            o.ASP[i,j,l] <- o.f[i,j,l] * o.SP[i,j,l]
            o.s[i,j,l] <- max(NAV[i,j] - o.SP[i,j,l],0) * o.f[i,j,l]
            o.S[i,j] <- o.S[i,j] + o.s[i,j,l]
          }
        }
        shares[i,j] <- sub[i,j] / NAV[i,j]
        if (shares[i,j] > 0) {    # ---------------- contribution as of [1,j]
          H <- H + 1
          cs[i,j,H] <- shares[i,j] * (1-n)
          VCS[i,j,H] <- cs[i,j,H] * NAV[i,j]
          rs[i,j,1,H] <- shares[i,j] - cs[i,j,H]
          maxNAV[i,j,1,H] <- NAV[i,j]
          vrs[i,j,1,H] <- min(NAV[i,j], maxNAV[i,j,1,H]) * rs[i,j,1,H]
          VRS[i,j,H] <- VRS[i,j,H] + vrs[i,j,1,H]          
          f[i,j,1,H] <- shares[i,j] * n
          SP[i,j,1,H] <- NAV[i,j]
          ASP[i,j,1,H] <- SP[i,j,1,H] * f[i,j,1,H]
          s[i,j,1,H] <- max(NAV[i,j]-SP[i,j,1,H],0) * f[i,j,1,H]
          S[i,j,H] <- S[i,j,H] + s[i,j,1,H]
          SS[i,j] <- SS[i,j] + S[i,j,H]
          for (h in 1 : (H - 1)) {
            cs[i,j,h] <- cs[12,j-1,h]
            VCS[i,j,h] <- cs[i,j,h] * NAV[i,j]           
            for (k in 1: K[h]) {
              rs[i,j,k,h] <- rs[12,j-1,k,h]
              maxNAV[i,j,k,h] <- maxNAV[12,j-1,k,h]
              vrs[i,j,k,h] <- min(NAV[i,j], maxNAV[i,j,k,h]) * rs[i,j,k,h]
              VRS[i,j,h] <- VRS[i,j,h] + vrs[i,j,k,h]             
              f[i,j,k,h] <- f[12,j-1,k,h]
              SP[i,j,k,h] <- SP[12,j-1,k,h]
              ASP[i,j,k,h] <- SP[i,j,k,h] * f[i,j,k,h]
              s[i,j,k,h] <- max(NAV[i,j]-SP[i,j,k,h],0) * f[i,j,k,h]
              S[i,j,h] <- S[i,j,h] + s[i,j,k,h]
            }
            SS[i,j] <- SS[i,j] + S[i,j,h]
          }
        } else {  #-------------------------------no contribution as of [1,j]
          for (h in 1 : H) {
            cs[i,j,h] <- cs[12,j-1,h]
            VCS[i,j,h] <- cs[i,j,h] * NAV[i,j]            
            for (k in 1: K[h]) {
              rs[i,j,k,h] <- rs[12,j-1,k,h]
              maxNAV[i,j,k,h] <- maxNAV[12,j-1,k,h]
              vrs[i,j,k,h] <- min(NAV[i,j], maxNAV[i,j,k,h]) * rs[i,j,k,h]
              VRS[i,j,h] <- VRS[i,j,h] + vrs[i,j,k,h]             
              f[i,j,k,h] <- f[12,j-1,k,h]
              SP[i,j,k,h] <- SP[12,j-1,k,h]
              ASP[i,j,k,h] <- SP[i,j,k,h] * f[i,j,k,h]
              s[i,j,k,h] <- max(NAV[i,j]-SP[i,j,k,h],0) * f[i,j,k,h]
              S[i,j,h] <- S[i,j,h] + s[i,j,k,h]
            }
            SS[i,j] <- SS[i,j] + S[i,j,h]
          }
        } 
      }   
    } else {     #------------------------------- [i>1,j]
      NAV[i,j] <- NAV[i-1,j] * (1 + r[i,j])
      if (L > 0) {
        for (l in 1: L){ # ---------Strike Prices of orphaned FARs 
          o.SP[i,j,l] <- o.SP[i-1,j,l] * (1+r[i,j])
          o.f[i,j,l] <- o.f[i-1,j,l]
          o.ASP[i,j,l] <- o.f[i,j,l] * o.SP[i,j,l]
          o.s[i,j,l] <- max(NAV[i,j] - o.SP[i,j,l],0) * o.f[i,j,l]
          o.S[i,j] <- o.S[i,j] + o.s[i,j,l]
        }
      }
      shares[i,j] <- sub[i,j] / NAV[i,j]
      if (shares[i,j] > 0) {  #----------------contribution as of [i>1,j]
        H <- H + 1
        cs[i,j,H] <- shares[i,j] * (1-n)
        VCS[i,j,H] <- cs[i,j,H] * NAV[i,j]        
        rs[i,j,1,H] <- shares[i,j] - cs[i,j,H]
        maxNAV[i,j,1,H] <- NAV[i,j]
        vrs[i,j,1,H] <- min(NAV[i,j], maxNAV[i,j,1,H]) * rs[i,j,1,H]
        VRS[i,j,H] <- VRS[i,j,H] + vrs[i,j,1,H]        
        f[i,j,1,H] <- shares[i,j] * n
        SP[i,j,1,H] <- NAV[i,j]
        ASP[i,j,1,H] <- SP[i,j,1,H] * f[i,j,1,H]
        s[i,j,1,H] <- max(NAV[i,j]-SP[i,j,1,H],0) * f[i,j,1,H]
        S[i,j,H] <- S[i,j,H] + s[i,j,1,H]
        SS[i,j] <- SS[i,j] + S[i,j,H]        
        for (h in 1: (H-1)) {
          cs[i,j,h] <- cs[i-1,j,h]
          VCS[i,j,h] <- cs[i,j,h] * NAV[i,j]          
          for (k in 1: K[h]) {
            rs[i,j,k,h] <- rs[i-1,j,k,h]
            maxNAV[i,j,k,h] <- maxNAV[i-1,j,k,h]
            vrs[i,j,k,h] <- min(NAV[i,j], maxNAV[i,j,k,h]) * rs[i,j,k,h]
            VRS[i,j,h] <- VRS[i,j,h] + vrs[i,j,k,h]          
            f[i,j,k,h] <- f[i-1,j,k,h]
            SP[i,j,k,h] <- SP[i-1,j,k,h]
            ASP[i,j,k,h] <- SP[i,j,k,h] * f[i,j,k,h]
            s[i,j,k,h] <- max(NAV[i,j] - SP[i,j,k,h],0) * f[i,j,k,h]
            S[i,j,h] <- S[i,j,h] + s[i,j,k,h]
          }
          SS[i,j] <- SS[i,j] + S[i,j,h]
        }
      } else {
        for (h in 1: H) {
          cs[i,j,h] <- cs[i-1,j,h]
          VCS[i,j,h] <- cs[i,j,h] * NAV[i,j]         
          for (k in 1: K[h]) {
            rs[i,j,k,h] <- rs[i-1,j,k,h]
            maxNAV[i,j,k,h] <- maxNAV[i-1,j,k,h]
            vrs[i,j,k,h] <- min(NAV[i,j], maxNAV[i,j,k,h]) * rs[i,j,k,h]
            VRS[i,j,h] <- VRS[i,j,h] + vrs[i,j,k,h]           
            f[i,j,k,h] <- f[i-1,j,k,h]
            SP[i,j,k,h] <- SP[i-1,j,k,h]
            ASP[i,j,k,h] <- SP[i,j,k,h] * f[i,j,k,h]
            s[i,j,k,h] <- max(NAV[i,j]-SP[i,j,k,h],0) * f[i,j,k,h]
            S[i,j,h] <- S[i,j,h] + s[i,j,k,h]
          }
          SS[i,j] <- SS[i,j] + S[i,j,h]
        }
      }
    }
    #----------------------------------------------------- Redemption >>
    for (h in 1 : H) {
      if (redeemIndex[i,j,h] == 1) {
        redeemS [i,j,h] <- redeem[i,j,h] / (VCS[i,j,h] + VRS[i,j,h]) 
        cs[i,j,h] <- cs[i,j,h] - redeemS [i,j,h] * cs[i,j,h]
        VCS[i,j,h] <- cs[i,j,h] * NAV[i,j]
        for (k in 1 : K[h]) {
          if (mexIndex[i,j,h]== 0) { #--------------------------------Generate orphaned FARs >>
            L <- L + 1
            o.f[i,j,L] <- redeemS [i,j,h] * rs[i,j,k,h]
            o.SP[i,j,L] <- SP[i,j,k[h],h]
            o.ASP[i,j,L] <- o.SP[i,j,L] * o.f[i,j,L]
            o.s[i,j,L] <- max(NAV[i,j] - o.SP[i,j,L], 0) * o.f[i,j,L]
            o.S[i,j] <- o.S[i,j] + o.s[i,j,L]
          }
          VRS[i,j,h] <- VRS[i,j,h] - redeemS [i,j,h] * rs[i,j,k,h] * min(NAV[i,j],maxNAV[i,j,k,h])
          S[i,j,h] <- S[i,j,h] - max(NAV[i,j] - SP[i,j,k,h], 0) * redeemS [i,j,h] * rs[i,j,k,h]
          SS[i,j] <- max(NAV[i,j] - SP[i,j,k,h], 0) * redeemS [i,j,h] * rs[i,j,k,h]
          f[i,j,k,h] <- f[i,j,k,h] - redeemS [i,j,h] * rs[i,j,k,h]
          s[i,j,k,h] <- max(NAV[i,j] - SP[i,j,k,h], 0) * f[i,j,k,h]
          rs[i,j,k,h] <- rs[i,j,k,h] - redeemS [i,j,h] * rs[i,j,k,h]
          vrs[i,j,k,h] <- min(NAV[i,j],maxNAV[i,j,k,h]) * rs[i,j,k,h]
        }
      } else {
        if (redeemIndex[i,j,h] == 2) {
          cs[i,j,h] <- cs[i,j,h] - redeem[i,j,h] / VCS[i,j,h] * cs[i,j,h]
          VCS[i,j,h] <- cs[i,j,h] * NAV[i,j]
        } else {
          if (redeemIndex[i,j,h] == 3) {
            redeemS[i,j,h] <- redeem[i,j,h] / VRS[i,j,h] 
            for (k in 1: K[h]) {
              if (mexIndex[i,j,h]== 0) {  #--------------------------------Generate orphaned FARs >>
                L <- L + 1
                o.f[i,j,L] <- redeemS[i,j,h] * rs[i,j,k,h]
                o.SP[i,j,L] <- SP[i,j,k[h],h]
                o.ASP[i,j,L] <- o.SP[i,j,L] * o.f[i,j,L]
                o.s[i,j,L] <- max(NAV[i,j] - o.SP[i,j,L], 0) * o.f[i,j,L]
                o.S[i,j] <- o.S[i,j] + o.s[i,j,L]
              }
              VRS[i,j,h] <- VRS[i,j,h] - redeemS [i,j,h] * rs[i,j,k,h] * min(NAV[i,j],maxNAV[i,j,k,h])
              S[i,j,h] <- S[i,j,h] - max(NAV[i,j] - SP[i,j,k,h], 0) * redeemS [i,j,h] * rs[i,j,k,h]
              SS[i,j] <- max(NAV[i,j] - SP[i,j,k,h], 0) * redeemS [i,j,h] * rs[i,j,k,h]
              f[i,j,k,h] <- f[i,j,k,h] - redeemS [i,j,h] * rs[i,j,k,h]
              s[i,j,k,h] <- max(NAV[i,j] - SP[i,j,k,h], 0) * f[i,j,k,h]
              rs[i,j,k,h] <- rs[i,j,k,h] - redeemS [i,j,h] * rs[i,j,k,h]
              vrs[i,j,k,h] <- min(NAV[i,j],maxNAV[i,j,k,h]) * rs[i,j,k,h]
            }
          }
        }
      }
    }
    SS [i,j] <- SS[i,j] + o.S[i,j]
    
    #----------------------------------------------------- Growth Grants >>
    if (j > 1 & i == 1) {
      for (h in 1 : H) {
        growth[j,h] <- VRS[i,j,h] + VCS[i,j,h] - max(pmax(VRS[1,,h]+VCS[1,,h])[1:j-1])
        if (growth[j,h] > 0 ) {
          K[h] <- K[h] + 1
          cs[i,j,h] <- cs[i,j,h] - growth[i,h] * n / NAV[i,j]
          VCS[i,j,h] <- cs[i,j,h] * NAV[i,j]
          
          rs[i,j,K[h],h] <- growth[j,h] * n / NAV[i,j]
          maxNAV[i,j,K[h],h] <- growth[i,h] * n / NAV[i,j]
          vrs[i,j,K[h],h] <- min(NAV[i,j],maxNAV[i,j,K[h],h]*rs[i,j,K[h],h])
          VRS[i,j,h] <- VRS[i,j,h] + vrs[i,j,K[h],h]
          
          f[i,j,K[h],h] <- growth[j,h] * n / NAV[i,j]
          SP[i,j,K[h],h] <- NAV[i,j]
          ASP[i,j,K[h],h] <- SP[i,j,K[h],h] * SP[i,j,K[h],h]
          s[i,j,K[h],h] <- max(NAV[i,j] - SP[i,j,K[h],h], 0) * f[i,j,K[h],h]
          S[i,j,h] <- S[i,j,h] + s[i,j,K[h],h]
          SS[i,j] <- SS[i,j] + s[i,j,K[h],h]
          
        }
      }
    }
  }
  j <- j + 1
}



