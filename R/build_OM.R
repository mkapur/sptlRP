## read data, set up movement matrix etc


## movement happens only below age 6, and is unidirectional from areas 1 and 2 to area 3

## load data and introduce some demographic variation among areas
dat0 <- read.table(here::here("omdata.txt"), header = T)

for(a in 1:nrow(dat0)){
  ## ascending logistic function for selex [a50,a95]
  dat0[a,5:6] <- 1/ ( 1 + exp(-log(19) * (a -  5) / ( 10-5)))
  ## ascending logistic function for fec
  dat0[a,2] <- 1/ ( 1 + exp(-log(19) * (a -  3) / ( 7-3)))
}

dat <- array(NA, dim = c(nrow(dat0),ncol(dat0),narea)) ## placeholder

for(i in 1:narea) dat[,,i] <- as.matrix(dat0)

if(narea == 3){
  X_ija_NULL <- array(0, dim = c(narea,narea,nages))
  X_ija_MIX <- array(0, dim = c(narea,narea,nages))
  X_ija <- array(NA, dim = c(narea,narea,nages))
for(a in 1:2){ ## only two areas have movement

  for(g in 1:dim(X_ija)[3]){ ## loop ages
    diag(X_ija_NULL[,,g]) <- rep(1, length(  diag(X_ija_NULL[,,g])))
    if(g < 10 & a == 1){
      X_ija[a,3,g] <-   X_ija_MIX[a,3,g]  <- 0.05 ## 20% movement from a to a3
      X_ija[a,a,g] <-   X_ija_MIX[a,a,g] <- 0.95 ## retained
      X_ija_MIX[3,1,g] <- 0.3 ## send 20% back from a3
      X_ija_MIX[3,3,g] <- 0.7 
    } else if(g < 6 & a == 2){
      X_ija[a,3,g] <- X_ija_MIX[a,3,g]  <- 0.15
      X_ija[a,a,g] <-  X_ija_MIX[a,a,g] <- 0.85

    } else{
      X_ija[a,,g] <- 0 ## no movement at older ages
      diag(X_ija[,,g]) <- 1 
      diag(X_ija_MIX[,,g]) <- 1 
      # cat( a, " ",diag(X_ija[,,g]) ,"\n")
    } # end else
  } ## end ages
} ## end areas
X_ija[is.na(X_ija)] <- 0
X_ija[3,3,] <- X_ija_NULL[3,3,] <- 1 ## area 3 is self-seeding
## sanity check - all rows should sum to 1
for(i in 1:dim(X_ija)[3]){
  print(rowSums(X_ija[,,a]) == 1)
}
} else if (narea == 2){

  X_ija_EQUAL <- X_ija_NULL2 <- X_ija_MIX2 <-X_ija_MIX2b <- X_ija_UNI2 <- array(NA, dim = c(narea,narea,nages))

  for(a in 1:2){ ## only two areas have movement
    for(g in 1:dim(X_ija_EQUAL)[3]){ ## loop ages
      diag(X_ija_NULL2[,,g]) <- rep(1, length(  diag(X_ija_NULL2[,,g])))
      if(g < 10){
        X_ija_UNI2[1,1:2,g] <- 0.5
        X_ija_UNI2[2,1,g] <- 0
        X_ija_UNI2[2,2,g] <- 1
        
        X_ija_MIX2[2,1,g] <- 0.25 
        X_ija_MIX2[2,2,g] <- 0.75 
        X_ija_MIX2[1,1,g] <- 0.8 
        X_ija_MIX2[1,2,g] <- 0.2
        
        X_ija_MIX2b[2,1,g] <- 0.5 
        X_ija_MIX2b[2,2,g] <- 0.5 
        X_ija_MIX2b[1,1,g] <- 0.8 
        X_ija_MIX2b[1,2,g] <- 0.2
        
        X_ija_EQUAL[1,1,g]  <- X_ija_EQUAL[2,2,g] <- 0.5
        X_ija_EQUAL[1,2,g]  <- X_ija_EQUAL[2,1,g] <- 0.5
      } else{
        X_ija_EQUAL[a,,g] <- X_ija_MIX2[a,,g] <- X_ija_MIX2b[a,,g] <- X_ija_UNI2[a,,g] <- 0 ## no movement at older ages
        diag(X_ija_EQUAL[,,g]) <- 1 
        diag(X_ija_MIX2[,,g]) <- 1 
        diag(X_ija_MIX2b[,,g]) <- 1 
        # cat( a, " ",diag(X_ija[,,g]) ,"\n")
      } # end else
    } ## end ages
  } ## end areas
  X_ija_NULL2[is.na(X_ija_NULL2)] <- 0
} ## end only 2 areas

## return unfished spawning biomass depending on method
getSB0 <- function(eq_method){
  SB0_i <- apply(doNage(eq_method = eq_method, Fv = rep(0,narea))[,10:12],2,sum)
  return(SB0_i)
}
