## read data, set up movement matrix etc


## movement happens only below age 6, and is unidirectional from areas 1 and 2 to area 3

## load data and introduce some demographic variation among areas
dat0 <- read.table(here("HOME3.txt"), header = T)
dat <- array(NA, dim = c(nages,ncol(dat0),narea)) ## placeholder

## first area is original
dat[,,1:3] <- as.matrix(dat0)
## second area, both sexes grow 15% larger, but with same fertility schedule
# dat[,3:4,2] <- dat[,3:4,1]*1.5
## third area, productive at early age
# dat[1:18,2,3] <- dat[4:21,2,1]
# dat[18:20,2,3] <- dat[18,2,3]

X_ija_NULL <- array(0, dim = c(narea,narea,nages))
X_ija_MIX <- array(0, dim = c(narea,narea,nages))
X_ija <- array(NA, dim = c(narea,narea,nages))
for(a in 1:2){ ## only two areas have movement

  for(g in 1:dim(X_ija)[3]){ ## loop ages
    diag(X_ija_NULL[,,g]) <- rep(1, length(  diag(X_ija_NULL[,,g])))
    if(g < 6 & a == 1){
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

## return unfished spawning biomass depending on method
getSB0 <- function(eq_method){
  SB0_i <- apply(doNage(eq_method = eq_method, Fv = rep(0,narea))[,10:12],2,sum)
  return(SB0_i)
}
