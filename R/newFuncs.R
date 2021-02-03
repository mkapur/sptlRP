bh <- function(h, prop, r0, b0, bcurr){
  num <- prop*4*h*r0*bcurr/b0
  denom1 <- bcurr*(5*h-1)
  denom2 <- bcurr/b0*(1-h)
  rec = num/(denom1+denom2)
  return(rec)
}

## movement; not age-specific for now
XIJ <- matrix(NA, nrow = narea, ncol = narea)
XIJ[1,1] <- 0.9 ## from one, to one
XIJ[1,2] <- 1-XIJ[1,1]
XIJ[2,1] <- 0.4 ## from one, to one
XIJ[2,2] <- 1-XIJ[2,1] 

## intermediate func to return SB_F
getNA12 <- function(nage=nage,narea=narea,XIJ=XIJ,mort=mort,FF=FF){
  na1 <- na2 <-  matrix(NA, ncol = nage, nrow = narea) ## monitoring the numbers at age in each area, sourced()
  y1 <- y2 <- matrix(NA, ncol = 1,nrow = narea)
  
  na1[,1] <- c(1,0) ## 1 recruit in a1 from a1, 0 in a1 from a2
  na1[,2] <- na1[1,1]*XIJ[1,] * mort
  
  ## na1 age 3 that stay at age 3 are the age-2s which stay put, die, survive fishing
  ## plus those that left at age 2 to area 2 but since came back, die and get fished
  na1[1,3] <- na1[1,2]*XIJ[1,1] * mort * (1-FF[1])+na1[2,2]*XIJ[2,1] * mort * (1-FF[2]) 
  ## assumes that the incomers are fished at F2 rates (wherever you WERE)
  na1[2,3] <- na1[2,2]*XIJ[2,2] * mort * (1-FF[2])+na1[1,2]*XIJ[1,2] * mort * (1-FF[1]) 
  ## these are spawners from a1 that ended up in a2
  
  na2[,1] <- c(0,1) ## 1 recruit in a2 from a2, 0 in a2 from a1
  na2[,2] <- na2[2,1]*(XIJ[2,])*mort    

  na2[1,3] <- na2[1,2]*XIJ[1,1]* mort * (1-FF[1])+na2[2,2]*XIJ[2,1] * mort * (1-FF[2]) 
  na2[2,3] <- na2[2,2]*XIJ[2,2] * mort * (1-FF[2])+na2[1,2]*XIJ[1,2] * mort * (1-FF[1]) 
  
  ## na1 is realy SBPR for area 1, so SPBR*R  = SB
  # SB_A1 <- na1[1,3]*obsR[1]+na2[1,3]*obsR[2] 
  # SB_A2 <- na1[2,3]*obsR[1]+na2[2,3]*obsR[2] 
  
  ## get yields in area, indexed by spawner source
  y1[1] <-  na1[1,2]*XIJ[1,1] * mort * (FF[1])+na1[2,2]*XIJ[2,1] * mort * (FF[2]) 
  y1[2] <- na1[2,2]*XIJ[2,2] * mort * (FF[2])+na1[1,2]*XIJ[1,2] * mort * (FF[1]) 
  
  y2[1] <- na2[1,2]*XIJ[1,1]* mort * (FF[1])+na2[2,2]*XIJ[2,1] * mort * (FF[2]) 
  y2[2] <- na2[2,2]*XIJ[2,2] * mort * (FF[2])+na2[1,2]*XIJ[1,2] * mort * (FF[1]) 
  
  y_a1 <- y1[1] + y2[1]
  y_a2 <- y1[2] + y2[2]
  return(list( na1,na2,y_a1,y_a2)) ## use these in calculating expR
}

getStuff <-  function(par, ## bar, propr 
                    NA_12, 
                    prop_RA1,
                    R0_input){
  Rbar <- par[1]; Rprop <- par[2] ## don't change with F
  na1 <- NA12[[1]] ## already calculated at F
  na2 <- NA12[[2]]
  obsR <- Rbar*c(Rprop,1-Rprop)
  SB_F <- c(na1[1,3]*obsR[1]+na2[1,3]*obsR[2],
            na1[2,3]*obsR[1]+na2[2,3]*obsR[2]) 
  
  NA12_0 <- getNA12(nage=nage,narea=narea,XIJ=XIJ,mort=mort,FF=c(0,0)) ## returns orange box from excel (deterministic
  SB_0 <- c(NA12_0[[1]][1,3]*obsR[1]+NA12_0[[2]][1,3]*obsR[2],
            NA12_0[[1]][2,3]*obsR[1]+NA12_0[[2]][2,3]*obsR[2])
  
  ## note that what comes out of NA12 is YPR!
  yld <-  c(na1[1,3]*obsR[1]+na2[1,3]*obsR[2],
              na1[2,3]*obsR[1]+na2[2,3]*obsR[2]) 
  # cat(SB_0,"\n")
  ## return expected R given SBF and INPUT values
  ## this only changes if FF or input values change
  expR <- c(bh(h = h, prop = prop_RA1, r0 = R0_input, bcurr = SB_F[1], b0 = SB_0[1]),
            bh(h = h, prop = 1-prop_RA1, r0 = R0_input, bcurr = SB_F[2], b0 = SB_0[1]))
  
  return(list('expR' = expR, 'SB' = SB_F, "obsR" = obsR,"SB_0"=SB_0,
              "yield" = yld))
}

optimFunc <- function( par, ## bar, propr 
                       NA_12, 
                       prop_RA1,
                       R0_input){
  # Rbar <- par[1]; Rprop <- par[2]
  # na1 <- NA12[[1]]
  # na2 <- NA12[[2]]
  # obsR <- Rbar*c(Rprop,1-Rprop)
  # SB_F <- c(na1[1,3]*obsR[1]+na2[1,3]*obsR[2],
  #           na1[2,3]*obsR[1]+na2[2,3]*obsR[2]) 
  # ## return expected R given SBF and INPUT values
  # ## this only changes if FF or input values change
  # expR <- c(bh(h = h, prop = prop_RA1, r0 = R0_input, bcurr = SB_F[1], b0 = 1),
  #           bh(h = h, prop = 1-prop_RA1, r0 = R0_input, bcurr = SB_F[2], b0 = 1))
  
  ## objective is difference between obsR and exp R
  # cat(obsR,"\t",expR,"\n")
  stuff <- getStuff(par, ## bar, propr 
           NA_12, 
           prop_RA1,
           R0_input)
  # cat(obsR,"\n")
  obj <- sum((stuff$obsR - stuff$expR)^2)
  return(obj)
}
