
getAB <- function(SRR = 1, h = steep, R0 = 1, i){
  sbpr0 <- alpha <- beta <- NULL
  
  ## always calculate at F = 0
  sbpr0 <-   sum(doNage(Fv = rep(0,narea))[,9+i]) #sum(dat[,2,i] * doNage(Fv = rep(0, narea))) 
  alpha <- sbpr0*((1-h)/(4*h))
  beta <- (5*h-1)/(4*h*R0) 
  
  return(c("alpha" = alpha, "beta" = beta))
}



doSRR <- function(SB_aiy, SB0i = SB0_i, h = steep, R0=1){
  R_yi <- 4*h*R0*SB_aiy/(SB0i*(1-h)+ SB_aiy*(5*h-1))
  return(R_yi)
}
getEqRec <- function(SRR = 1, h = steep, 
                     Fv = rep(0,narea), 
                     gam = 1, R0 = 1, S0 = 0.6739975){
  sumSBPR <- R <- NULL
  
  for(i in 1:narea){
    
    ## get s-tilde
    sumSBPR[i] <- sum(doNage(Fv = Fv)[,9+i]) ## SB in cols 10:12
    # sumSBPR <- sum(sbpr)
    # if(SRR == 1){ ## bevholt
    
    ab <- getAB(SRR = 1, h = steep, i=i) ## specific alpha, beta for area
    R[i] <- (  sumSBPR[i] - ab[1] )/(ab[2] *   sumSBPR[i]) ## Equation 9. eq recruits
    rm(ab)
    # } else if(SRR == 2){ ## ricker
    #   ab <- getAB(SRR = 2, h = steep)
    #   R <- log(ab[1]*sumSBPR)/(ab[2]*sumSBPR) ## Question 1A
    # } else if(SRR == 3){ ## Pella
    #   ab <- getAB(SRR = 3, h = steep)
    #   R <- (S0/sumSBPR) * (1 - (1 - ab[1] * sumSBPR)/(ab[2] * ab[1] * sumSBPR))^(1/gam) ## Question 1B
  }
  return(list('rec' = as.numeric(R),'spawnbio' = sumSBPR))
}
