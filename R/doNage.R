
doNage <- function(X = X_ija, ## input movement matrix
                   indat = dat, ## with bio info
                   Fv = rep(0,narea),
                   M = c(rep(0.2,10), rep(0.15,11)), ## time varying M
                   refR = rec_level, ## all areas
                   constSelex = 0.25){
  
  N_ai <- NSurv_ai <- Yield_ai <-  Z_ai <- B_ai <- SB_ai <- matrix(0, nrow = nages, ncol = narea) ## placeholder
  N_sai <- NSurv_sai <- Yield_sai <-  Z_sai <- B_sai <- 
    array(NA, dim = c(2,nages,narea))
  
  
   for(sex in 1:2){
    for(a in 1:nages){
      if(a == 1) NSurv_sai[sex,a,] <- N_sai[sex,a,] <- 0.5*refR ## distribute R to areas according to recr_dist
       for(i in 1:narea){ ## loop areas within ages
        # Z_sai[sex,a,i] <- M + constSelex*Fv[i] ## disregard selex
        Z_sai[sex,a,i] <- M[a] + dat[a,4+sex,i]*Fv[i] ## include selex
        
        ## Calc Survivors for each area-age
        if(a > 1  & a < max(nages)) {
          NSurv_sai[sex,a,i] <- N_sai[sex,a-1,i]*exp(-Z_sai[sex,a-1,i])
          
        } ## end age < maxage
        if(a == max(nages)){
          NSurv_sai[sex,a,i] <-  N_sai[sex,a-1,i]*exp(-Z_sai[sex,a-1,i])/ifelse(Z_sai[sex,a,i] !=0,
                                                                                1- exp(-Z_sai[sex,a,i]),1)
        } ## end plus group
      } ## end survivors-in-area
      
      for(i in 1:narea){ ## loop areas within ages
        ## for each age mix survivors among areas according to movement specs
        pLeave = NCome = 0
        for(j in 1:narea){
          if(i != j){
            pLeave = pLeave + X[i,j,a]
            # NCome = NCome + X[j,i,a]*NSurv_ai[a,j]
            NCome = NCome + X[j,i,a]*NSurv_sai[sex,a,j]
          } # end i != j
        } # end subareas j
        if(a >1) N_sai[sex,a,i] <- (1-pLeave)* NSurv_sai[sex,a,i] + NCome
        B_sai[sex,a,i] <-   N_sai[sex,a,i]*indat[a,sex+2,i] ## weight in 3 and 4 col
        
        if(sex == 1)    SB_ai[a,i]  <-  B_sai[1,a,i]*indat[a,2,i]
        ## Calc Yield for each area-age   
        Yield_sai[sex,a,i] <- dat[a,4+sex,i]*Fv[i]*B_sai[sex,a,i] ## disregard selex
      } # end subareas i 
    } ## end ages
  } ## end sexes
  
  ## combine sexes
  for(a in 1:nages){
    for(i in 1:narea){
      B_ai[a,i] <- sum(B_sai[,a,i])
      N_ai[a,i]  <- sum(N_sai[,a,i])
      Z_ai[a,i]  <- sum(Z_sai[,a,i])
      Yield_ai[a,i] <- sum(Yield_sai[,a,i])
    }
  }
  # accumulate total SPB and Yield 
  
  SB_total <- sum(SB_ai)
  Yield_total <- sum(Yield_ai)
  return(list("N_ai" = N_ai, 
              "Z_ai" = Z_ai, 
              "B_ai" = B_ai,
              "SB_ai" = SB_ai,
              "SB_i" = apply(SB_ai,2,sum),
              "Yield_i" =  apply(Yield_ai,2,sum),
              "SB_total"= SB_total, 
              "Yield_total" = Yield_total))
}




