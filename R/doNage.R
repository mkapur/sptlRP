
doNage <- function(X = X_ija, ## input movement matrix
                   indat = dat, ## with bio info'
                   Fv = rep(0,narea),
                   M = 0.15,
                   rdist = recr_dist,
                   refR = rec_level,
                   constSelex = 0.25){
  
  N_ai <- NSurv_ai <- Yield_ai <-  Z_ai <- B_ai <- SB_ai <- matrix(0, nrow = nages, ncol = narea) ## placeholder
  N_sai <- NSurv_sai <- Yield_sai <-  Z_sai <- B_sai <- 
    array(NA, dim = c(2,nages,narea))
  
  
   for(sex in 1:2){
    for(a in 1:nages){
      # if(a == 1) NSurv_ai[a,] <- N_ai[a,] <- 0.5*refR*rdist ## distribute R to areas according to recr_dist
      if(a == 1) NSurv_sai[sex,a,] <- N_sai[sex,a,] <- 0.5*refR*rdist ## distribute R to areas according to recr_dist
      
       for(i in 1:narea){ ## loop areas within ages
        # Z_ai[a,i] <- M + indat[a,5,i]*Fv[i] ## female selex for now (cols 5:6)
        # Z_sai[sex,a,i] <- M + indat[a,sex+4,i]*Fv[i] ## female selex for now (cols 5:6)
        Z_sai[sex,a,i] <- M + constSelex*Fv[i] ## disregard selex
        
        ## Calc Survivors for each area-age
        if(a > 1  & a < max(nages)) {
          # NSurv_ai[a,i] <- N_ai[a-1,i]*exp(-Z_ai[a-1,i])
          NSurv_sai[sex,a,i] <- N_sai[sex,a-1,i]*exp(-Z_sai[sex,a-1,i])
          
        } ## end age < maxage
        if(a == max(nages)){
          # NSurv_ai[a,i] <-  NSurv_ai[a-1,i]*exp(-Z_ai[a-1,i])/(1- exp(-Z_ai[a,i]))
          # NSurv_ai[a,i] <-  N_ai[a-1,i]*exp(-Z_ai[a-1,i])/(1- exp(-Z_ai[a,i]))
          NSurv_sai[sex,a,i] <-  N_sai[sex,a-1,i]*exp(-Z_sai[sex,a-1,i])/(1- exp(-Z_sai[sex,a,i]))
          
          ## Calc SPB for each area-age
          # B_ai[a,i] <-   NSurv_ai[a,i]*indat[a,s+2,i] ## weight in 3 and 4 col
          # SB_ai[a,i]  <- B_ai[a,i]*indat[a,2,i]
          # ## Calc Yield for each area-age   
          # Yield_ai[a,i] <- Fv[i]*indat[a,s+4,i]*B_ai[a,i]
        } ## end plus group
        
        ## Calc SPB for each area-age
        # B_ai[a,i] <-   NSurv_ai[a,i]*indat[a,s+2,i] ## weight in 3 and 4 col
        # SB_ai[a,i]  <- B_ai[a,i]*indat[a,2,i]
        # ## Calc Yield for each area-age   
        # Yield_ai[a,i] <- Fv[i]*indat[a,s+4,i]*B_ai[a,i]
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
        # if(a >1) N_ai[a,i] <- (1-pLeave)* NSurv_ai[a,i] + NCome
        if(a >1) N_sai[sex,a,i] <- (1-pLeave)* NSurv_sai[sex,a,i] + NCome
        
        # B_ai[a,i] <-   N_ai[a,i]*indat[a,sex+2,i] ## weight in 3 and 4 col
        B_sai[sex,a,i] <-   N_sai[sex,a,i]*indat[a,sex+2,i] ## weight in 3 and 4 col
        
        if(sex == 1)    SB_ai[a,i]  <-  B_sai[1,a,i]*indat[a,2,i]
        ## Calc Yield for each area-age   
        # Yield_ai[a,i] <- Fv[i]*indat[a,sex+4,i]*B_ai[a,i]
        # Yield_sai[sex,a,i] <- Fv[i]*indat[a,sex+4,i]*B_sai[sex,a,i] 
        Yield_sai[sex,a,i] <- constSelex*Fv[i]*B_sai[sex,a,i] ## disregard selex
        
        
      } # end subareas i 
    } ## end ages
  } ## end sexes
  
  ## combine sexes
  for(a in 1:nages){
    for(i in 1:narea){
      B_ai[a,i] <- sum(B_sai[,a,i])
      N_ai[a,i]  <- sum(N_sai[,a,i])
      Yield_ai[a,i] <- sum(Yield_sai[,a,i])
    }
  }
  # accumulate total SPB and Yield 
  
  SB_total <- sum(SB_ai)
  Yield_total <- sum(Yield_ai)

  
  
  return(list("N_ai" = N_ai, 
              "Z_ai" = Z_ai, 
              "B_ai" = B_ai,
              "SB_i" = apply(SB_ai,2,sum),
              "Yield_i" =  apply(Yield_ai,2,sum),
              "SB_total"= SB_total, 
              "Yield_total" = Yield_total))
}


## function to return population level F given various Fv, p_i
## Eq 7 in CJFAS pub, but not subtracting M
# langsF <- function(M, N_ai, Fv,p_i){
#   num1 <-  p_i[1]*N_ai[a-1,1]*exp(-Fv[1]-M)
#   num2 <-  p_i[2]*N_ai[a-1,2]*exp(-Fv[2]-M)
#   num3 <-  p_i[3]*N_ai[a-1,3]*exp(-Fv[3]-M)
#   denom <- sum(p_i*N_ai[a-1,])
#   return(-log(sum(num1,num2,num3)/denom))
# }


