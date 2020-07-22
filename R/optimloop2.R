optim_loop2 <- function(Fv_i,
                       rec_level_idx = 1,
                       movemat = X_ija,
                       globalR0 = 1000,
                       recr_dist = c(1, 1, 1)
                       ) {
  
  
  radj <- matrix(NA, nrow = maxiter, ncol = narea)
  
  
  ## ORIGINAL APPROACH - USER PROVIDED R0_I
  R0 <- R0_list[[rec_level_idx]]
  rec_level <- R0
  
  ## NEW APPROACH - MOVEMAT DESIGNED RDIST
  # X_12 <- movemat[,,1][upper.tri(movemat[,,1])]
  # X_21 <- movemat[,,1][lower.tri(movemat[,,1])]
  # rec_level <-  c(globalR0/(1+X_12/X_21),globalR0-globalR0/(1+X_12/X_21))
  # rec_level <-  c(sum(R0)/(1+X_12/X_21),sum(R0)-sum(R0)/(1+X_12/X_21))
  # R0 <- rec_level ## unchanging
  
  SB0_i <- doNage(Fv = rep(0,narea), 
                  X = movemat,
                  rdist = recr_dist,
                  refR = rec_level)$SB_i
  
  last_req <- yield_FI <- B_FI <- SBPR_i <- YPR_i <- NULL
  for(k in 1:maxiter){ ## Loop over steps A & B
    
    if(k == 1){
      rdistUse <- recr_dist ## no distribution now; full rec-level in each area
      rlevelUse = rec_level ## pre-specified No recruits in area, currently R0
    } else{
      rdistUse <- recr_dist ## only after computing R_i
      rlevelUse = last_req# round(last_req,2)# last_req  # c(R_eq_i[v,1:2], max(1,round(R_eq_i[v,3],0)))
    }
    
    prop <- doNage( Fv = Fv_i, 
                    X = movemat,
                    rdist = rdistUse,
                    refR = rlevelUse) 
    
    
    # call Equ_Spawn_Recr_Fxn for each area to get B_equil and R_equil from SPB/R and SR parms
    for(i in 1:narea){ ## will overwrite second time
      # calc area-specific SPB/R and Yield/R, using area-specific R
      
      if( k > 1){
        rleveltmp = rlevelUse[i] #min(, R0[i]) if unconstrained will be ~10 over R0
      } else{
        rleveltmp = rlevelUse[i]
      }
      # if(round(rleveltmp) == 0) next() ## end iteration if rec is now zero
      # cat(v, k,i,rleveltmp,"\n")
      # radj[k,v,i] <- rleveltmp
      radj[k,i] <- rleveltmp ## store this
      
      
      # rRef_proposed_radj[k,v,i,RR] <- rleveltmp
      # SBPR_i[i] <-  prop$SB_i[i]/(rleveltmp*rdistUse[i]) ## Rick's idea
      SBPR_i <-  prop$SB_i[i]/(rleveltmp+0.005*R0[i]) ## Rick's idea
      
      YPR_i[i] <- prop$Yield_i[i]/(rleveltmp*rdistUse[i])
      # YPR_i <- prop$Yield_i[i]/((rleveltmp+0.05*R0[i])*rdistUse[i])
      
      
      
      ## Calc area-specific recruits using area-specific SB etc
      propEq <- Equil_Spawn_Recr_Fxn(steepness = steep[i], SSB_virgin = SB0_i[i],
                                     Recr_virgin = R0[i], SPR_temp = SBPR_i)
      
      # B_eq_i[v,i] <- propEq$B_equil
      last_req[i] <- propEq$R_equil ## gets overwritten each iteration
    } ## end areas
    # if(k == maxiter){ ## store quantities
    if(k > 5){
      ## stop iters if req hasn't changed more than 1%
      if(any(round(abs(radj[k,]/radj[k-5,] - 1),2)  > 0.05) & k < maxiter){
        next()
     }else if(any(round(abs(radj[k,]/radj[k-5,] - 1),2)  <=  0.05) | k == maxiter){
       for (i in 1:narea) {
         yield_FI[i] <-  YPR_i[i] *  last_req[i]
         B_FI[i] <-    SBPR_i[i] *  last_req[i]
         cat(k, i,  last_req[i], yield_FI[i], "\n")
       }
        break("maxiter reached ",i,k)
      } ## end if neither 1%
    } ## end K check
    # } ## end areas
    
    # cat(k,i,last_req,yield_FI,"\n")
    
   
    # cat(last_req,yield_FI,"\n")

  } ## end k:maxiter

  
  ## save totals from final iteration
  return(list(Yield = sum(yield_FI), Biomass = sum(B_FI), 
              Yield_i = yield_FI, Biomass_i = B_FI, radj = radj))
}
