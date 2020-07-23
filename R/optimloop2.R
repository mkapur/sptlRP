getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

optim_loop2 <- function(Fv_i,
                       rec_level_idx = 1,
                       movemat = X_ija,
                       recr_dist = c(1, 1, 1)
                       ) {
  
  # rec_level <- R0 <- R0_list[[RR]]
  # proposed <- data.frame(Fv = NA, Yield = NA, B = NA)
  # proposed_i <- array(NA, dim = c(length(Ftest),3,narea), dimnames = list(NULL,c('Fv','Yield',"B"))) ## now for each area  ## define virgin biomass by AREA, does not change
  # B_eq_i <- R_eq_i <- B_eq_i_INIT <- R_eq_i_INIT <- SB_Ri <- Yield_Ri <- matrix(NA, nrow =length(Ftest), ncol = narea)
  # radj <- array(NA, dim = c(maxiter,length(Ftest),narea)) ## keeping track of convergence
  
  radj <- matrix(NA, nrow = maxiter, ncol = narea)
  
  
  ## ORIGINAL APPROACH - USER PROVIDED R0_I
  R0 <- R0_list[[rec_level_idx]]
  rec_level <- R0
  
  ## NEW APPROACH - MOVEMAT DESIGNED RDIST
  # X_12 <- movemat[,,1][upper.tri(movemat[,,1])]
  # X_21 <- movemat[,,1][lower.tri(movemat[,,1])]
  # rec_level <-  c(1000/(1+X_12/X_21),globalR0-1000/(1+X_12/X_21))
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
      SBPR_i[i] <-  prop$SB_i[i]/(rleveltmp+0.005*R0[i]) ## Rick's idea
      
      YPR_i[i] <- prop$Yield_i[i]/(rleveltmp)
      # YPR_i <- prop$Yield_i[i]/((rleveltmp+0.05*R0[i])*rdistUse[i])
      
      
      
      ## Calc area-specific recruits using area-specific SB etc
      propEq <- Equil_Spawn_Recr_Fxn(steepness = steep[i], SSB_virgin = SB0_i[i],
                                     Recr_virgin = R0[i], SPR_temp = SBPR_i[i])
      
      # B_eq_i[v,i] <- propEq$B_equil
      last_req[i] <- propEq$R_equil ## gets overwritten each iteration
    } ## end areas
    # if(k == maxiter){ ## store quantities
    if(k > 5){
      ## keep iterating if changing more than 5% in early iters
      if(any(round(abs(radj[k,]/radj[k-5,] - 1),2)  > 0.05) & k < 20){
        next()
        ## if it is still bouncing wildly after 20, use the mode
      } else if(k > 20 & any(round(abs(radj[k,]/radj[k-5,] - 1),2)  > 0.05)){
        for (i in 1:narea) {
          # min(sort(table(radj[15:21,i]),decreasing=TRUE)[1:2])
          yield_FI[i] <-  YPR_i[i] * getmode(radj[20:k,i])
          B_FI[i] <-    SBPR_i[i] * getmode(radj[20:k,i])
          cat(Fv,k, i, 
              # min(as.numeric(tail(names(sort(table(radj[15:k,i]))), 2))), 
              getmode(radj[20:k,i]),
              B_FI[i], yield_FI[i], "\n")
        }
        break("maxiter reached ",i,k)
      ## if the changes are less than 5% or we've reached maxiter, end
     }else if(all(round(abs(radj[k,]/radj[k-5,] - 1),2)  <=  0.05) | k == maxiter){
       for (i in 1:narea) {
         yield_FI[i] <-  YPR_i[i] *  last_req[i]
         B_FI[i] <-    SBPR_i[i] *  last_req[i]
         cat(Fv,k, i,  last_req[i], B_FI[i], yield_FI[i], "\n")
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
