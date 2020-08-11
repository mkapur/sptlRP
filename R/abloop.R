

abloop <- function(Fv_i,
                        rec_level_idx = 1,
                        movemat = X_ija){

  radj <- matrix(NA, nrow = maxiter, ncol = narea)

  R0 <- R0_list[[rec_level_idx]]
  rec_level <- R0

  SB0_i <- doNage(Fv = rep(0,narea), 
                  X = movemat,
                  refR = rec_level)$SB_i
  
  ## get alpha, beta to match inputs
  ## fixed for each sim -- did this to match previous sims
  alph = (SB0_i/R0)*(1-steep)/(4*steep)
  bet = (5*steep-1)/(4*steep*R0)
  
  
  last_req <- first_req <- yield_FI <- B_FI <- SBPR_i <- YPR_i <- NULL
  for(k in 1:maxiter){ ## Loop over steps A & B
    
    if(k == 1){
      rlevelUse <- rec_level ## pre-specified No recruits in area, currently R0
    } else{
      rlevelUse <- last_req + rec_level
    }
    
    prop <- doNage( Fv = Fv_i, 
                    X = movemat,
                    refR = rlevelUse) 
    
    
    # call Equ_Spawn_Recr_Fxn for each area to get B_equil and R_equil from SPB/R and SR parms
    for(i in 1:narea){ ## will overwrite second time
      # calc area-specific SPB/R and Yield/R, using area-specific R
      
      if( k > 1){
        rleveltmp = rlevelUse[i] #min(, R0[i]) if unconstrained will be ~10 over R0
      } else{
        rleveltmp = rlevelUse[i]
      }
      radj[k,i] <- rleveltmp ## store this

      SBPR_i[i] <-  prop$SB_i[i]/(rleveltmp+0.005*R0[i]) ## Rick's idea

      YPR_i[i] <- prop$Yield_i[i]/(rleveltmp)

      ## Calc area-specific recruits using area-specific SB etc
      propEq <- abSRR(alpha = alph[i], 
                      beta = bet[i], 
                      SPR_temp =  SBPR_i[i])

      last_req[i] <- propEq$R_equil ## gets overwritten each iteration
    } ## end areas

    # cat(k,radj[k,],"\n")
    if(k > 20){
      if(all(round(abs(radj[k,]/radj[k-1,] - 1),2)  <=  0.05) |  k == maxiter){
        for (i in 1:narea) {
          yield_FI[i] <-  YPR_i[i] *  last_req[i]
          B_FI[i] <-    SBPR_i[i] *  last_req[i]
        } ## end areas

        break("maxiter reached ",i,k)
      } ## end if neither 1%
    } ## end K check

  } ## end k:maxiter

  ## save totals from final iteration
  return(list(
    Yield = sum(yield_FI),
    Biomass = sum(B_FI),
    Yield_i = yield_FI,
    Biomass_i = B_FI,
    radj = radj,
    last_req = last_req, ## what was used for Yield Calc
    last_YPR = YPR_i ## ratio mult for Yield
  ))
}
