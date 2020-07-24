
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

optim_loop2 <- function(Fv_i,
                        rec_level_idx = 1,
                        movemat = X_ija,
                        recr_dist = c(1, 1, 1), 
                        currReq = NA){
  
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
  
  last_req <- first_req <- yield_FI <- B_FI <- SBPR_i <- YPR_i <- NULL
  for(k in 1:maxiter){ ## Loop over steps A & B
    
    if(k == 1){
      rdistUse <- recr_dist ## no distribution now; full rec-level in each area
      rlevelUse = rec_level ## pre-specified No recruits in area, currently R0
    } else{
      rdistUse <- recr_dist ## only after computing R_i
      rlevelUse = last_req + (1*rec_level) #last_req# round(last_req,2)# last_req  # c(R_eq_i[v,1:2], max(1,round(R_eq_i[v,3],0)))
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
      cat(k,i, prop$SB_i[i],"\n")
      cat(k,i, SBPR_i[i],"\n")

      YPR_i[i] <- prop$Yield_i[i]/(rleveltmp)
      # YPR_i[i] <- prop$Yield_i[i]/((rleveltmp+0.005*R0[i]))
      # cat(k,i, YPR_i[i],"\n")
      
      ## Calc area-specific recruits using area-specific SB etc
      propEq <- Equil_Spawn_Recr_Fxn(steepness = steep[i], SSB_virgin = SB0_i[i],
                                     Recr_virgin = R0[i], SPR_temp = SBPR_i[i])
      
  
      if(k == 1) first_req[i] <- propEq$R_equil
      # B_eq_i[v,i] <- propEq$B_equil
      last_req[i] <- propEq$R_equil ## gets overwritten each iteration
    } ## end areas
    # cat(Ftest[Fv],k, YPR_i, SBPR_i,last_req,"\n")
    
    # if(k == 1) { plot(rep(k,2), SBPR_i, col = c('black','blue'),
    #                   pch = 19,ylim = c(0,0.1), xlim = c(1,101))}
    # points(rep(k,2), SBPR_i, col = c('black','blue'),pch = 19)
    
    # if(k == 1) { plot(rep(k,2), YPR_i, col = c('black','blue'),
    #                   pch = 19,ylim = c(0,0.25), xlim = c(1,101))}
    # points(rep(k,2), YPR_i, col = c('black','blue'),pch = 19)
    # if(k == maxiter){ ## store quantities
    if(k > 20){
      ## keep iterating if changing more than 5% in early iters
      # if(any(round(abs(radj[k,]/radj[k-1,] - 1),2)  > 0.1) & k < maxiter){
        # next()
        ## if it is still bouncing wildly after 50, 
        ## use the mode OR rescale it OR coerce depleted ones to 1
      # } else if(any(round(abs(radj[k,]/radj[k-5,] - 1),2)  > 0.05) & k > 50){
        # last_req[which(radj[k,] < 1)] <- 1
        # next()
        # cat(Fv,k,i, rescale_req(movemat=movemat,new_req = last_req, p_ik = c(0.8,0.2)),"\n")
        # last_req <- rescale_req(movemat=movemat,
        #                        new_req = currReq,
        #                        p_ik = c(0,0))#,1-c(rec_level/sum(rec_level))) #c(0,1))
        # next()
        # for (i in 1:narea) {
          
          ## use mode
          # yield_FI[i] <-  YPR_i[i] * getmode(radj[50:k,i])
          # B_FI[i] <-    SBPR_i[i] * getmode(radj[50:k,i])
          # radj[k,i] <- getmode(radj[50:k,i])
          
          ## use min mode
          # yield_FI[i] <-  YPR_i[i] * min(as.numeric(tail(names(sort(table(radj[45:k,i]))), 2)))
          # B_FI[i] <-    SBPR_i[i] * min(as.numeric(tail(names(sort(table(radj[45:k,i]))), 2)))
          ## overwrite radj to show what was used
          # radj[k,i] <- min(as.numeric(tail(names(sort(table(radj[45:k,i]))), 2)))
          #
          ## use first iteration
          # yield_FI[i] <-  YPR_i[i] * first_req[i]
          # B_FI[i] <-    SBPR_i[i] * first_req[i]
          # radj[k,i] <-  first_req[i]
          
          ## use whatever last-req returned from main function
          # yield_FI[i] <-  YPR_i[i] * last_req[i]
          # B_FI[i] <-    SBPR_i[i] * last_req[i]
          # radj[k,i] <-  first_req[i]
          # 
          # cat(Fv,k, i,
          #     # 
          #     min(as.numeric(tail(names(sort(table(radj[15:k,i]))), 2))),
          #     getmode(radj[50:k,i]),
          #     B_FI[i], yield_FI[i], "\n")
        # }
        # break("maxiter reached ",i,k)
        ## if the changes are less than 5%
      ## or the ra
      ## or we've reached maxiter, end
      # }else 
      if(all(round(abs(radj[k,]/radj[k-1,] - 1),2)  <=  0.05) | 
         # all(round(abs(radj[k,]/radj[2,])) == 1)  |
         k == maxiter){
        # cat('done',"\n")
        # Yield = sum(YPR_i)*sum(last_req)
        # Yield = sum(YPR_i*last_req)
        for (i in 1:narea) {
          yield_FI[i] <-  YPR_i[i] *  last_req[i]
          B_FI[i] <-    SBPR_i[i] *  last_req[i]
          cat(Ftest[Fv],k, i,  last_req[i], B_FI[i], yield_FI[i], "\n")
            # cat(Ftest[Fv],k, i,  last_req[i], B_FI[i], yield_FI[i], "\n")
          } ## end areas
          # } else {
          #   ## had to manually calculate the eq, so use this
          #   last_req <- rescale_req(movemat=movemat,
          #                           new_req = last_req, 
          #                           p_ik = c(0.8,0.2))
          #   for (i in 1:narea) {
          #     yield_FI[i] <-  YPR_i[i] *  last_req[i]
          #     B_FI[i] <-    SBPR_i[i] *  last_req[i]
          #     cat(Ftest[Fv],k, i,  last_req[i], B_FI[i], yield_FI[i], "\n")
          #   } ## end areas
          # }
          break("maxiter reached ",i,k)
        } ## end if neither 1%
    } ## end K check
    # } ## end areas
  
    # cat(last_req,yield_FI,"\n")
    
  } ## end k:maxiter
  
  # data.frame(radj) %>% filter(!is.na(X1)) %>% mutate(X2/X1) ## 1.6
  # data.frame(radj) %>% filter(!is.na(X1)) %>% mutate(X1/X2) ## 0.62
  # 
  # Fv_i = 0.8*c(0.8,0.2)
  # radj <- matrix(NA, nrow = maxiter, ncol = narea)
  

  
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
