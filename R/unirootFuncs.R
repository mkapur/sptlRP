
## functions to optimize using uniroot ----
dfx.dxSYS <- function(Fv_test,RLI, movemat = X_ija_MIX2, pik = c(1,1) ){
  
  y1 <- optim_loop2(Fv_i = pik*(Fv_test-0.001),
                    rec_level_idx = RLI, 
                    movemat = movemat)$Yield
  
  cat('SS',y1,'\n')
  
  y2 <- optim_loop2(Fv_i =  pik*(Fv_test+0.001), 
                    rec_level_idx = RLI,
                    movemat = movemat)$Yield
  appx <- (y2-y1)/(0.002) #0.002 is total X delta; we are using system yield
  return(appx)
}


dfx.dxSYSab <- function(Fv_test,RLI, movemat = X_ija_MIX2, pik = c(1,1)  ){
  
  y1 <- abloop(Fv_i =  pik*(Fv_test-0.001), 
               rec_level_idx = RLI, 
               movemat = movemat)$Yield
  
  cat('ab',y1,'\n')
  
  y2 <- abloop(Fv_i = pik*(Fv_test+0.001), 
               rec_level_idx = RLI,
               movemat = movemat)$Yield
  appx <- (y2-y1)/(0.002) #0.002 is total X delta; we are using system yield
  return(appx)
}


dfx.dxSYS_curr <-  function(Fv_test,RLI, movemat = X_ija_MIX2, pik = c(1,1)  ){
  y1 <- run_one_current(Fv_i = pik*(Fv_test-0.001), 
                        rec_level_idx = RLI, 
                        movemat = movemat)$Yield
  y2 <- run_one_current(Fv_i = pik*(Fv_test+0.001), 
                        rec_level_idx = RLI, 
                        movemat = movemat)$Yield
  appx <- (y2-y1)/(0.002) #0.002 is total X delta; we are using system yield
  return(appx)
}

## deprecated ----
## 2B F_i @ proposed ----

# minFunc <- function(F1,F2,RLI){
#   minus <- as.numeric(c(F1 - 0.001, F2 - 0.001))
#   plus <- as.numeric(c(F1 +0.001, F2 + 0.001))
#   y1 <- optim_loop2(Fv_i = minus, rec_level_idx = RLI, movemat = X_ija_MIX2)$Yield
#   y2 <- optim_loop2(Fv_i = plus, rec_level_idx = RLI, movemat = X_ija_MIX2)$Yield
#   appx <- abs(y2-y1)/(0.002) ## system yield again
#   return(appx)
# }
# 
# minFuncAB <- function(F1,F2,RLI){
#   minus <- as.numeric(c(F1 - 0.001, F2 - 0.001))
#   plus <- as.numeric(c(F1 +0.001, F2 + 0.001))
#   y1 <- abloop(Fv_i = minus, rec_level_idx = RLI, movemat = X_ija_MIX2)$Yield
#   y2 <- abloop(Fv_i = plus, rec_level_idx = RLI, movemat = X_ija_MIX2)$Yield
#   appx <- abs(y2-y1)/(0.002) ## system yield again
#   return(appx)
# }
# 
# 
# areaopt <- areaoptAB <- data.frame('RR' = NA, 
#                       'F1' = NA, 'F2' = NA,
#                       'YA1' = NA, 'YA2' = NA, 
#                       'BA1' = NA, 'BA2' = NA, 
#                       'CURRENT_YIELD')
# for(RR in 1:3){
#   areaopt[RR,'RR'] <- areaoptAB[RR,'RR'] <- RR
#   ##proposed srr
#   FVTEMP <-  coef(mle(minFunc,
#                       start = list(F1 = 0.025, F2 = 0.025),
#                       method = "L-BFGS-B",
#                       fixed = list( RLI = RR), ## subsetting eq method
#                       lower = c(0.02, 0.02), upper = c(0.4,0.4)))[1:2]
#   
#   run_at_msy <- optim_loop2(Fv_i = FVTEMP, rec_level_idx = RR,movemat = X_ija_MIX2)
#   areaopt[RR,2:3] <- FVTEMP
#   areaopt[RR,4:5] <- round(run_at_msy$Yield_i,2)
#   areaopt[RR,6:7] <- round(run_at_msy$Biomass_i,2)
#   
#   curr_at_ftemp <- run_one_current(Fv_i =   as.numeric(areaopt[RR,2:3]), 
#                                    rec_level_idx = RR,movemat = X_ija_MIX2)
#   areaopt[RR, 'CURRENT_YIELD'] <-   curr_at_ftemp$Yield
#   ## proposed ab
#   FVTEMP <-  coef(mle(minFuncAB,
#                       start = list(F1 = 0.025, F2 = 0.025),
#                       method = "L-BFGS-B",
#                       fixed = list( RLI = RR), ## subsetting eq method
#                       lower = c(0.02, 0.02), upper = c(0.4,0.4)))[1:2]
#   
#   run_at_msy <- abloop(Fv_i = FVTEMP, rec_level_idx = RR,movemat = X_ija_MIX2)
#   areaoptAB[RR,2:3] <- FVTEMP
#   areaoptAB[RR,4:5] <- round(run_at_msy$Yield_i,2)
#   areaoptAB[RR,6:7] <- round(run_at_msy$Biomass_i,2)
#   
#   curr_at_ftemp <- run_one_current(Fv_i =   as.numeric(areaoptAB[RR,2:3]), 
#                                    rec_level_idx = RR,
#                                    movemat = X_ija_MIX2)
#   areaoptAB[RR, 'CURRENT_YIELD'] <-curr_at_ftemp$Yield
#  
# }
# 
# save(areaopt,file = here("area_optimize_proposed_ss.Rdata"))
# save(areaoptAB,file = here("area_optimize_proposed_AB.Rdata"))
