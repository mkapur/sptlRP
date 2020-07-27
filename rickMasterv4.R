rm(list = ls())

## for running example
require(reshape)
require(dplyr, quietly = T)
require(here)
require(stats4)

## for plotting
require(cowplot)
require(patchwork)
require(ggsidekick)
require(ggplot2, quietly = T)



## 2-area model with mixture movement ----
rm(list = ls())

narea <- 2
nages <- 21
steep <- rep(0.7,2)
recr_dist <- c(1,1) ## global recruits to areas
R0_list <- list(c(500,500), c(300,700), c(700,300))
# R0_list <- list(c(50,50), c(30,70), c(70,30))
# R0_list <- list(c(450,450), c(250,650), c(650,250))

lapply(list.files(here::here("R"), full.names = TRUE), source)
maxiter =  101 ## set to 1 to only use eigen
master_plots <- master_plots_area <- list();idx = 1

for(s in 1:3){
  splt <- list(c(1,1),
               c(0.7,0.3),
               c(0.4,0.6))[[s]]
  
  ## custom Ftest to max at 1
  Ftest <- seq(0,1/min(splt),0.05) ## 80% of 1.25 is 1.0, so examining full expl
  
  
  ## loop system wide F
  fyr_2area <- array(NA, dim = c(length(Ftest),4, length(R0_list))) ## F x 3  x RR 
  far_2area<- array(NA, dim = c(length(Ftest),2*narea+1, length(R0_list))) ## F x 3 x narea x RR 
  kvar_radj_2area <-kvar_radj_2areaAB<-  array(NA, dim = c(maxiter,length(Ftest),narea+1,length(R0_list))) ## iters, Fv, 2 areas , RR x movements
  
  for(RR in 1:length(R0_list)){
    # eigv <- eigen(list(X_ija_EQUAL[,,1], X_ija_MIX2[,,1], X_ija_UNI2[,,1])[[m]])$vectors * sqrt(2)
    # receq = abs(diag(eigv) * R0_list[[RR]]) #eigv*R0_list[[RR]]
    for(Fv in 1:length(Ftest)){
      curr <- run_one_current(Fv_i = Ftest[Fv]*splt,
                              rec_level_idx = RR,
                              movemat = X_ija_MIX2)
      # cat(curr$Yield,"\n")
      current_Req <- curr$R_ESUMB
      prop <- optim_loop2(Fv_i = Ftest[Fv]*splt, #rep(Ftest[Fv],narea),#
                          rec_level_idx = RR,
                          movemat = X_ija_MIX2)
      
      abprop <- abloop(Fv_i = Ftest[Fv]*splt, 
                     rec_level_idx = RR,
                     movemat = X_ija_MIX2)

      fyr_2area[Fv,1,RR] <- Ftest[Fv]
      fyr_2area[Fv,2,RR] <- curr$Yield
      fyr_2area[Fv,3,RR] <- prop$Yield
      fyr_2area[Fv,4,RR] <- abprop$Yield
      
      far_2area[Fv,1,RR] <- Ftest[Fv]
      far_2area[Fv,2,RR] <- prop$Yield_i[1]
      far_2area[Fv,3,RR] <- prop$Yield_i[2]
      far_2area[Fv,4,RR] <- abprop$Yield_i[1]
      far_2area[Fv,5,RR] <- abprop$Yield_i[2]
      
      
      kvar_radj_2area[,Fv,2:3,RR] <- prop$radj
      kvar_radj_2area[,Fv,1,RR] <-    kvar_radj_2areaAB[,Fv,1,RR]  <- Ftest[Fv]
      kvar_radj_2areaAB[,Fv,2:3,RR] <- abprop$radj

    } ## end Fs
  } ## end input rec levels
  
  ## plot radj
  # png(here('figs',paste0('R_eq_ABloop_2area_Buff=0.005_5ppenalty_Mixture_',
  #                        paste(splt,collapse = "_"),"_",
  #                        Sys.Date(),'.png')),
  #     height = 8.5, width = 11, unit = 'in', res = 600)
  # plot_radj(radj_kvar = kvar_radj_2areaAB, Fidx = 17:21)
  # dev.off()
  
  ## plot yield comps solo and by area
  cat(idx,"\n")
  for(i in 1:length(R0_list)){
    master_plots[[idx]] <- plot_yield_curves(sys_matrix = fyr_2area,
                                             byarea = FALSE,
                                             splitid = s)[[i]]
    
    master_plots_area[[idx]] <- plot_yield_curves(sys_matrix  = far_2area, 
                                                  byarea = TRUE)[[i]]
    idx = idx+1
  }
  

  ## save individual pik x 3
  # ggsave(Rmisc::multiplot(
  #   plotlist = plot_yield_curves(sys_matrix = fyr_2area, byarea = FALSE,
  #                                splitid = s), cols = 3),
  #         file = here('figs',paste0('Yield_comparisons_2area_buff0.005_5ppenalty_',
  #                                   paste(splt,collapse = "_"),"_",
  #                                   Sys.Date(),'.png')),
  #         width = 10, height = 8, unit = 'in', dpi = 520)
  # 
  # ggsave( Rmisc::multiplot(plotlist = plot_yield_curves(sys_matrix  = far_2area, byarea = TRUE), cols = 3),
  #         file = here('figs',paste0('Yielda_comparisons_ABloop_2area_buff0.005_5ppenalty_',
  #                                   paste(splt,collapse = "_"),"_",
  #                                   Sys.Date(),'.png')),
  #         width = 10, height = 8, unit = 'in', dpi = 520)

}

ggsave( Rmisc::multiplot(plotlist = master_plots, 
                         layout = matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)),
        file = here::here('figs', paste0("Yields_total_mix2b_",Sys.Date(),'.png')),
        width = 10, height = 8, unit = 'in', dpi = 520)

ggsave( Rmisc::multiplot(plotlist = master_plots_area, 
                         layout = matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)),
        file = here::here('figs',paste0("Yields_area_mix2b_",  Sys.Date(),'.png')),
        width = 10, height = 8, unit = 'in', dpi = 520)
## TRIAL 2: Optimizing F ----

## functions to optimize ----
dfx.dxSYS <- function(Fv_test,RLI, movemat = X_ija_MIX2, pik = c(1,1) ){
  y1 <- optim_loop2(Fv_i = pik*(Fv_test-0.001),
                    rec_level_idx = RLI, 
                    movemat = movemat)$Yield
  y2 <- optim_loop2(Fv_i =  pik*(Fv_test+0.001), 
                    rec_level_idx = RLI,
                    movemat = movemat)$Yield
  appx <- (y2-y1)/(0.002) #0.002 is total X delta; we are using system yield
  return(appx)
}


dfx.dxSYSab <- function(Fv_test,RLI, movemat = X_ija_MIX2, pik = c(1,1)  ){
  y1 <- abloop(Fv_i =  pik*(Fv_test-0.001), 
               rec_level_idx = RLI, movemat = movemat)$Yield
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


## 2A F sys @ both approaches ----
sysopt <- sysoptAB <- array(NA, dim = c(3,8,3)) ## RR X cols x S
# names(sysopt[,1:3]) <- c('RR','FsysMSY','BsysMSY','YsysMSY',
                         # 'YA1','YA2','BA1','BA2')


sysopt_curr <- array(NA, dim = c(3,4,3))
# names(sysopt[,1:3]) <- c('RR','FsysMSY','BsysMSY','YsysMSY')
                         

for(s in 1:3){
  splt <- list(c(1,1),
               c(0.6,0.4),
               c(0.2,0.8))[[s]]
  Ftest <- seq(0,1/min(splt),0.05) ## 80% of 1.25 is 1.0, so examining full expl
  
  for(RR in 1:3){
    
    # ## PROPOSED
    sysopt[RR,1,s] <-   sysoptAB[RR,1,s] <-  sysopt_curr[RR,1,s] <- RR
    sysopt[RR,2,s] <- as.numeric(uniroot(f = dfx.dxSYS,
                                         RLI = 1,
                                         pik = splt,
                                           interval = c(0.02,1))[1])
    
    
    
    run_at_msy <- optim_loop2(
      Fv_i = rep(sysopt[RR, 2, s], narea),
      rec_level_idx = RR,
      movemat = X_ija_MIX2b
    )
    sysopt[RR,3,s] <- run_at_msy$Yield
    sysopt[RR,4,s] <- run_at_msy$Biomass
    
    sysopt[RR,5:6,s] <- run_at_msy$Yield_i
    sysopt[RR,7:8,s] <- run_at_msy$Biomass_i
    
    ## proposed AB
    sysoptAB[RR,2,s] <- as.numeric(uniroot(f = dfx.dxSYSab,
                                           RLI = 1,      pik = splt,
                                           
                                           interval = c(0.02,1))[1])
    run_at_msy <- abloop(Fv_i = rep(  sysoptAB[RR,2,s],narea),
                         movemat  = X_ija_MIX2b,
                         rec_level_idx = RR)
    sysoptAB[RR,3,s] <- run_at_msy$Yield
    sysoptAB[RR,4,s] <- run_at_msy$Biomass
    
    sysoptAB[RR,5:6,s] <- run_at_msy$Yield_i
    sysoptAB[RR,7:8,s] <- run_at_msy$Biomass_i  
    ## current
    sysopt_curr[RR,2,s] <- as.numeric(uniroot(f = dfx.dxSYS_curr, 
                                              RLI = RR,
                                              pik = splt,
                                              interval = c(0.02,1))[1])
    curr_at_FMSY <- run_one_current(Fv_i = rep(  sysopt_curr[RR,2,s],narea),  
                                    movemat  = X_ija_MIX2b,
                                    rec_level_idx = RR)
    
    sysopt_curr[RR,3,s] <- curr_at_FMSY$Yield
    sysopt_curr[RR,4,s] <- curr_at_FMSY$B
    
  } # end RR
} ## end split

save(sysopt,file = here::here("sys_optimize_proposed_ss_MIX2b.Rdata"))
save(sysoptAB,file = here::here("sys_optimize_proposed_AB_MIX2b.Rdata"))
save(sysopt_curr,file = here::here("sys_optimize_current_MIX2b.Rdata"))

## 2B F_i @ proposed ----

minFunc <- function(F1,F2,RLI){
  minus <- as.numeric(c(F1 - 0.001, F2 - 0.001))
  plus <- as.numeric(c(F1 +0.001, F2 + 0.001))
  y1 <- optim_loop2(Fv_i = minus, rec_level_idx = RLI, movemat = X_ija_MIX2)$Yield
  y2 <- optim_loop2(Fv_i = plus, rec_level_idx = RLI, movemat = X_ija_MIX2)$Yield
  appx <- abs(y2-y1)/(0.002) ## system yield again
  return(appx)
}

minFuncAB <- function(F1,F2,RLI){
  minus <- as.numeric(c(F1 - 0.001, F2 - 0.001))
  plus <- as.numeric(c(F1 +0.001, F2 + 0.001))
  y1 <- abloop(Fv_i = minus, rec_level_idx = RLI, movemat = X_ija_MIX2)$Yield
  y2 <- abloop(Fv_i = plus, rec_level_idx = RLI, movemat = X_ija_MIX2)$Yield
  appx <- abs(y2-y1)/(0.002) ## system yield again
  return(appx)
}


areaopt <- areaoptAB <- data.frame('RR' = NA, 
                      'F1' = NA, 'F2' = NA,
                      'YA1' = NA, 'YA2' = NA, 
                      'BA1' = NA, 'BA2' = NA, 
                      'CURRENT_YIELD')
for(RR in 1:3){
  areaopt[RR,'RR'] <- areaoptAB[RR,'RR'] <- RR
  ##proposed srr
  FVTEMP <-  coef(mle(minFunc,
                      start = list(F1 = 0.025, F2 = 0.025),
                      method = "L-BFGS-B",
                      fixed = list( RLI = RR), ## subsetting eq method
                      lower = c(0.02, 0.02), upper = c(0.4,0.4)))[1:2]
  
  run_at_msy <- optim_loop2(Fv_i = FVTEMP, rec_level_idx = RR,movemat = X_ija_MIX2)
  areaopt[RR,2:3] <- FVTEMP
  areaopt[RR,4:5] <- round(run_at_msy$Yield_i,2)
  areaopt[RR,6:7] <- round(run_at_msy$Biomass_i,2)
  
  curr_at_ftemp <- run_one_current(Fv_i =   as.numeric(areaopt[RR,2:3]), 
                                   rec_level_idx = RR,movemat = X_ija_MIX2)
  areaopt[RR, 'CURRENT_YIELD'] <-   curr_at_ftemp$Yield
  ## proposed ab
  FVTEMP <-  coef(mle(minFuncAB,
                      start = list(F1 = 0.025, F2 = 0.025),
                      method = "L-BFGS-B",
                      fixed = list( RLI = RR), ## subsetting eq method
                      lower = c(0.02, 0.02), upper = c(0.4,0.4)))[1:2]
  
  run_at_msy <- abloop(Fv_i = FVTEMP, rec_level_idx = RR,movemat = X_ija_MIX2)
  areaoptAB[RR,2:3] <- FVTEMP
  areaoptAB[RR,4:5] <- round(run_at_msy$Yield_i,2)
  areaoptAB[RR,6:7] <- round(run_at_msy$Biomass_i,2)
  
  curr_at_ftemp <- run_one_current(Fv_i =   as.numeric(areaoptAB[RR,2:3]), 
                                   rec_level_idx = RR,
                                   movemat = X_ija_MIX2)
  areaoptAB[RR, 'CURRENT_YIELD'] <-curr_at_ftemp$Yield
 
}

save(areaopt,file = here("area_optimize_proposed_ss.Rdata"))
save(areaoptAB,file = here("area_optimize_proposed_AB.Rdata"))

# 
# 
# plot(rep(0,2),
#      kvar_radj_2area[,1,2:3,1][min(which(is.na(kvar_radj_2area[,1,2:3,1])))-1,],
#      xlim = c(0,max(Ftest)),
#      ylim = c(0,1000),
#      col = c('black','blue'), pch =19)
# tmpx = NULL; rproc = data.frame("Fval", "A1_term", "A2_term")
# 
# for(i in 2:length(Ftest)){
#   tmpx[i] = ifelse(min(which(is.na(kvar_radj_2area[,i,2:3,1])))-1 == Inf, 101, 
#                 min(which(is.na(kvar_radj_2area[,i,2:3,1])))-1)
#   
#   
#   points(rep(Ftest[i],2),
#          kvar_radj_2area[,i,2:3,1][tmpx[i],],
#          col = c('black','blue'), pch  = 19)
#   rproc$Fval[i] <- Ftest[i]
#   rproc$A1_term[i] = kvar_radj_2area[,i,2,1][tmpx[i]]
#   rproc$A2_term[i] = kvar_radj_2area[,i,3,1][tmpx[i]]
#   
# }
# ## identify relationship for "functional" a1 a2 radj decline
# 
# 
# lm(A1_term ~ Fval, data = subset(rproc, Fval<0.4))
# lm(A2_term ~ Fval, data = subset(rproc, Fval>0.4))
# lm(A1_term/A2_term ~ Fval, data = subset(rproc, Fval>0.75))
# lm(A1_term/A2_term ~ Fval, data = subset(rproc, Fval<0.8))

## 3-area Model with Unidirectional movement ----
narea <- 3
nages <- 21
steep <- rep(1,3)
maxiter <- 101
recr_dist <- c(1,1,1) ## global recruits to areas
R0_list <- list(c(42,33,25),
                rev(c(42,33,25)),
                c(33.3,33.3,33.3))


## load functions & initialize OM
lapply(list.files(here("R"), full.names = TRUE), source)

Ftest <- seq(0,1.25,0.05) ## 80% of 1.25 is 1.0, so examining full expl



for(s in 1:3){
  splt <- list(c(1,1,1),
               c(0.25,0.25,0.5),
               c(0.45,0.45,0.1))[[s]]
  
  
  ## loop system wide F
  fyr_3area <- array(NA, dim = c(length(Ftest),3, length(R0_list))) ## F x 3  x RR 
  far_3area<- array(NA, dim = c(length(Ftest),narea+1, length(R0_list))) ## F x 3 x narea x RR 
  kvar_radj_3area <- array(NA, dim = c(maxiter,length(Ftest),narea+1,length(R0_list))) ## iters, Fv, 2 areas , RR x movements
  
  for(RR in 1:length(R0_list)){
    for(Fv in 1:length(Ftest)){
      curr <- run_one_current(Fv_i = Ftest[Fv]*splt,#rep(Ftest[Fv],narea), 
                              rec_level_idx = RR, 
                              recr_dist= recr_dist, 
                              movemat = X_ija)
      prop <- optim_loop2(Fv_i = Ftest[Fv]*splt, #rep(Ftest[Fv],narea), 
                          rec_level_idx = RR, 
                          # receq= receq,
                          recr_dist= recr_dist, 
                          movemat = X_ija)
      # sys3area[Fv,1,RR] <- Ftest[Fv]
      # sys3area[Fv,2,RR] <- curr$Yield
      # sys3area[Fv,3,RR] <- prop$Yield
      # 
      # radj3area[,Fv,2:4,RR] <- prop$radj
      # radj3area[,Fv,1,RR] <- Ftest[Fv]
      
      fyr_3area[Fv,1,RR] <- Ftest[Fv]
      fyr_3area[Fv,2,RR] <- curr$Yield
      fyr_3area[Fv,3,RR] <- prop$Yield
      
      far_3area[Fv,1,RR] <- Ftest[Fv]
      far_3area[Fv,2,RR] <- prop$Yield_i[1]
      far_3area[Fv,3,RR] <- prop$Yield_i[2]
      far_3area[Fv,4,RR] <- prop$Yield_i[3]
      
      
      kvar_radj_3area[,Fv,2:4,RR] <- prop$radj
      kvar_radj_3area[,Fv,1,RR] <- Ftest[Fv]
      
      # rRef_proposed[,,RR] <- as.matrix(proposed)
      # rRef_proposed_i[,,,RR] <- proposed_i
      # cat(m,"\t",RR,"\t",Fv,"\n")
    } ## end Fs
  } ## end input rec levels
  
  ## plot yield comps solo and by area
  plot_yield_curves(sys_matrix  = fyr_3area, byarea = FALSE)
  ggsave( plot_yield_curves(sys_matrix  = fyr_3area, byarea = FALSE),
          file = here('figs',paste0('Yield_comparisons_3area_buff0.005_5ppenalty_',
                                    paste(splt,collapse = "_"),"_",
                                    Sys.Date(),'.png')),
          width = 10, height = 8, unit = 'in', dpi = 520)
  
  plot_yield_curves(sys_matrix  = far_3area, byarea = TRUE)
  ggsave( plot_yield_curves(sys_matrix  = far_3area, byarea = TRUE),
          file = here('figs',paste0('Yielda_comparisons_3area_buff0.005_5ppenalty_',
                                    paste(splt,collapse = "_"),"_",
                                    Sys.Date(),'.png')),
          width = 10, height = 8, unit = 'in', dpi = 520)
  
  ## plot radj
  png(here('figs',paste0('R_eq_iterations_3area_Buff=0.005_5ppenalty_Mixture_',
                         paste(splt,collapse = "_"),"_",
                         Sys.Date(),'.png')),
      height = 8.5, width = 11, unit = 'in', res = 600)
  plot_radj(kvar_radj_3area)
  dev.off()
  
}
