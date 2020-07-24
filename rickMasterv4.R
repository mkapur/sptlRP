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

## 2-area model with mixture movement ----
rm(list = ls())

narea <- 2
nages <- 21
steep <- rep(0.7,3)
recr_dist <- c(1,1) ## global recruits to areas
# R0_list <- list(c(500,500), c(300,700), c(700,300))
# R0_list <- list(c(50,50), c(30,70), c(70,30))
R0_list <- list(c(450,450), c(250,650), c(650,250))


lapply(list.files(here("R"), full.names = TRUE), source)
maxiter =  101 ## set to 1 to only use eigen
# Ftest <- seq(0,1.25,0.05) ## 80% of 1.25 is 1.0, so examining full expl

for(s in 1:3){
  splt <- list(c(1,1),
               c(0.6,0.4),
               c(0.8,0.2))[[s]]
  
  ## custom Ftest to max at 1
  Ftest <- seq(0,1/min(splt),0.05) ## 80% of 1.25 is 1.0, so examining full expl
  
  
  ## loop system wide F
  fyr_2area <- array(NA, dim = c(length(Ftest),3, length(R0_list))) ## F x 3  x RR 
  far_2area<- array(NA, dim = c(length(Ftest),narea+1, length(R0_list))) ## F x 3 x narea x RR 
  kvar_radj_2area <- array(NA, dim = c(maxiter,length(Ftest),narea+1,length(R0_list))) ## iters, Fv, 2 areas , RR x movements
  
  for(RR in 1:length(R0_list)){
    # eigv <- eigen(list(X_ija_EQUAL[,,1], X_ija_MIX2[,,1], X_ija_UNI2[,,1])[[m]])$vectors * sqrt(2)
    # receq = abs(diag(eigv) * R0_list[[RR]]) #eigv*R0_list[[RR]]
    for(Fv in 1:length(Ftest)){
      curr <- run_one_current(Fv_i = Ftest[Fv]*splt,
                              rec_level_idx = RR, 
                              recr_dist= recr_dist, 
                              movemat = X_ija_NULL2)
      # cat(curr$Yield,"\n")
      current_Req <- curr$R_ESUMB
      prop <- optim_loop2(Fv_i = Ftest[Fv]*splt, #rep(Ftest[Fv],narea),#
                          rec_level_idx = RR,
                          recr_dist= recr_dist,
                          movemat = X_ija_NULL2,
                          currReq = current_Req)
      
      # cat(Ftest[Fv], sum(prop$radj[min(which(is.na(prop$radj)))-1,]),current_Req,"\n")
      # cat(Ftest[Fv], prop$Yield,curr$Yield,"\n")
      # cat(Ftest[Fv], prop$radj[min(which(is.na(prop$radj)))-1,],"\n")
      # cat(Ftest[Fv], prop$radj[2,],"\n")
      
      
      fyr_2area[Fv,1,RR] <- Ftest[Fv]
      fyr_2area[Fv,2,RR] <- curr$Yield
      fyr_2area[Fv,3,RR] <- prop$Yield
      
      far_2area[Fv,1,RR] <- Ftest[Fv]
      far_2area[Fv,2,RR] <- prop$Yield_i[1]
      far_2area[Fv,3,RR] <- prop$Yield_i[2]
      
      
      kvar_radj_2area[,Fv,2:3,RR] <- prop$radj
      kvar_radj_2area[,Fv,1,RR] <- Ftest[Fv]
      
    } ## end Fs
  } ## end input rec levels
  
  ## plot radj
  # png(here('figs',paste0('R_eq_iterations_2area_Buff=0.005_5ppenalty_Mixture_',
  #                        paste(splt,collapse = "_"),"_",
  #                        Sys.Date(),'.png')),
  #     height = 8.5, width = 11, unit = 'in', res = 600)
  plot_radj(radj_kvar = kvar_radj_2area, Fidx = 10:15)
  # dev.off()
  
  ## plot yield comps solo and by area
  plot_yield_curves(sys_matrix = fyr_2area, byarea = FALSE)
  # ggsave( plot_yield_curves(sys_matrix = fyr_2area, byarea = FALSE),
  #         file = here('figs',paste0('Yield_comparisons_2area_buff0.005_5ppenalty_',
  #                                   paste(splt,collapse = "_"),"_",
  #                                   Sys.Date(),'.png')),
  #         width = 10, height = 8, unit = 'in', dpi = 520)
  
  plot_yield_curves(sys_matrix  = far_2area, byarea = TRUE) 
  # ggsave( plot_yield_curves(sys_matrix  = far_2area, byarea = TRUE),
  #         file = here('figs',paste0('Yielda_comparisons_2area_buff0.005_5ppenalty_',
  #                                   paste(splt,collapse = "_"),"_",
  #                                   Sys.Date(),'.png')),
  #         width = 10, height = 8, unit = 'in', dpi = 520)
  
  
}
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


