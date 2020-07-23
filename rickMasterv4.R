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
steep <- rep(0.7,3)
maxiter <- 101
recr_dist <- c(1,1,1) ## global recruits to areas
R0_list <- list(c(420,330,250),
                rev(c(420,330,250)),
                c(333,333,333))


## load functions & initialize OM
lapply(list.files(here("R"), full.names = TRUE), source)

Ftest <- seq(0,1,0.05)
# rRef_current <- array(NA, dim = c(length(Ftest),3,length(R0_list)))


## loop system wide F
# sys3area <- array(NA, dim = c(length(Ftest),3,length(R0_list))) ## F x 3 x RR x
# radj3area <- array(NA, dim = c(maxiter,length(Ftest),  narea+1,length(R0_list))) ## iters, Fv, 2 areas , RR 
# 
# 
# 
# rRef_proposed <- array(NA, dim = c(length(Ftest),3,length(R0_list)))
# rRef_proposed_i <- array(NA, dim = c(length(Ftest),3,narea,length(R0_list)))


## loop system wide F
fyr_3area <- array(NA, dim = c(length(Ftest),3, length(R0_list))) ## F x 3  x RR 
far_3area<- array(NA, dim = c(length(Ftest),narea+1, length(R0_list))) ## F x 3 x narea x RR 
kvar_radj_3area <- array(NA, dim = c(maxiter,length(Ftest),narea+1,length(R0_list))) ## iters, Fv, 2 areas , RR x movements


for(RR in 1:length(R0_list)){
  for(Fv in 1:length(Ftest)){
    curr <- run_one_current(Fv_i = c(Ftest[Fv]*1,Ftest[Fv]*1,Ftest[Fv]*1 ),#rep(Ftest[Fv],narea), 
                            rec_level_idx = RR, 
                            recr_dist= recr_dist, 
                            movemat = X_ija)
    prop <- optim_loop2(Fv_i = c(Ftest[Fv]*1,Ftest[Fv]*1,Ftest[Fv]*1 ), #rep(Ftest[Fv],narea), 
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
        file = here('figs',paste0('Yield_comparisons_3area_buff0.005_5ppenalty_',Sys.Date(),'.png')),
       width = 10, height = 8, unit = 'in', dpi = 520)

plot_yield_curves(sys_matrix  = far_3area, byarea = TRUE)
ggsave( plot_yield_curves(sys_matrix  = far_3area, byarea = TRUE),
        file = here('figs',paste0('Yielda_comparisons_3area_buff0.005_5ppenalty_',Sys.Date(),'.png')),
        width = 10, height = 8, unit = 'in', dpi = 520)

## plot radj
png(here('figs',paste0('R_eq_iterations_3area_Buff=0.005_5ppenalty_Mixture_',
                       Sys.Date(),'.png')),
    height = 8.5, width = 11, unit = 'in', res = 600)
plot_radj(kvar_radj_3area)
dev.off()

## 2-area model with mixture movement ----
rm(list = ls())
narea <- 2
nages <- 21
steep <- rep(0.7,3)
recr_dist <- c(1,1) ## global recruits to areas
R0_list <- list(c(500,500), c(300,700), c(700,300))
lapply(list.files(here("R"), full.names = TRUE), source)
maxiter =  101 ## set to 1 to only use eigen
Ftest <- seq(0,1,0.05)

## loop system wide F
fyr_2area <- array(NA, dim = c(length(Ftest),3, length(R0_list))) ## F x 3  x RR 
far_2area<- array(NA, dim = c(length(Ftest),narea+1, length(R0_list))) ## F x 3 x narea x RR 
kvar_radj_2area <- array(NA, dim = c(maxiter,length(Ftest),narea+1,length(R0_list))) ## iters, Fv, 2 areas , RR x movements

for(RR in 1:length(R0_list)){
  # eigv <- eigen(list(X_ija_EQUAL[,,1], X_ija_MIX2[,,1], X_ija_UNI2[,,1])[[m]])$vectors * sqrt(2)
  # receq = abs(diag(eigv) * R0_list[[RR]]) #eigv*R0_list[[RR]]
  for(Fv in 1:length(Ftest)){
    curr <- run_one_current(Fv_i = c(Ftest[Fv]*0.8,Ftest[Fv]*0.2), #rep(Ftest[Fv],narea),  # 
                            rec_level_idx = RR, 
                            recr_dist= recr_dist, 
                            movemat = X_ija_MIX2)
    # cat(curr$Yield,"\n")
    prop <- optim_loop2(Fv_i = c(Ftest[Fv]*0.8,Ftest[Fv]*0.2), #rep(Ftest[Fv],narea),
                        rec_level_idx = RR,
                        recr_dist= recr_dist,
                        movemat = X_ija_MIX2)
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

## plot yield comps solo and by area
plot_yield_curves(sys_matrix = fyr_2area, byarea = FALSE)
ggsave( plot_yield_curves(sys_matrix = fyr_2area, byarea = FALSE),
        file = here('figs',paste0('Yield_comparisons_2area_buff0.005_5ppenalty_8020_',Sys.Date(),'.png')),
        width = 10, height = 8, unit = 'in', dpi = 520)

plot_yield_curves(sys_matrix  = far_2area, byarea = TRUE)
ggsave( plot_yield_curves(sys_matrix  = far_2area, byarea = TRUE),
file = here('figs',paste0('Yielda_comparisons_2area_buff0.005_5ppenalty_',Sys.Date(),'.png')),
width = 10, height = 8, unit = 'in', dpi = 520)

## plot radj
# png(here('figs',paste0('R_eq_iterations_2area_Buff=0.005_5ppenalty_Mixture_',
#                        Sys.Date(),'.png')),
#     height = 8.5, width = 11, unit = 'in', res = 600)
plot_radj(radj_kvar = kvar_radj_2area, Fidx = 15:18)
# dev.off()
