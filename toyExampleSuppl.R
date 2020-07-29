## Toy example suppl
## additional analyses conducted for but not shown in main manuscript


## 2-area model with mixture movement using MIX2B ----
## settings ----
narea <- 2
nages <- 21
steep <- rep(0.7,2)
R0_list <- list(c(500,500)*0.5, c(300,700)*0.5, c(700,300)*0.5)

lapply(list.files(here::here("R"), full.names = TRUE), source)
maxiter =  101 

## Run Algo and plot Yields vs Ftotal by secnario ----

master_plots <- master_plots_area <- list();idx = 1
## load FMSY dataframes made with uniroot
load("C:/Users/mkapur/Dropbox/UW/sptlRP/sys_optimize_current_MIX2b.Rdata") ## syopt_curr
load("C:/Users/mkapur/Dropbox/UW/sptlRP/sys_optimize_proposed_AB_MIX2b.Rdata") ## sysoptAB
load("C:/Users/mkapur/Dropbox/UW/sptlRP/sys_optimize_proposed_ss_MIX2b.Rdata") ## sysopt

for(s in 1:3){
  splt <- list(c(1,1),
               c(0.7,0.3),
               c(0.2,0.8))[[s]]
  
  ## custom Ftest to max at 1
  Ftest <- seq(0,1/min(splt),0.05) ## 80% of 1.25 is 1.0, so examining full expl
  
  
  ## loop system wide F
  fyr_2area <- array(NA, dim = c(length(Ftest),4, length(R0_list))) ## F x 3  x RR 
  far_2area<- array(NA, dim = c(length(Ftest),2*narea+1, length(R0_list))) ## F x 3 x narea x RR 
  kvar_radj_2area <-kvar_radj_2areaAB<-  array(NA, dim = c(maxiter,length(Ftest),narea+1,length(R0_list))) ## iters, Fv, 2 areas , RR x movements
  
  for(RR in 1:length(R0_list)){
    for(Fv in 1:length(Ftest)){
      curr <- run_one_current(Fv_i = Ftest[Fv]*splt,
                              rec_level_idx = RR,
                              movemat = X_ija_MIX2b)
      # cat(curr$Yield,"\n")
      current_Req <- curr$R_ESUMB
      prop <- optim_loop2(Fv_i = Ftest[Fv]*splt, #rep(Ftest[Fv],narea),#
                          rec_level_idx = RR,
                          movemat = X_ija_MIX2b)
      
      abprop <- abloop(Fv_i = Ftest[Fv]*splt, 
                       rec_level_idx = RR,
                       movemat = X_ija_MIX2b)
      
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
  png(here('figs',paste0('R_eq_ABsyntax_2area_Buff=0.005_5ppenalty_MIX2b_',
                         paste(splt,collapse = "_"),"_",
                         Sys.Date(),'.png')),
      height = 8.5, width = 11, unit = 'in', res = 600)
  plot_radj(radj_kvar = kvar_radj_2areaAB, Fidx = 12:16)
  dev.off()

  png(here('figs',paste0('R_eq_SSsyntax_2area_Buff=0.005_5ppenalty_MIX2b_',
                         paste(splt,collapse = "_"),"_",
                         Sys.Date(),'.png')),
      height = 8.5, width = 11, unit = 'in', res = 600)
  plot_radj(radj_kvar = kvar_radj_2area, Fidx = 12:16)
  dev.off()
  
  # ## plot yield comps solo and by area
  # cat(idx,"\n")
  for(i in 1:length(R0_list)){
    master_plots[[idx]] <- plot_yield_curves(sys_matrix = fyr_2area,
                                             byarea = FALSE,
                                             splitid = s)[[i]]
    
    master_plots_area[[idx]] <- plot_yield_curves(sys_matrix  = far_2area,
                                                  splitid = s,
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
        file = here::here('figs', paste0("Yields_total_MIX2b_",Sys.Date(),'.png')),
        width = 10, height = 8, unit = 'in', dpi = 520)

ggsave( Rmisc::multiplot(plotlist = master_plots_area, 
                         layout = matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)),
        file = here::here('figs',paste0("Yields_area_MIX2b_",  Sys.Date(),'.png')),
        width = 10, height = 8, unit = 'in', dpi = 520)

## confirm SRR funx working ----

plot(0:1000, abSRR(alpha = alph[1],  beta = bet[1],  
                   SPR_temp = 0:1000/R0_list[[1]][1])$R_equil,
     ylim = c(0,1000))

for(i in 0:1000){
  points(i, Equil_Spawn_Recr_Fxn(steepness = 0.7, 
                                 SSB_virgin = SB0_i[1],
                                 Recr_virgin = R0[1],
                                 SPR_temp  = (i)/R0_list[[1]][1])$R_equil,
         col = 'blue')
}


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