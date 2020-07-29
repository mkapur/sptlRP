## Toy Example Master
## M Sosa Kapur kapurm@uw.edu - Summer 2020
## Code to reproduce analyses presented in Kapur et al.
## Eq Quantities for next generation of spatial assessment models

# rm(list = ls())

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
## settings ----
narea <- 2
nages <- 21
steep <- rep(0.7,2)
R0_list <- list(c(500,500)*0.5, c(300,700)*0.5, c(700,300)*0.5)

lapply(list.files(here::here("R"), full.names = TRUE), source)
maxiter =  101 


## Find Ftotal MSY for plotting ----
sysopt <- sysoptAB <- array(NA, dim = c(3,8,3)) ## RR X cols x S
sysopt_curr <- array(NA, dim = c(3,4,3))

for(s in 1:3){
  splt <- list(c(1,1),
               c(0.7,0.3),
               c(0.2,0.8))[[s]]
  Ftest <- seq(0,1/min(splt),0.05) ## 80% of 1.25 is 1.0, so examining full expl
  
  for(RR in 1:3){
    
    # ## PROPOSED
    sysopt[RR,1,s] <-   sysoptAB[RR,1,s] <-  sysopt_curr[RR,1,s] <- RR
    sysopt[RR,2,s] <- as.numeric(uniroot(f = dfx.dxSYS,
                                         RLI = 1,
                                         pik = splt,
                                         movemat = X_ija_MIX2,
                                         interval = c(0.02,1))[1])
    
    
    
    run_at_msy <- optim_loop2(
      Fv_i = splt*sysopt[RR, 2, s],
      rec_level_idx = RR,
      movemat = X_ija_MIX2
    )
    sysopt[RR,3,s] <- run_at_msy$Yield
    sysopt[RR,4,s] <- run_at_msy$Biomass
    
    sysopt[RR,5:6,s] <- run_at_msy$Yield_i
    sysopt[RR,7:8,s] <- run_at_msy$Biomass_i
    
    ## proposed AB
    sysoptAB[RR,2,s] <- as.numeric(uniroot(f = dfx.dxSYSab,
                                           RLI = 1,  
                                           pik = splt,
                                           movemat = X_ija_MIX2,
                                           interval = c(0.02,ifelse(s == 1, 0.55,1)))[1])
    
    run_at_msy <- abloop(Fv_i = splt*sysoptAB[RR,2,s],
                         movemat  = X_ija_MIX2,
                         rec_level_idx = RR)
    sysoptAB[RR,3,s] <- run_at_msy$Yield
    sysoptAB[RR,4,s] <- run_at_msy$Biomass
    
    sysoptAB[RR,5:6,s] <- run_at_msy$Yield_i
    sysoptAB[RR,7:8,s] <- run_at_msy$Biomass_i  
    ## current
    sysopt_curr[RR,2,s] <- as.numeric(uniroot(f = dfx.dxSYS_curr, 
                                              RLI = RR,
                                              pik = splt,
                                              movemat = X_ija_MIX2,
                                              interval = c(0.02,1))[1])
    
    curr_at_FMSY <- run_one_current(Fv_i =  splt*sysopt_curr[RR,2,s],  
                                    movemat  = X_ija_MIX2,
                                    rec_level_idx = RR)
    
    sysopt_curr[RR,3,s] <- curr_at_FMSY$Yield
    sysopt_curr[RR,4,s] <- curr_at_FMSY$B
    
  } # end RR
} ## end split

# save(sysopt,file = here::here("sys_optimize_proposed_ss_MIX2.Rdata"))
# save(sysoptAB,file = here::here("sys_optimize_proposed_AB_MIX2.Rdata"))
# save(sysopt_curr,file = here::here("sys_optimize_current_MIX2.Rdata"))

## Run Algo and plot Yields vs Ftotal by secnario ----

# load("C:/Users/mkapur/Dropbox/UW/sptlRP/sys_optimize_current_MIX2.Rdata") ## syopt_curr
# load("C:/Users/mkapur/Dropbox/UW/sptlRP/sys_optimize_proposed_AB_MIX2.Rdata") ## sysoptAB
# load("C:/Users/mkapur/Dropbox/UW/sptlRP/sys_optimize_proposed_ss_MIX2.Rdata") ## sysopt


master_plots <- master_plots_area <- list();idx = 1

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
  # png(here('figs',paste0('R_eq_ABsyntax_2area_Buff=0.005_5ppenalty_MIX2b_',
  #                        paste(splt,collapse = "_"),"_",
  #                        Sys.Date(),'.png')),
  #     height = 8.5, width = 11, unit = 'in', res = 600)
  # plot_radj(radj_kvar = kvar_radj_2areaAB, Fidx = 12:16)
  # dev.off()
  # 
  # 
  # png(here('figs',paste0('R_eq_SSsyntax_2area_Buff=0.005_5ppenalty_MIX2b_',
  #                        paste(splt,collapse = "_"),"_",
  #                        Sys.Date(),'.png')),
  #     height = 8.5, width = 11, unit = 'in', res = 600)
  # plot_radj(radj_kvar = kvar_radj_2area, Fidx = 12:16)
  # dev.off()
  
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
}

ggsave( Rmisc::multiplot(plotlist = master_plots, 
                         layout = matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)),
        file = here::here('figs', paste0("Yields_total_MIX2b_",Sys.Date(),'.png')),
        width = 10, height = 8, unit = 'in', dpi = 520)

ggsave( Rmisc::multiplot(plotlist = master_plots_area, 
                         layout = matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)),
        file = here::here('figs',paste0("Yields_area_MIX2b_",  Sys.Date(),'.png')),
        width = 10, height = 8, unit = 'in', dpi = 520)









