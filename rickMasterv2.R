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

## settings ----
narea <- 3
nages <- 21
steep <- rep(0.7,3)
recr_dist <- c(1,1,1) ## global recruits to areas

R0_list <- list(c(420,330,250),
                rev(c(420,330,250)),
                c(333,333,333),
                c(499,499,2)) #,
                # c(2,2,499),
                # c(420,330,1000))
## each area has its own R0
# rec_level <- R0 ## I suggest it should be the area-specific R0.
# nominal_dist <- R0/sum(R0)


## load functions & initialize OM
lapply(list.files(here("R"), full.names = TRUE), source)

## TRIAL 1: Comparing approaches at various RRef levels ----


## Current SS Approach ----

## Get NAA using movement. Input X_ija_NULL to turn movement OFF (smooth curve)
## applying system-wide F
Ftest <- seq(0,1,0.05)
# current <- data.frame(Fv = NA, Yield = NA, B = NA)

rRef_current <- array(NA, dim = c(length(Ftest),3,length(R0_list)))

for(RR in 1:dim(rRef_current)[3]){
  rec_level <- R0 <- R0_list[[RR]]
  current <- data.frame(Fv = NA, Yield = NA, B = NA)
  rick <- data.frame() ## storage for SRR question
  
  for(v in 1:length(Ftest)){
    rlevelUse = rec_level
    ## define virgin biomass
    SB0 <- doNage( Fv = rep(0,narea), 
                   X = X_ija,
                   rdist = recr_dist,
                   refR = rlevelUse)$SB_total
    
    ## get values at present Fv
    curr <- doNage( Fv =  rep(Ftest[v],narea), #c(rep(Ftest[v],2),0), #
                    X = X_ija,
                    rdist = recr_dist, ## these are set to 1
                    refR = rlevelUse)
    
    # calc SPB/R and Yield/R
    SB_R <- curr$SB_total/sum(rlevelUse)
    Yield_R <- curr$Yield_total/sum(rlevelUse)
    
    #call Equil_Spawn_Recr_Fxn to get B_equil and R_equil from SPB/R and SR parms
    currEq <- Equil_Spawn_Recr_Fxn(steepness = steep[1], SSB_virgin = SB0, 
                                   Recr_virgin = sum(R0), SPR_temp = SB_R)## L17247 ON TPL
    
    if(currEq$R_equil > sum(R0)) currEq$R_equil <- sum(R0) ## could alternatively use flattop BH
    current[v,'Fv'] <- Ftest[v]
    current[v,'Yield'] <- Yield_R * currEq$R_equil
    current[v,'B'] <- SB_R* currEq$R_equil ## the same as currEq$B_equil
    rick[v,'R_ESUMB'] <- currEq$R_equil ## expected recruits given sum biomass in area
    rick[v,'SBeqtotal2'] <- currEq$B_equil ## expected recruits given sum biomass in area
    
    # } ## end iters
  } ## end F
  
  rRef_current[,,RR] <- as.matrix(current)
} ## end RefR

## proposed approach ----

## applying system-wide F
maxiter = 101
rRef_proposed <- array(NA, dim = c(length(Ftest),3,length(R0_list)))
rRef_proposed_radj <- rRef_proposed_sbpr <- rRef_proposed_SBi <- array(NA, dim = c(maxiter,length(Ftest),narea,length(R0_list)))
rRef_proposed_i <- array(NA, dim = c(length(Ftest),3,narea,length(R0_list)))

for(RR in 1:dim(rRef_proposed)[3]){
  rec_level <- R0 <- R0_list[[RR]]
  proposed <- data.frame(Fv = NA, Yield = NA, B = NA)
  proposed_i <- array(NA, dim = c(length(Ftest),3,narea), dimnames = list(NULL,c('Fv','Yield',"B"))) ## now for each area  ## define virgin biomass by AREA, does not change
  B_eq_i <- R_eq_i <- B_eq_i_INIT <- R_eq_i_INIT <- SB_Ri <- Yield_Ri <- matrix(NA, nrow =length(Ftest), ncol = narea)
  radj <- array(NA, dim = c(maxiter,length(Ftest),narea)) ## keeping track of convergence
  
  SB0_i <- doNage(Fv = rep(0,narea), 
                  X = X_ija,
                  rdist = recr_dist,
                  refR = rec_level)$SB_i
  
  for(v in 1:length(Ftest)){
    for(k in 1:maxiter){ ## Loop over steps A & B
      
      if(k == 1){
        rdistUse <- recr_dist ## no distribution now; full rec-level in each area
        rlevelUse = rec_level ## pre-specified No recruits in area, currently R0
        # SB_Ri3LAST  <- rec_level[3] ## not yet overwritten
      } else{
        rdistUse <- recr_dist ## only after computing R_i
        rlevelUse =  R_eq_i[v,]# c(R_eq_i[v,1:2], max(1,round(R_eq_i[v,3],0)))
        # SB_Ri3LAST  <- SB_Ri[v,3] ## not yet overwritten
      }
      cat("inreq \t",v,k,i, rlevelUse, "\n")
      ## get values at present Fv
      # In each iteration, calculate the SSB and Yield that 
      # comes from those recruits, taking movement into account
      prop <- doNage( Fv = rep(Ftest[v],narea), #c(rep(Ftest[v],2),0), #
                      X = X_ija,
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
        rRef_proposed_radj[k,v,i,RR] <- rleveltmp
        # SB_Ri[v,i] <- prop$SB_i[i]/(rleveltmp*rdistUse[i])## on k = 1 will just be rleveltemp
   
        
        # SB_Ri[v,i] <- prop$SB_i[i]/(rleveltmp*rdistUse[i]) ## on k = 1 will just be rleveltemp
        SB_Ri[v,i] <-  prop$SB_i[i]/((rleveltmp+0.01*R0[i])*rdistUse[i]) ## Rick's idea
        # if( SB_Ri[v,i] > SB0_i[i]) SB_Ri[v,i] <- SB0_i[i] ## penalty for dividing small numbers
        
        # if(k > 2){
          
          # SB_Ri[v,3] <- mean(rRef_proposed_sbpr[k-1,v,3,RR],
          #                   prop$SB_i[i]/(rleveltmp*rdistUse[i]))
          # SB_Ri[v,3] <- min(rRef_proposed_sbpr[k-1,v,3,RR],
          #                   prop$SB_i[i]/(rleveltmp*rdistUse[i])) ## on k = 1 will just be rleveltemp
        # }
        
        Yield_Ri[v,i] <- prop$Yield_i[i]/(rleveltmp*rdistUse[i])
        
        rRef_proposed_SBi[k,v,i,RR] <-  prop$SB_i[i]
        rRef_proposed_sbpr[k,v,i,RR] <- SB_Ri[v,i]
        
     
        # cat(v, k,i,    prop$SB_i[i],SB_Ri[v,i],"\n")
        ## Calc area-specific recruits using area-specific SB etc
        propEq <- Equil_Spawn_Recr_Fxn(steepness = steep[i], SSB_virgin = SB0_i[i],
                                       Recr_virgin = R0[i], SPR_temp = SB_Ri[v,i])
        
        B_eq_i[v,i] <- propEq$B_equil
        R_eq_i[v,i] <- propEq$R_equil ## gets overwritten each iteration
        
        if(k == maxiter){ ## store quantities
          proposed_i[v,'Fv',i] <- Ftest[v]
          proposed_i[v,'Yield',i] <-  Yield_Ri[v,i]*R_eq_i[v,i]
          proposed_i[v,'B',i] <-    SB_Ri[v,i] *R_eq_i[v,i]
          # cat("B \t",v,k,i,  proposed_i[v,'B',i], "\n")
          cat("Yield \t",v,k,i,  proposed_i[v,'B',i], "\n")
          
        } ## end k max
      } ## end areas
      # cat("SB_i \t",v,k,i, prop$SB_i, "\n")
      # cat("SBPR \t",v,k,i, SB_Ri[v,], "\n")
      # cat("NEWREQ \t",v,k,i, R_eq_i[v,], "\n")
      # cat("SB_i_ratio \t",v,k,i, prop$SB_i/sum(prop$SB_i), "\n")
      # cat("NewREQ_ratio \t",v,k,i, R_eq_i[v,]/sum(R_eq_i[v,]), "\n")
      # cat("SBPR_ratio \t",v,k,i, SB_Ri[v,]/sum(SB_Ri[v,]), "\n")
      
      if(k == maxiter){ ## store quantities
        ## storing info, not currently used
        rick[v,"Fv"] <- Ftest[v]
        rick[v,"SBeqtotal"] <-   sum(B_eq_i[v,] )
        ## sum of expected recruits in areas
        rick[v,"R_SUMEBA"]  <- sum( R_eq_i[v,])
      }
    } ## end k:maxiter
    ## save totals from final iteration
    proposed[v,'Fv'] <- Ftest[v]
    proposed[v,'Yield'] <-   sum(proposed_i[v,'Yield',])
    proposed[v,'B'] <-  sum(proposed_i[v,'B',])

    
  } ## end FV

  rRef_proposed[,,RR] <- as.matrix(proposed)
  rRef_proposed_i[,,,RR] <- proposed_i
} ## end RR

## do the oscillations start whenever B_sink < B_source? yes
# ss <- data.frame(RR = 1:4, Fv_obs= c(0.55,0.6,0.55,NA), FV_SLTS = NA)
# for(i in 1:4){
#   ss$RR <- i
# sinkLTsource <- which(rRef_proposed_i[,3,3,i] < rRef_proposed_i[,3,2,i] &
#         rRef_proposed_i[,3,3,i] < rRef_proposed_i[,3,1,i] )[1]
# Ftest[sinkLTsource]
# ss$FV_SLTS[i] <- Ftest[sinkLTsource]
# }

## TRIAL 2: Optimizing F ----

## functions to optimize ----
dfx.dxSYS <- function(Fv_test,RLI ){
  y1 <- optim_loop(Fv_i = rep(Fv_test-0.001,narea), rec_level_idx = RLI)$Yield
  y2 <- optim_loop(Fv_i = rep(Fv_test+0.001,narea), rec_level_idx = RLI)$Yield
  appx <- (y2-y1)/(0.002) #0.002 is total X delta; we are using system yield
  return(appx)
}


dfx.dxSYS_curr <- function(Fv_test,RLI ){
  y1 <- run_one_current(Fv_i = rep(Fv_test-0.001,narea), rec_level_idx = RLI)$Yield
  y2 <- run_one_current(Fv_i = rep(Fv_test+0.001,narea), rec_level_idx = RLI)$Yield
  appx <- (y2-y1)/(0.002) #0.002 is total X delta; we are using system yield
  return(appx)
}


## 2A F sys @ both approaches ----
sysopt <- data.frame('RR' = NA, 'FsysMSY' = NA, 'BsysMSY' = NA, 'YsysMSY' = NA,
                     'YA1' = NA, 'YA2' = NA, 'YA3' = NA, 
                     'BA1' = NA, 'BA2' = NA, 'BA3'= NA )
sysopt_curr <- data.frame('RR' = NA, 'FsysMSY' = NA, 'BsysMSY' = NA, 'YsysMSY' = NA)
for(RR in 1:4){
  
  # ## PROPOSED
  sysopt[RR,'RR'] <-  sysopt_curr[RR,'RR'] <- RR
  sysopt[RR,'FsysMSY'] <- as.numeric(uniroot(f = dfx.dxSYS,
                   RLI = 1,
                   interval = c(0.02,1))[1])

  run_at_msy <- optim_loop(Fv_i = rep(  sysopt[RR,'FsysMSY'],narea), rec_level_idx = RR)
  sysopt[RR,'YsysMSY'] <- run_at_msy$Yield
  sysopt[RR,'BsysMSY'] <- run_at_msy$Biomass

  sysopt[RR,5:7] <- run_at_msy$Yield_i
  sysopt[RR,8:10] <- run_at_msy$Biomass_i
  
  ## current
  sysopt_curr[RR,'FsysMSY'] <- as.numeric(uniroot(f = dfx.dxSYS_curr, 
                                     RLI = RR,
                                     interval = c(0.02,1))[1])
  curr_at_FMSY <- run_one_current(Fv_i = rep(  sysopt_curr[RR,'FsysMSY'],narea), rec_level_idx = RR)
  
  sysopt_curr[RR,'YsysMSY'] <- curr_at_FMSY$Yield
  sysopt_curr[RR,'BsysMSY'] <- curr_at_FMSY$B

  
  


}

save(sysopt,file = here("sys_optimize_proposed_21iter.Rdata"))
save(sysopt_curr,file = here("sys_optimize_current_21iter.Rdata"))

## 2B F_i @ proposed ----

minFunc <- function(F1,F2,F3,RLI){
  minus <- as.numeric(c(F1 - 0.001, F2 - 0.001, F3 - 0.001))
  plus <- as.numeric(c(F1 +0.001, F2 + 0.001, F3 + 0.001))
  y1 <- optim_loop(Fv_i = minus, rec_level_idx = RLI)$Yield
  y2 <- optim_loop(Fv_i = plus, rec_level_idx = RLI)$Yield
  appx <- abs(y2-y1)/(0.002) ## system yield again
  return(appx)
}


areaopt <- data.frame('RR' = NA, 
                      'F1' = NA, 'F2' = NA, 'F3' = NA,
                     'YA1' = NA, 'YA2' = NA, 'YA3' = NA, 
                     'BA1' = NA, 'BA2' = NA, 'BA3'= NA,
                     'CURRENT_YIELD')
for(RR in 1:4){
  areaopt[RR,'RR'] <- RR
  FVTEMP <-  coef(mle(minFunc,
                      start = list(F1 = 0.025, F2 = 0.025, F3 = 0.025),
                      method = "L-BFGS-B",
                      fixed = list( RLI = RR), ## subsetting eq method
                      lower = c(0.02, 0.02,0.02), upper = c(0.4,0.4,0.4)))[1:3]

  run_at_msy <- optim_loop(Fv_i = FVTEMP, rec_level_idx = RR)
  areaopt[RR,2:4] <- FVTEMP

  areaopt[RR,5:7] <- round(run_at_msy$Yield_i,2)
  areaopt[RR,8:10] <- round(run_at_msy$Biomass_i,2)

  
  curr_at_ftemp <- run_one_current(Fv_i =   as.numeric(areaopt[1,2:4]), rec_level_idx = RR)
  areaopt[RR, 'CURRENT_YIELD'] <- curr_at_ftemp$Yield
}

save(areaopt,file = here("area_optimize_proposed_21iter.Rdata"))


## TRIAL 3: Eigen 2 Area ----
rm(list = ls())
narea <- 2
nages <- 21
steep <- rep(0.7,3)
recr_dist <- c(1,1) ## global recruits to areas


R0_list <- list(c(500,500),
                rev(c(700,300)),
                c(998,2))



lapply(list.files(here("R"), full.names = TRUE), source)
maxiter =  2 ## set to 1 to only use eigen
Ftest <- seq(0,1,0.05)

## loop system wide F
sys2area <- array(NA, dim = c(length(Ftest),3,length(R0_list),3)) ## F x 3 x RR x Movemats
radj2area <- array(NA, dim = c(maxiter,length(Ftest),narea+1,length(R0_list),3)) ## iters, Fv, 2 areas , RR x movements




for(m in 1:2){ #1:length(list(X_ija_EQUAL, X_ija_MIX2))){
  for(RR in 1:length(R0_list)){
    
    eigv <- eigen(list(X_ija_EQUAL[,,1], X_ija_MIX2[,,1], X_ija_UNI2[,,1])[[m]])$vectors * sqrt(2)
    receq = abs(diag(eigv) * R0_list[[RR]]) #eigv*R0_list[[RR]]

    for(Fv in 1:length(Ftest)){
      curr <- run_one_current(Fv_i = rep(Ftest[Fv],narea), 
                              rec_level_idx = RR, 
                              recr_dist= recr_dist, 
                              movemat = list(X_ija_EQUAL, X_ija_MIX2)[[m]])
      cat(curr$Yield,"\n")
      prop <- optim_loop(Fv_i = rep(Ftest[Fv],narea), 
                         rec_level_idx = RR, 
                         receq= receq,
                         recr_dist= recr_dist, movemat = list(X_ija_EQUAL, X_ija_MIX2,X_ija_UNI2)[[m]])
      sys2area[Fv,1,RR,m] <- Ftest[Fv]
      sys2area[Fv,2,RR,m] <- curr$Yield
      sys2area[Fv,3,RR,m] <- prop$Yield
      
      radj2area[,Fv,2:3,RR,m] <- prop$radj
      radj2area[,Fv,1,RR,m] <- Ftest[Fv]
      
      cat(m,"\t",RR,"\t",Fv,"\n")
    } ## end Fs
  } ## end input rec levels
} ## end movement approaches

## plot yield curves
plist2a <- barlist <- plist2a2 <- list();idx = 1
for(m in 1:2){
  for(RR in 1:length(R0_list)){
    tmp <- data.frame(sys2area[,,RR,m])
    names(tmp) <- c('Fv','current','proposed')
    plist2a[[idx]] <- tmp %>% melt(id = 'Fv') %>%
      ggplot(., aes(x = Fv, y = value, color = variable, linetype = variable)) +
      geom_line(lwd = 1.1) +
      scale_color_manual(values = c('seagreen','goldenrod'), labels = c('current','proposed')) +
      scale_linetype_manual(values = c('solid','dashed'), labels = c('current','proposed'), guide=FALSE ) +
      labs(x = 'F', y = ifelse(RR == 1, 'Yield',""), color = "",
           title = paste0(c('Equal (symmetric) movement','Asymmetric mixture movement')[m])) +
      scale_y_continuous(limits = c(0,120), breaks = seq(0,130,10)) +
      theme_sleek() + 
      theme(legend.position = if(RR<3) 'none' else c(0.75,0.5))
    
    barlist[[ifelse(idx >3, idx-3,idx)]] <- melt(data.frame(R0_list[[RR]])) %>%
      mutate(Area = 1:2) %>%
      ggplot(., aes(x = Area, y = value, fill = factor(Area))) +
      geom_histogram(stat = 'identity',
                     boundary = 0)+
      scale_fill_grey()  +
      annotate('text', x = 1:2, y = 200,
               label = paste('Area',1:2),
               color = c("white","grey22"), 
               size = 3) +
      scale_y_continuous(limits = c(0,1100)) +
      theme_void()+
      theme(legend.position = 'none')
    
    
    idx = idx+1
  }
}

for(RR in 1:length(plist2a)){
  plist2a2[[RR]] <- ggdraw() +
    draw_plot(plist2a[[RR]]) +
    draw_plot(barlist[[ifelse(RR >3, RR-3, RR)]]+
                annotate('text', x = 1.5, 
                         y = 1100, 
                         cex = 2,
                         label = 'Initial Recruitment'), 
              x = 0.7, y = 0.15,
              width = .25, height = .25)
}
Rmisc::multiplot(plotlist = plist2a2,  cols = 3, 
                 layout = rbind(c(1,2,3),c(4,5,6)))


ggsave(Rmisc::multiplot(plotlist = plist3a2,  cols = 4, 
                        layout = rbind(c(1,2,3,4),c(5,6,7,8))),
       file = here('figs','Yield_comparisons_2area_eigen.png'),
       width = 10, height = 8, unit = 'in', dpi = 520)

## TRIAL 4: Eigen for 3 area model ----
rm(list = ls())
narea <- 3
nages <- 21
steep <- rep(0.7,3)
recr_dist <- c(1,1,1) ## global recruits to areas

R0_list <- list(c(420,330,250),
                rev(c(420,330,250)),
                c(333,333,333),
                c(499,499,2))

lapply(list.files(here("R"), full.names = TRUE), source)
maxiter =  2 ## set to 1 to only use eigen
Ftest <- seq(0,1,0.05)

## loop system wide F
sys3area <- array(NA, dim = c(length(Ftest),3,length(R0_list),2)) ## F x 3 x RR x Movemats
radj3area <- array(NA, dim = c(maxiter,length(Ftest),narea+1,length(R0_list),2)) ## iters, Fv, 2 areas , RR x movements

for(m in 1:2){ #1:length(list(X_ija_EQUAL, X_ija_MIX2))){
  for(RR in 1:length(R0_list)){
    
    if(m == 2){
      ## for mixing, do as before
      eigv <- eigen(list(X_ija[,,1], X_ija_MIX[,,1])[[m]])$vectors * sqrt(2)
      # abs(diag(eigv) * R0_list[[RR]])
      receq = abs(diag(eigv) * R0_list[[RR]]) #eigv*R0_list[[RR]]
    } else if(m == 1){
      ## unidirectional movement
      eigv <- eigen(list(X_ija[,,1], X_ija_MIX[,,1])[[m]])$values
      # eigv * R0_list[[RR]] 
      receq = eigv * R0_list[[RR]] #eigv*R0_list[[RR]]
    }

      cat(m, RR, receq,"\n")
    # } else{
    #   receq = R0_list[[RR]]
    # }
    
    for(Fv in 1:length(Ftest)){
      curr <- run_one_current(Fv_i = rep(Ftest[Fv],narea), 
                              rec_level_idx = RR, 
                              recr_dist= recr_dist, 
                              movemat = list(X_ija, X_ija_MIX)[[m]])
      cat(curr$Yield,"\n")
      prop <- optim_loop(Fv_i = rep(Ftest[Fv],narea), 
                         rec_level_idx = RR, 
                         receq= receq,
                         recr_dist= recr_dist, movemat = list(X_ija, X_ija_MIX)[[m]])
      
      sys3area[Fv,1,RR,m] <- Ftest[Fv]
      sys3area[Fv,2,RR,m] <- curr$Yield
      sys3area[Fv,3,RR,m] <- prop$Yield
      
      radj3area[,Fv,2:4,RR,m] <- prop$radj
      radj3area[,Fv,1,RR,m] <- Ftest[Fv]
      
      # cat(m,"\t",RR,"\t",Fv,"\n")
    } ## end Fs
  } ## end input rec levels
} ## end movement approaches

## plot radjf
for(m in 1:1){#dim(radj3area)[5]){
  # png(here('figs',paste0('R_eq_iterations_3area_',c('EQUAL','MIX')[m],'.png')),
  #          height = 8.5, width = 11, unit = 'in', res = 600)

      plotseq = c(11:13)
      par(mfrow = c(dim(radj3area)[4],
                    length(plotseq)),
          mar = c(5,5,1.5,1.5))
      radj3areaTMP <- radj3area[,,,,m]
      for(i in 1:dim(radj3area)[4]){
        
        # radj <- radj3areaTMP[,,,i]
        radj <- radj3areaTMP[,,,i]
        
        for(j in plotseq){
          if(i == dim(radj3area)[4] & j == max(plotseq)) next()
          plot(radj[,j,2], col = 'black', 
               type = 'l',
               ylim = c(0,1000),
               xlab = '',
               ylab =  'R_eq')
          text(x =1.5, y = 800,
               cex = 1.5, label = paste0('F = ',Ftest[j]))
          ## niter x fv x areas
          
          lines(radj[,j,3], col = 'blue', type = 'l',pch = 19, cex = 1)
          lines(radj[,j,4], col = 'red', type = 'l',pch = 19, cex = 1)
          
          
          ## add iterated values (terminal)
          points(2,rRef_proposed_radj[101,j,1,i], pch = 19, col = 'black')
          points(2,rRef_proposed_radj[101,j,2,i], pch = 19, col = 'blue')
          points(2,rRef_proposed_radj[101,j,3,i], pch = 19, col = 'red')
          
          
          ## add R0/starts
          # points(R0_list[[i]][1], pch = 5, col = 'black')
          # points(R0_list[[i]][2], pch = 19, col = 'blue')
          # points(R0_list[[i]][3], pch = 19, col = 'red')
        } ## end j (Fs)
      } ## end loop over RRs
      plot.new()
      legend('center',
             col = c('black','blue','red'), 
             legend = c(paste('Area',1:3,'R_eq from Eigen'),
                        paste('Area',1:3,'R_eq from Iter')),
             lty = c(1,1,1,NA,NA,NA),
             pch = c(NA, NA,NA,19,19,19),
             cex = 0.75)
      text(x = 0.5, y= 1, adj = 1,  labels = c('EQUAL','MIX')[m])
      
      # dev.off()
} ##end loop over movement mats





## plot yield curves
plist3a <- barlist <- plist3a2 <- list();idx = 1
for(m in 1:2){
  for(RR in 1:length(R0_list)){
    tmp <- data.frame(sys3area[,,RR,m])
    names(tmp) <- c('Fv','current','proposed')
    plist3a[[idx]] <- tmp %>% melt(id = 'Fv') %>%
    ggplot(., aes(x = Fv, y = value, color = variable, linetype = variable)) +
      geom_line(lwd = 1.1) +
      scale_color_manual(values = c('seagreen','goldenrod'), labels = c('current','proposed')) +
      scale_linetype_manual(values = c('solid','dashed'), labels = c('current','proposed'), guide=FALSE ) +
      labs(x = 'F', y = ifelse(RR == 1, 'Yield',""), color = "",
           title = paste0(c('Unidirectional (source-sink) movement',
                            'Asymmetric mixture movement')[m])) +
      scale_y_continuous(limits = c(0,120), breaks = seq(0,130,10)) +
      theme_sleek() + 
      theme(legend.position = if(RR<3) 'none' else c(0.75,0.5))
    
    barlist[[ifelse(idx >4, idx-4,idx)]] <- melt(data.frame(R0_list[[RR]])) %>%
      mutate(Area = 1:3) %>%
      ggplot(., aes(x = Area, y = value, fill = factor(Area))) +
      geom_histogram(stat = 'identity',
                     boundary = 0)+
      scale_fill_grey()  +
      annotate('text', x = 1:3, y = 200,
               label = paste('Area',1:3),
               color = c("white","grey88","grey22"), 
               size = 2) +
      scale_y_continuous(limits = c(0,550)) +
      theme_void()+
      theme(legend.position = 'none')
    
    
    idx = idx+1
  }
}

for(RR in 1:length(plist3a)){
  plist3a2[[RR]] <- ggdraw() +
    draw_plot(plist3a[[RR]]) +
    draw_plot(barlist[[ifelse(RR >4, RR-4, RR)]]+
                annotate('text', x = 2, 
                         y = 525, 
                         cex = 2,
                         label = 'Initial Recruitment'), 
              x = 0.7, y = 0.15,
              width = .25, height = .25)
}
Rmisc::multiplot(plotlist = plist3a2,  cols = 4, 
                 layout = rbind(c(1,2,3,4),c(5,6,7,8)))


ggsave(Rmisc::multiplot(plotlist = plist3a2,  cols = 4, 
                        layout = rbind(c(1,2,3,4),c(5,6,7,8))),
       file = here('figs','Yield_comparisons_3area_eigen.png'),
       width = 10, height = 8, unit = 'in', dpi = 520)
