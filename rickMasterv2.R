rm(list = ls())

## for running example
require(reshape)
require(dplyr, quietly = T)
require(here)
require(stats4)

## for plotting
require(patchwork)
require(ggsidekick)
require(ggplot2, quietly = T)

## settings ----
narea <- 3
nages <- 21
steep <- list(rep(0.26,3),
              rep(0.7,3),
              c(0.69,0.7,0.71))[[2]] ## global
recr_dist <- list(c(1,1,1),
                  c(0.5,0.3,0.2))[[1]] ## global recruits to areas

R0 <- c(420,330,250) ## each area has its own R0
rec_level <- R0 ## I suggest it should be the area-specific R0.
nominal_dist <- R0/sum(R0)


## load functions & initialize OM
lapply(list.files(here("R"), full.names = TRUE), source)

## Current SS Approach ----

## Get NAA using movement. Input X_ija_NULL to turn movement OFF (smooth curve)
## applying system-wide F
Ftest <- seq(0,1,0.005)
current <- data.frame(Fv = NA, Yield = NA, B = NA)
rick <- data.frame() ## storage for SRR question

for(v in 1:length(Ftest)){
  # cat(v,"\n")
  # for(k in 1:maxiter){
    # if(k == 1){
      rlevelUse = rec_level
    # } else{
    #   proposed_R_i <- currEq$R_equil*nominal_dist
    # #   ## overwrite those which are too high
    #   # proposed_R_i[which(proposed_R_i > R0)] <- rec_level[which(proposed_R_i > R0)]
    #   proposed_R_i[which(proposed_R_i < 1)] <- 1
    #   rlevelUse = proposed_R_i
    # #   # cat(rlevelUse,"\n")
    # #   # rm(proposed_R_i)
    # }
    ## define virgin biomass
    SB0 <- doNage( Fv = rep(0,narea), 
                   X = X_ija,
                   rdist = recr_dist,
                   refR = rlevelUse)$SB_total
    
    ## get values at present Fv
    curr <- doNage( Fv = rep(Ftest[v],narea), 
                    X = X_ija,
                    rdist = recr_dist, ## these are set to 1
                    refR = rlevelUse)
    
    # if(v > 115 & v < 135 & k == maxiter) cat(Ftest[v],rlevelUse,"\n")
    # calc SPB/R and Yield/R
    SB_R <- curr$SB_total/sum(rlevelUse)
    Yield_R <- curr$Yield_total/sum(rlevelUse)

    #call Equil_Spawn_Recr_Fxn to get B_equil and R_equil from SPB/R and SR parms
    currEq <- Equil_Spawn_Recr_Fxn(steepness = steep[1], SSB_virgin = SB0, 
                                   Recr_virgin = sum(R0), SPR_temp = SB_R)## L17247 ON TPL
    
    if(currEq$R_equil > sum(R0)) currEq$R_equil <- sum(R0) ## could alternatively use flattop BH
    current[v,'Fv'] <- Ftest[v]
    current[v,'Yield'] <- Yield_R * currEq$R_equil
    # if(v > 115 & v < 135 & k == maxiter) cat(Ftest[v],  current[v,'Yield'],"\n")
    current[v,'B'] <- SB_R* currEq$R_equil ## the same as currEq$B_equil
    rick[v,'R_ESUMB'] <- currEq$R_equil ## expected recruits given sum biomass in area
    rick[v,'SBeqtotal2'] <- currEq$B_equil ## expected recruits given sum biomass in area
    
  # } ## end iters
} ## end F

## proposed approach ----

## applying system-wide F
maxiter = 101
proposed <- data.frame(Fv = NA, Yield = NA, B = NA)
proposed_i <- array(NA, dim = c(length(Ftest),3,narea), dimnames = list(NULL,c('Fv','Yield',"B"))) ## now for each area
B_eq_i <- R_eq_i <- B_eq_i_INIT <- R_eq_i_INIT <- SB_Ri <- Yield_Ri<- matrix(NA, nrow =length(Ftest), ncol = narea)
radj <- array(NA, dim = c(maxiter,length(Ftest),3)) ## keeping track of convergence

# for(v in 110:160){
for(v in 1:length(Ftest)){
  # if(v %% 10 == 0) cat(v,"\n")
  for(k in 1:maxiter){ ## Loop over steps A & B
    
    if(k == 1){
      rdistUse <- recr_dist ## no distribution now; full rec-level in each area
      rlevelUse = rec_level ## pre-specified No recruits in area, currently R0
    } else{
      rdistUse <- recr_dist ## only after computing R_i
      rlevelUse =   list(rec_level, 
                       rick[v,'R_ESUMB'],
                       rick[v,'R_SUMEBA'],
                       R_eq_i[v,],
                       R_eq_i[v,]*B_eq_i[v,]/sum(B_eq_i[v,]),
                       apply(rbind( R_eq_i[v,],radj[k-1,v,] ),2,mean),
                       rec_level*B_eq_i[v,]/sum(B_eq_i[v,]),
                       sum(R_eq_i[v,])*nominal_dist)[[4]] 
      
      
      
    }
    # radj[k,v,] <- rlevelUse
    # if(k == 10  |k == 9 ) cat(v,"\t",k,"\t",paste(round(rlevelUse)),"\n") ## monitoring if this changes

    ## define virgin biomass by AREA
    SB0_i <- doNage(Fv = rep(0,narea), 
                     X = X_ija,
                     rdist = rdistUse,
                     refR = list(rec_level,rlevelUse)[[2]])$SB_i
    
    ## get values at present Fv
    # In each iteration, calculate the SSB and Yield that 
    # comes from those recruits, taking movement into account
    prop <- doNage( Fv = rep(Ftest[v],narea), 
                    X = X_ija,
                    rdist = rdistUse,
                    refR = rlevelUse) 
    # radj[k,v,] <- rlevelUse
    # call Equ_Spawn_Recr_Fxn for each area to get B_equil and R_equil from SPB/R and SR parms
    for(i in 1:narea){ ## will overwrite second time
      # calc area-specific SPB/R and Yield/R, using area-specific R
      # if(length(rlevelUse) > 1 & k > 1){
      if( k > 1){
        rleveltmp = list(rlevelUse[i],
                         min(rlevelUse[i],R0[i]),
                         max(rlevelUse[i],R0[i]),
                         mean(c(rlevelUse[i],radj[k-1,v,i] )),
                         max(rlevelUse[i],1))[[1]]
      #  if(rlevelUse[i] > R0[i]){
      #    rleveltmp = R0[i]
      #  } else if(rlevelUse[i] <1){
      #    rleveltmp = 1
      #  }
      } else{
        rleveltmp = rlevelUse[i]
      }
      # if(k %% 10 == 0 | k %% 3 == 0) cat(v, k,i,round(rleveltmp),"\n")
      # cat(v, k,i,rlevelUse[i],radj[k-1,v,i] ,"\n")
      cat(v, k,i,round(rleveltmp),"\n")
      radj[k,v,i] <- rleveltmp
      
      # if(round(prop$Yield_i[i],3) == 0) prop$Yield_i[i] <- 0  
      # prop$Yield_i[i] <- round(prop$Yield_i[i],3) ## avoid div by small numbers and get weirdstuff
      # prop$SB_i[i] <- round(prop$SB_i[i],3) ## avoid div by small numbers and get weirdstuff
      
      
      SB_Ri[v,i] <- prop$SB_i[i]/(rleveltmp*rdistUse[i]) ## on k = 1 will just be rleveltemp
      Yield_Ri[v,i] <- prop$Yield_i[i]/(rleveltmp*rdistUse[i])
      
      # if(k %% 10 == 0 ) cat(v, k,i,  Yield_Ri[v,i],"\n")
      
      ## Calc area-specific recruits using area-specific SB etc
      propEq <- Equil_Spawn_Recr_Fxn(steepness = steep[i], SSB_virgin = SB0_i[i],
                                     Recr_virgin = R0[i], SPR_temp = SB_Ri[v,i])
      B_eq_i[v,i] <- propEq$B_equil
      R_eq_i[v,i] <- propEq$R_equil ## gets overwritten each iteration

      if(k == maxiter){ ## store quantities
        proposed_i[v,'Fv',i] <- Ftest[v]
        proposed_i[v,'Yield',i] <-  Yield_Ri[v,i]*R_eq_i[v,i]
        proposed_i[v,'B',i] <-    SB_Ri[v,i] *R_eq_i[v,i]
      } ## end k max
    } ## end areas    
    cat(v, k,i, Yield_Ri[v,i]*R_eq_i[v,i],"\n")
    if(k == maxiter){ ## store quantities
      ## storing info, not currently used
      rick[v,"Fv"] <- Ftest[v]
      rick[v,"SBeqtotal"] <-   sum(B_eq_i[v,] )
      ## sum of expected recruits in areas
      rick[v,"R_SUMEBA"]  <- sum( R_eq_i[v,])
    }
  } ## end k:maxiter
  proposed[v,'Fv'] <- Ftest[v]
  proposed[v,'Yield'] <-   sum(proposed_i[v,'Yield',])# sum(Yield_Ri[v,] * R_eq_i[v,]) # sum(Yield_Ri[v,]) * sum(R_eq_i[v,])
  proposed[v,'B'] <-  sum(proposed_i[v,'B',])#sum(SB_Ri[v,] *  R_eq_i[v,i])  #sum(SB_Ri[v,]) * sum(B_eq_i[v,]) 
} ## end FV

