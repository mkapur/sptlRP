require(ggplot2, quietly = T)
require(reshape)
require(dplyr, quietly = T)
require(here)
require(ggsidekick)
require(stats4)
## settings
narea <- 3
nages <- 21
steep <- 0.44 ## global
recr_dist <- c(0.5,0.3,0.2) ## global recruits to areas
rec_level <- 100 ## reference recruitment level "R"
R0 <- 500 ## virgin recruitment

## load functions & initialize OM
lapply(list.files(here("R"), full.names = TRUE), source)

## Current SS Approach ----

## define virgin biomass
SB0 <- doNage( Fv = rep(0,narea), 
               X = X_ija,
               rdist = recr_dist,
               refR = rec_level)$SB_total

## Get NAA using movement. Input X_ija_NULL to turn movement OFF (smooth curve)
## applying system-wide F
current <- data.frame(Fv = NA, Yield = NA, B = NA)
Ftest <- seq(0,1,0.05)
for(v in 1:length(Ftest)){
  
  ## get values at present Fv
  curr <- doNage( Fv = rep(Ftest[v],narea), 
                  X = X_ija,
                  rdist = recr_dist,
                  refR = rec_level)

  # calc SPB/R and Yield/R
  SB_R <- curr$SB_total/rec_level
  Yield_R <- curr$Yield_total/rec_level
  
  #call Equil_Spawn_Recr_Fxn to get B_equil and R_equil from SPB/R and SR parms
  currEq <- Equil_Spawn_Recr_Fxn(steepness = steep, SSB_virgin = SB0, 
                                 Recr_virgin = R0, SPR_temp = SB_R)## L17247 ON TPL
  current[v,'Fv'] <- Ftest[v]
  current[v,'Yield'] <- Yield_R * currEq$R_equil
  current[v,'B'] <- SB_R*currEq$B_equil
}




## proposed approach ----

## applying system-wide F
Ftest <- seq(0,1,0.005)
proposed <- data.frame(Fv = NA, Yield = NA, B = NA)
proposed_i <- array(NA, dim = c(length(Ftest),3,narea), dimnames = list(NULL,c('Fv','Yield',"B"))) ## now for each area
B_eq_i <- R_eq_i <-SB_Ri <- Yield_Ri<- matrix(NA, nrow =length(Ftest), ncol = narea)

for(v in 1:length(Ftest)){

  for(k in 1:2){ ## Loop over steps A & B
    
    if(k == 1){
      rUse <- recr_dist
    } else{
      rUse <- recr_dist_adj
    }
    ## define virgin biomass by AREA
    SB0_i <- doNage( Fv = rep(0,narea), 
                     X = X_ija,
                     rdist = recr_dist,
                     refR = rec_level)$SB_i
    
    ## get values at present Fv
    prop <- doNage( Fv = rep(Ftest[v],narea), 
                    X = X_ija,
                    rdist = rUse,
                    refR = rec_level)
    
    # call Equ_Spawn_Recr_Fxn for each area to get B_equil and R_equil from SPB/R and SR parms
    for(i in 1:narea){ ## will overwrite second time
      # calc area-specific SPB/R and Yield/R, using area-specific R
      SB_Ri[v,i] <- prop$SB_i[i]/(rec_level*rUse[i])
      Yield_Ri[v,i] <- prop$Yield_i[i]/(rec_level*rUse[i])
      ## Calc recruits using area-specific SB etc
      propEq <- Equil_Spawn_Recr_Fxn(steepness = steep, SSB_virgin = SB0_i[i],
                                     Recr_virgin = R0*rUse[i], SPR_temp = SB_Ri[v,i])
      B_eq_i[v,i] <- propEq$B_equil
      R_eq_i[v,i] <- propEq$R_equil
      if(v ==75) cat(k," ",i," ",B_eq_i[v,i], "\n")
      # if(k == 2){ ## store quantities 
      #   proposed_i[v,'Fv',i] <- Ftest[v]
      #   proposed_i[v,'Yield',i] <-  Yield_Ri[v,i]*R_eq_i[v,i]
      #   proposed_i[v,'B',i] <-   SB_Ri[v,i]*B_eq_i[v,i]
      # } ## end k == 2 
    } ## end areas    
    # use ratio of B_equils among areas to calculate adjustment to recr_dist 
    if(k == 1) recr_dist_adj <- B_eq_i[v,]/sum(B_eq_i[v,]); next()
    # if(k == 1) recr_dist_adj <- B_eq_i[v,]/sum(B_eq_i[v,])*recr_dist; next()
    
  } ## end steps a/b loop

  proposed[v,'Fv'] <- Ftest[v]
  proposed[v,'Yield'] <- sum(Yield_Ri[v,] * R_eq_i[v,]) # sum(Yield_Ri[v,]) * sum(R_eq_i[v,])
  proposed[v,'B'] <-  sum(SB_Ri[v,] * B_eq_i[v,])  #sum(SB_Ri[v,]) * sum(B_eq_i[v,]) 
} ## end FV


## sanity check --
ggplot(current, aes(x = Fv, y = Yield)) + 
  geom_line(lwd = 1.1, aes(color = 'current')) + 
  geom_line(data = proposed, lwd = 1.1, aes(color = 'proposed')) + 
  theme_sleek()



ggplot(current, aes(x = B, y = Yield)) + 
  geom_line(lwd = 1.1, aes(color = 'current')) + 
  geom_line(data = proposed, lwd = 1.1, aes(color = 'proposed')) + 
  theme_sleek()
