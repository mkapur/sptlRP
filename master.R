## Code to replicate Kapur et al (202X)
## Spatial reference points for next-gen assessment models

require(dplyr)
require(here)
require(ggplot2);require(ggsidekick);require(patchwork)
require(reshape2)

source(here("R","fnxs.R"))

## settings, unchanged 
R0_global <- 4
Rprop_input <- 0.65
steep = 0.75
## build datasets to spec (will autosave figure)
dat <- makeDat(wa = c(5,5),
               fec_a50 = c(6,6),
               fec_a95 = c(12,12),
               slx_a50 = c(9,9),
               slx_a95 = c(13,13),
               pStay = c(0.9,0.6))



FFs <- expand.grid(seq(0,1,0.05),seq(0,1,0.05))

## apply new method
out <- array(NA, dim = c(nrow(FFs),14,2), 
             dimnames = list(c(rep(NA, nrow(FFs))),c("FF_Area1","FF_Area2",
                                     "estRbar","estRprop",
                                     "Yield_A1","Yield_A2",
                                     "SB_A1","SB_A2",
                                     "SB0_A1","SB0_A2",
                                     "expR_A1","expR_A2",
                                     "obsR_A1","obsR_A2"),
                             c('new','old'))) ## each slice is old or new


for(i in 1:nrow(FFs)){
  out[i,'FF_Area1',1] <- FFs[i,1];   out[i,'FF_Area2',1] <- FFs[i,2]
  
  ## this is the new method; old method uses global inputs
  opt0 <- optim_loop(FFs,i) 
  opt_temp <- opt0$opt_temp; tmp0 <- opt0$tmp0; tmp <- opt0$tmp
  out[i,'estRbar',1] <- opt_temp$par[1];  out[i,'estRprop',1] <- opt_temp$par[2];
  out[i,'estRbar',2] <- R0_global;  out[i,'estRprop',2] <- Rprop_input
  
  ## derived quants at optimized value
  yields <- as.numeric(getYield(passR = out[i,'estRbar',1], passRprop =   out[i,'estRprop',1], YPR_F = tmp$YPR))
  out[i,'Yield_A1',1] <- yields[1];  out[i,'Yield_A2',1] <- yields[2];
  
  sbs <- as.numeric(getSB(passR = out[i,'estRbar',1], passRprop = out[i,'estRprop',1], SBPR_F = tmp$SBPR))
  out[i,'SB_A1',1] <- sbs[1];  out[i,'SB_A2',1] <- sbs[2];
  
  sb0 <- as.numeric(getSB(passR = out[i,'estRbar',1], passRprop = out[i,'estRprop',1], SBPR_F = tmp0$SBPR))
  out[i,'SB0_A1',1] <- sb0[1];  out[i,'SB0_A2',1] <- sb0[2];
  
  rexp <- as.numeric(getExpR(passR = out[i,'estRbar',1], passRprop =   out[i,'estRprop',1],SB_F = sbs, SB_0 =sb0))
  out[i,'expR_A1',1] <- rexp[1];  out[i,'expR_A2',1] <- rexp[2];
  
  obsr <- as.numeric(out[i,'estRbar',1]*c(out[i,'estRprop'],1-out[i,'estRprop',1]))
  out[i,'obsR_A1',1] <- obsr[1];  out[i,'obsR_A2',1] <- obsr[2];
  
  ## derived quants at global value ("current method")
  yields <- as.numeric(getYield(passR = out[i,'Rbar',2], passRprop =   out[i,'Rprop',2], YPR_F = tmp$YPR))
  out[i,'Yield_A1',2] <- yields[1];  out[i,'Yield_A2',2] <- yields[2];
  
  sbs <- as.numeric(getSB(passR = out[i,'Rbar',2], passRprop =   out[i,'Rprop',2], SBPR_F = tmp$SBPR))
  out[i,'SB_A1',2] <- sbs[1];  out[i,'SB_A2',2] <- sbs[2];
  
  sb0 <- as.numeric(getSB(passR = out[i,'Rbar',2], passRprop =   out[i,'Rprop',2],SBPR_F = tmp0$SBPR))
  out[i,'SB0_A1',2] <- sb0[1];  out[i,'SB0_A2',2] <- sb0[2];
  
  rexp <- as.numeric(getExpR(passR = out[i,'Rbar',2], passRprop =   out[i,'Rprop',2],SB_F = sbs, SB_0 =sb0))
  out[i,'expR_A1',2] <- rexp[1];  out[i,'expR_A2',2] <- rexp[2];
  
  obsr <- as.numeric(out[i,'estRbar',2]*c( out[i,'Rbar',2],1-out[i,'Rprop',2]))
  out[i,'obsR_A1',2] <- obsr[1];  out[i,'obsR_A2',2] <- obsr[2];
  rm(opt0)
}



# https://stackoverflow.com/questions/57173162/function-for-uniroot-that-has-two-parameters-that-need-to-be-run-across-a-vector
## the example above actually has 3 pars and he optimizes over 2 known vectors
## the mapply will return the best F value given proportion
FpropVec <- seq(0.01,1,0.01) ## all possible proportions of F in Area 1
fbest <-
  mapply(
    function(Fv_prop)
      uniroot(f = dfx.dxSYS, 
                         interval = c(0.02,5),
                         Fv_prop = Fv_prop)[1],
    FpropVec)
propmsy <- data.frame('Fprop' = FpropVec,'FMSY' = matrix(unlist(fbest)))  


## now take those best values and return ssb, yield etc
## this is telling us where the best yield actually occurs, as a function of both
out2 <- data.frame()
for(i in 1:nrow(propmsy)){
  out2[i,'FMSY'] <- propmsy[i,'FMSY']
  out2[i,'Fprop'] <- propmsy[i,'Fprop']
  FFs <- c(propmsy[i,'Fprop']*propmsy[i,'FMSY'],(1-propmsy[i,'Fprop'])*propmsy[i,'FMSY'] )
  out2[i,'FF_Area1'] <- FFs[1]
  out2[i,'FF_Area2'] <- FFs[2]
  
  opt0 <- optim_loop(FFs,i =NA) ## already specified
  opt_temp <- opt0$opt_temp; tmp0 <- opt0$tmp0; tmp <- opt0$tmp
  out2[i,'estRbar'] <- opt_temp$par[1];  out2[i,'estRprop'] <- opt_temp$par[2];
  
  ## derived quants at optimized value
  yields <- getYield(passR = out2[i,'estRbar'], passRprop =   out2[i,'estRprop'], YPR_F = tmp$YPR)
  out2[i,'Yield_A1'] <- yields[1];  out2[i,'Yield_A2'] <- yields[2]; 
  sbs <- getSB(passR = out2[i,'estRbar'], passRprop = out2[i,'estRprop'], SBPR_F = tmp$SBPR)
  out2[i,'SB_A1'] <- sbs[1];  out2[i,'SB_A2'] <- sbs[2];
  sb0 <- getSB(passR = out2[i,'estRbar'], passRprop = out2[i,'estRprop'], SBPR_F = tmp0$SBPR)
  out2[i,'SB0_A1'] <- sb0[1];  out2[i,'SB0_A2'] <- sb0[2];
  rexp <- getExpR(passR = out2[i,'estRbar'], passRprop =   out2[i,'estRprop'],SB_F = sbs, SB_0 =sb0)
  out2[i,'expR_A1'] <- rexp[1];  out2[i,'expR_A2'] <- rexp[2];
  obsr <- out2[i,'estRbar']*c(out2[i,'estRprop'],1-out2[i,'estRprop'])
  out2[i,'obsR_A1'] <- obsr[1];  out2[i,'obsR_A2'] <- obsr[2];
  rm(tmp)
}
out$tyield <- out$Yield_A1+out$Yield_A2
out$fprop <- out$FF_Area1/(out$FF_Area1+out$FF_Area2)
out[which.max(out$tyield),]


out2$tyield <- out2$Yield_A1+out2$Yield_A2
out2[out2 < 0] <- NA
out2[which.max(out2$tyield),]


source(here('R','figs.R'))
