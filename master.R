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
out <- array(NA, dim = c(nrow(FFs),15,2), 
             dimnames = list(c(1:nrow(FFs)),c("FF_Area1","FF_Area2",
                                     "estRbar","estRprop",
                                     "Yield_A1","Yield_A2",
                                     "SB_A1","SB_A2",
                                     "SB0_A1","SB0_A2",
                                     "expR_A1","expR_A2",
                                     "obsR_A1","obsR_A2","tyield"),
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
  
  sbs <-getSB(passR = out[i,'estRbar',1], passRprop = out[i,'estRprop',1], SBPR_F = tmp$SBPR)
  out[i,'SB_A1',1] <- as.numeric(sbs[1]);  out[i,'SB_A2',1] <- as.numeric(sbs[2]);
  
  sb0 <- getSB(passR = out[i,'estRbar',1], passRprop = out[i,'estRprop',1], SBPR_F = tmp0$SBPR)
  out[i,'SB0_A1',1] <- as.numeric(sb0[1]);  out[i,'SB0_A2',1] <-as.numeric(sb0[2]);
  
  rexp <- as.numeric(getExpR(passR = out[i,'estRbar',1], 
                             passRprop = out[i,'estRprop',1],
                             SB_F =sbs, SB_0 =sb0))
  out[i,'expR_A1',1] <- rexp[1];  out[i,'expR_A2',1] <- rexp[2];
  
  obsr <- as.numeric(out[i,'estRbar',1]*c(out[i,'estRprop',1],1-out[i,'estRprop',1]))
  out[i,'obsR_A1',1] <- obsr[1];  out[i,'obsR_A2',1] <- obsr[2];
  rm(opt0)
  
  ## derived quants at global value ("current method")
  yields <- as.numeric(getYield(passR = out[i,'estRbar',2], passRprop =   out[i,'estRprop',2], YPR_F = tmp$YPR))
  out[i,'Yield_A1',2] <- yields[1];  out[i,'Yield_A2',2] <- yields[2];
  
  
  sbs <-getSB(passR = out[i,'estRbar',2], passRprop = out[i,'estRprop',2], SBPR_F = tmp$SBPR)
  out[i,'SB_A1',2] <- as.numeric(sbs[1]);  out[i,'SB_A2',2] <- as.numeric(sbs[2]);
  
  sb0 <- getSB(passR = out[i,'estRbar',2], passRprop = out[i,'estRprop',2], SBPR_F = tmp0$SBPR)
  out[i,'SB0_A1',2] <- as.numeric(sb0[1]);  out[i,'SB0_A2',2] <-as.numeric(sb0[2]);

  rexp <- as.numeric(getExpR(passR = out[i,'estRbar',2], passRprop =   out[i,'estRprop',2],
                             SB_F = data.frame(sbs), SB_0 =data.frame(sb0)))
  out[i,'expR_A1',2] <- rexp[1];  out[i,'expR_A2',2] <- rexp[2];
  
  obsr <- as.numeric(out[i,'estRbar',2]*c( out[i,'estRbar',2],1-out[i,'estRprop',2]))
  out[i,'obsR_A1',2] <- obsr[1];  out[i,'obsR_A2',2] <- obsr[2];
  out[i,'tyield',1] <- out[i,'Yield_A1',1]+ out[i,'Yield_A2',1]
  out[i,'tyield',2] <- out[i,'Yield_A1',2]+ out[i,'Yield_A2',2]
}



# https://stackoverflow.com/questions/57173162/function-for-uniroot-that-has-two-parameters-that-need-to-be-run-across-a-vector
## the example above actually has 3 pars and he optimizes over 2 known vectors
## the mapply will return the best F value given proportion
## inside dfx.dxSYS_new we run optim and use passed R, Rprop
## need a second version of this which uses global R, rprop
FpropVec <- seq(0.01,1,0.01) ## all possible proportions of F in Area 1
fbest_new <-
  mapply(
    function(Fv_prop)
      uniroot(f = dfx.dxSYS_new, 
                         interval = c(0.02,5),
                         Fv_prop = Fv_prop)[1],
    FpropVec)

fbest_global  <-
  mapply(
    function(Fv_prop)
      uniroot(f = dfx.dxSYS_global, 
              interval = c(0.02,5),
              Fv_prop = Fv_prop)[1],
    FpropVec)

propmsy <- data.frame('Fprop' = FpropVec,
                      'FMSY_new' = matrix(unlist(fbest)),
                      'FMSY_global' = matrix(unlist(fbest_global)))  

## now take what's determined to be FMSY  and return ssb, yield etc
## this is telling us where the best yield actually occurs, as a function of both
out2 <- array(NA, dim = c(nrow(propmsy),17,2), 
             dimnames = list(c(rep(NA, nrow(propmsy))),c("Fprop", "FMSY",
                                                     "FF_Area1","FF_Area2",
                                                     "estRbar","estRprop",
                                                     "Yield_A1","Yield_A2",
                                                     "SB_A1","SB_A2",
                                                     "SB0_A1","SB0_A2",
                                                     "expR_A1","expR_A2",
                                                     "obsR_A1","obsR_A2", 'tyield'),
                             c('new','old'))) ## each slice is old or new
for(i in 1:nrow(propmsy)){
  out2[i,'Fprop',1:2] <- propmsy[i,'Fprop']
  out2[i,'FMSY',1] <- propmsy[i,'FMSY_new']
  out2[i,'FMSY',1] <- propmsy[i,'FMSY_global']
  FFs_new <- c(propmsy[i,'Fprop']*propmsy[i,'FMSY_new'],(1-propmsy[i,'Fprop'])*propmsy[i,'FMSY_new'] )
  FFs_global <- c(propmsy[i,'Fprop']*propmsy[i,'FMSY_global'],(1-propmsy[i,'Fprop'])*propmsy[i,'FMSY_global'] )
  
  out2[i,'FF_Area1',1] <- FFs_new[1]; out2[i,'FF_Area2',1] <- FFs_new[2]
  out2[i,'FF_Area1',2] <- FFs_global[1]; out2[i,'FF_Area2',2] <- FFs_global[2]
  
  opt0 <- optim_loop(FFs_new,i = NA) ## already specified
  opt_temp <- opt0$opt_temp; tmp0 <- opt0$tmp0; tmp <- opt0$tmp
  
  out2[i,'estRbar',1] <- opt_temp$par[1];  out2[i,'estRprop',1] <- opt_temp$par[2];
  out2[i,'estRbar',2] <- R0_global;  out2[i,'estRprop',2] <- Rprop_input
  ## derived quants at optimized value
  yields <- as.numeric(getYield(passR = out2[i,'estRbar',1], passRprop =   out2[i,'estRprop',1], YPR_F = tmp$YPR))
  out2[i,'Yield_A1',1] <- yields[1];  out2[i,'Yield_A2',1] <- yields[2];
  
  sbs <-getSB(passR = out2[i,'estRbar',1], passRprop = out2[i,'estRprop',1], SBPR_F = tmp$SBPR)
  out2[i,'SB_A1',1] <-  as.numeric(sbs[1]);  out2[i,'SB_A2',1] <-  as.numeric(sbs[2]);
  
  sb0 <- getSB(passR = out2[i,'estRbar',1], passRprop = out2[i,'estRprop',1], SBPR_F = tmp0$SBPR)
  out2[i,'SB0_A1',1] <-  as.numeric(sb0[1]);  out2[i,'SB0_A2',1] <-  as.numeric(sb0[2]);
  
  rexp <- as.numeric(getExpR(passR = out2[i,'estRbar',1], 
                             passRprop = out2[i,'estRprop',1],
                             SB_F = sbs, SB_0 =sb0))
  out2[i,'expR_A1',1] <- rexp[1];  out2[i,'expR_A2',1] <- rexp[2];
  
  obsr <- as.numeric(out2[i,'estRbar',1]*c(out2[i,'estRprop',1],1-out2[i,'estRprop',1]))
  out2[i,'obsR_A1',1] <- obsr[1];  out2[i,'obsR_A2',1] <- obsr[2];
  rm(opt0)
  ## derived quants at global value ("current method")
  yields <- as.numeric(getYield(passR = out2[i,'estRbar',2], passRprop =   out2[i,'estRprop',2], YPR_F = tmp$YPR))
  out2[i,'Yield_A1',2] <- yields[1];  out2[i,'Yield_A2',2] <- yields[2];
  
  sbs <-getSB(passR = out2[i,'estRbar',2], passRprop = out2[i,'estRprop',2], SBPR_F = tmp$SBPR)
  out2[i,'SB_A1',2] <-  as.numeric(sbs[1]);  out2[i,'SB_A2',2] <-  as.numeric(sbs[2]);
  
  sb0 <- getSB(passR = out2[i,'estRbar',2], passRprop = out2[i,'estRprop',2], SBPR_F = tmp0$SBPR)
  out2[i,'SB0_A1',2] <-  as.numeric(sb0[1]);  out2[i,'SB0_A2',2] <-  as.numeric(sb0[2]);

  rexp <- as.numeric(getExpR(passR = out2[i,'estRbar',2], passRprop =out2[i,'estRprop',2],
                             SB_F = data.frame(sbs), SB_0 =data.frame(sb0)))
  out2[i,'expR_A1',2] <- rexp[1];  out2[i,'expR_A2',2] <- rexp[2];
  
  obsr <- as.numeric(out2[i,'estRbar',2]*c( out2[i,'estRbar',2],1-out2[i,'estRprop',2]))
  out2[i,'obsR_A1',2] <- obsr[1];  out2[i,'obsR_A2',2] <- obsr[2];
  
  out2[i,'tyield',1] <- out2[i,'Yield_A1',1]+ out2[i,'Yield_A2',1]
  out2[i,'tyield',2] <- out2[i,'Yield_A1',2]+ out2[i,'Yield_A2',2]
  rm(tmp)
}



out$fprop <- out$FF_Area1/(out$FF_Area1+out$FF_Area2)
out[which.max(out$tyield),]
out2$tyield <- out2$Yield_A1+out2$Yield_A2
out2[out2 < 0] <- NA
out2[which.max(out2$tyield),]




