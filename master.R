## Code to replicate Kapur et al (202X)
## Spatial reference points for next-gen assessment models

require(dplyr)
require(here)
require(ggplot2);require(ggsidekick);require(patchwork)
require(reshape2)

source(here("R","fnxs.R"))

## build datasets to spec (will autosave figure)
dat <- makeDat(wa = c(5,5),
               fec_a50 = c(6,6),
               fec_a95 = c(12,12),
               slx_a50 = c(9,9),
               slx_a95 = c(13,13),
               pStay = c(0.9,0.6))

## settings
R0_global <- 4
Rprop_input <- 0.65
steep = 0.75

FFs <- expand.grid(seq(0,1,0.05),seq(0,1,0.05))
out <- data.frame()
for(i in 1:nrow(FFs)){
  
  out[i,'FF_Area1'] <- FFs[i,1];   out[i,'FF_Area2'] <- FFs[i,2]
  tmp <- doPR(dat,FF = as.numeric(c(FFs[i,]))) 
  tmp0 <-  doPR(dat,FF = c(0,0) )
 
  ## optimize this
  opt_temp <- optim(par = c(4,0.6),
        SBPR_F = tmp$SBPR,
        SBPR_0 = tmp0$SBPR,
        lower = c(1E-4,1E-4),
        upper = c(NA,1),
        method = "L-BFGS-B",
        fn = optimFunc, hessian = FALSE,
        control = list(
                       maxit = 1000,
                       ndeps = rep(1e-4,2)))

  out[i,'estRbar'] <- opt_temp$par[1];  out[i,'estRprop'] <- opt_temp$par[2];

  ## derived quants at optimized value
  yields <- getYield(passR = out[i,'estRbar'], passRprop =   out[i,'estRprop'], YPR_F = tmp$YPR)
  out[i,'Yield_A1'] <- yields[1];  out[i,'Yield_A2'] <- yields[2]; ## build this
  sbs <- getSB(passR = out[i,'estRbar'], passRprop = out[i,'estRprop'], SBPR_F = tmp$SBPR)
  out[i,'SB_A1'] <- sbs[1];  out[i,'SB_A2'] <- sbs[2];
  sb0 <- getSB(passR = out[i,'estRbar'], passRprop = out[i,'estRprop'], SBPR_F = tmp0$SBPR)
  out[i,'SB0_A1'] <- sb0[1];  out[i,'SB0_A2'] <- sb0[2];
  rexp <- getExpR(passR = out[i,'estRbar'], passRprop =   out[i,'estRprop'],SB_F = sbs, SB_0 =sb0)
  out[i,'expR_A1'] <- rexp[1];  out[i,'expR_A2'] <- rexp[2];
  obsr <- out[i,'estRbar']*c(out[i,'estRprop'],1-out[i,'estRprop'])
  out[i,'obsR_A1'] <- obsr[1];  out[i,'obsR_A2'] <- obsr[2];
  rm(tmp)
}
source(here('R','figs.R'))

## now to optimize in 2d space via uniroot
# dfx.dxSYS_curr
# sysopt_curr[RR,2,s] <- as.numeric(uniroot(f = dfx.dxSYS_curr, 
#                                           RLI = RR,
#                                           pik = splt,
#                                           movemat = X_ija_all,
#                                           interval = c(0.02,1))[1])

