## Code to replicate Kapur et al (202X)
## Spatial reference points for next-gen assessment models

require(dplyr)
require(here)
require(ggplot2);require(ggsidekick)
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
  out[i,'FF_Area1'] <- FF_temp[1];   out[i,'FF_Area2'] <- FF_temp[2]; 
  tmp <- doPR(dat,FF =  as.numeric(c(FFs[i,]))) 
  tmp0 <-  doPR(dat,FF = c(0,0) )
 
  ## optimize this
  opt_temp <- optim(par = c(4,0.6),
        SBPR_F=   tmp$SBPR,
        SBPR_0 = tmp0$SBPR,
        lower = c(0,0),
        upper = c(NA,1),
        method = "L-BFGS-B",
        fn = optimFunc, hessian = FALSE)

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



## sanity checking functions ----
png(here('figs','nominal_srr.png'))
plot(cbind(1,bh(h=steep, prop = Rprop_input, r0 = R0_global, b0 = 15, bcurr = 1)),
     ylim = c(0,3),xlim = c(0,100), xlab = 'SBcurrent', ylab = 'BH Rec')
for(i in 1:100)points(cbind(i,bh(h=steep, prop = Rprop_input, r0 = R0_global, b0 = 15, bcurr = i)))
dev.off()


par(mfrow = c(2,2))
tmp <- doPR(dat, FF = c(0,0)) ## defaults, no fishing
all(round(colSums(tmp$NPR[,,1])) == round(colSums(tmp$NPR[,,2]))) ## net exchange should be conserved
plot(tmp$NPR[1,,1], main = 'A1 SRC');points(tmp$NPR[2,,1],col = 'blue') ## should look reasonable
legend('topright', pch =1, legend = c('stay in a1', 'end up in a2'), col = c('black','blue'))
plot(tmp$NPR[1,,2], main = 'A2 SRC');points(tmp$NPR[2,,2],col = 'blue') ## should look reasonable

## test run with no movement
dat <- makeDat(wa = c(5,5),
               fec_a50 = c(6,6),
               fec_a95 = c(12,12),
               slx_a50 = c(9,9),
               slx_a95 = c(13,13),
               pStay = c(1,1))
tmp <- doPR(dat, FF = c(0,0)) ## defaults, no fishing
all(round(colSums(tmp$NPR[,,1])) == round(colSums(tmp$NPR[,,2]))) ## net exchange should be conserved
plot(tmp$NPR[1,,1], main = 'A1 SRC');points(tmp$NPR[2,,1],col = 'blue') 
legend('topright', pch =1, legend = c('stay in a1', 'end up in a2'), col = c('black','blue'))
plot(tmp$NPR[1,,2], main = 'A2 SRC');points(tmp$NPR[2,,2],col = 'blue') 
dev.off()

## test with movement & some Fs
par(mfrow = c(2,2))
dat <- makeDat(wa = c(5,5),
               fec_a50 = c(6,6),
               fec_a95 = c(12,12),
               slx_a50 = c(9,9),
               slx_a95 = c(13,13),
               pStay = c(0.9,0.6))
tmp <- doPR(dat, FF = c(0.2,0.2)) ## defaults, equally low fishing
all(round(colSums(tmp$NPR[,,1])) == round(colSums(tmp$NPR[,,2]))) ## net exchange should be conserved
plot(tmp$NPR[1,,1],ylim = c(0,1), main = 'A1 SRC');points(tmp$NPR[2,,1],col = 'blue') ## A2 ()
legend('topright', pch =1, legend = c('stay in a1', 'end up in a2'), col = c('black','blue'))
plot(tmp$NPR[1,,2],ylim = c(0,1), main = 'A2 SRC');points(tmp$NPR[2,,2],col = 'blue') 

dat <- makeDat(wa = c(5,5),
               fec_a50 = c(6,6),
               fec_a95 = c(12,12),
               slx_a50 = c(9,9),
               slx_a95 = c(13,13),
               pStay = c(0.9,0.6))
tmp <- doPR(dat, FF = c(0.8,0.8)) ## defaults, no fishing
all(round(colSums(tmp$NPR[,,1])) == round(colSums(tmp$NPR[,,2]))) ## net exchange should be conserved
plot(tmp$NPR[1,,1],ylim = c(0,1), main = 'A1 SRC');points(tmp$NPR[2,,1],col = 'blue') 
legend('topright', pch =1, legend = c('stay in a1', 'end up in a2'), col = c('black','blue'))
plot(tmp$NPR[1,,2], ylim = c(0,1),main = 'A2 SRC');points(tmp$NPR[2,,2],col = 'blue') 


