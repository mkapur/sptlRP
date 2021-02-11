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
  opt0 <- optim_loop(FFs,i)
  opt_temp <- opt0$opt_temp; tmp0 <- opt0$tmp0; tmp <- opt0$tmp
  out[i,'estRbar'] <- opt_temp$par[1];  out[i,'estRprop'] <- opt_temp$par[2];

  ## derived quants at optimized value
  yields <- getYield(passR = out[i,'estRbar'], passRprop =   out[i,'estRprop'], YPR_F = tmp$YPR)
  out[i,'Yield_A1'] <- yields[1];  out[i,'Yield_A2'] <- yields[2]; 
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
# dfx.dxSYS_curr - test that it works in 1d
# as.numeric(uniroot(f = dfx.dxSYS, 
#                    # Fv_prop = 0.5,
#                    # interval = c(1e-4,1),
#                    lower = 0.01, upper =2)[1])
# # FvtestVec <- seq(0.01,2,0.01)

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
ggplot(propmsy, aes(y = FMSY,x = Fprop)) +
  geom_line(lwd = 1.1) +
  scale_y_continuous(limits = c(0.5,1)) +
  theme_sleek() +
  labs(x = 'Proportion F applied to Area 1', y = 'FMSY')
ggsave(last_plot(), file = here('figs',paste0(Sys.Date(),'propFvsMSY.png')))


## now take those best values and return ssb, yield etc
## this is telling us where the best yield actually occurs, as a function of both
out2 <- data.frame()
for(i in 1:nrow(propmsy)){
  out2[i,'FMSY'] <- propmsy[i,'FMSY']
  out2[i,'Fprop'] <- propmsy[i,'Fprop']
  out2[i,'FF_Area1'] <- propmsy[i,'Fprop']*propmsy[i,'FMSY'];   out2[i,'FF_Area2'] <- (1-propmsy[i,'Fprop'])*propmsy[i,'FMSY']
  opt0 <- optim_loop(FFs,i)
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
out$fprop <- out$Yield_A1/(out$Yield_A1+out$Yield_A2)
out[which.max(out$tyield),]

out2$tyield <- out2$Yield_A1+out2$Yield_A2
out2[out2 < 0] <- NA

ggplot(out, aes(x = FF_Area1, y=FF_Area2 , fill = FF_Area1/(FF_Area1+FF_Area2) )) +
  scale_color_viridis_c(na.value = 'white') +
  theme_sleek() +
  geom_tile() + coord_equal() 
ggplot(out2, aes(x = FF_Area1, y=FF_Area2 , fill = Fprop )) +
  scale_color_viridis_c(na.value = 'white') +
  theme_sleek() +
  geom_tile() + coord_equal() 

out2 %>%
  ggplot(., aes(y = FMSY,x = Fprop, color =tyield )) +
  geom_point() +
  scale_y_continuous(limits = c(0.5,1)) +
  scale_color_viridis_c(na.value = 'white') +
  theme_sleek() +
  labs(x = 'Proportion F applied to Area 1', y = 'FMSY', color = 'Total Yield')


## plot propF x fmsy x yield
out2 %>%
  select(FMSY,Fprop,tyield) %>%
  # reshape2::melt(id = c("FMSY","Fprop")) %>%
  # mutate(Area = substr(variable,7,8), yield = value) %>%
  # select(-variable,-value) %>%
  ggplot(., aes(y = FMSY, x = Fprop, color = tyield)) +
  # geom_tile() +
    geom_point() +
  # coord_equal() +
  # ggsidekick::theme_sleek() + 
  scale_y_continuous(expand = c(0,0), limits = c(0.57,0.85), breaks = seq(0.57,0.85,0.01)) + 
  scale_x_continuous(expand = c(0,0), limits = c(0,1), breaks = seq(0,1,0.01)) +
  # scale_fill_viridis_c(option = 'magma',na.value = 'white') +
  # facet_wrap(~Area) +
  labs(x = 'Fprop', y = 'FMSY', fill = 'Yield in area') 

out2[which.max(out2$tyield),]  
ggsave(last_plot(), file = here('figs',paste0(Sys.Date(),'propFvsMSY_tyield.png')))
  
