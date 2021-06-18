require(here)
require(dplyr)
require(ggplot2)

## sanity check 1 ----
## Confirm without movement things are equivalent
## among methods and across Prop
source(here('R','fnxs-dep2.r'))
Prop <- 0.5
Detail <- T
SBPF0 <- 100.3253#480.904 # GLOBAL
PopN_global(c(-10000,-100000)) ## F=0

for (II in 1:9) {
  Prop <- II*0.1
  Detail <- F
  ss <- optim(par=c(log(0.1),log(0.1)),fn=PopN_global)
  ss <- optim(par=ss$par,fn=PopN_global)
  Detail <- T
  PopN_global(ss$par)
}
for (II in 1:9) {
  Prop <- II*0.1
  Detail <- F
  ## UPDATE SB0 based on prop
  SBPF0 <- PopN_local(c(-10000,-10000), getSBPF0=T) ## F=0
  PopN_local(c(-10000,-10000)) ## F=0
  # cat(Prop,"\t",SBPF0,"\t", sum(SBPF0),"\n")

  ss <- optim(par=c(log(0.1),log(0.1)),
              fn=PopN_local, 
              lower = c(-10000,-10000),
              upper = c(log(10000),log(10000)),
              method = 'L-BFGS-B')
  ss <- optim(par=ss$par,
              fn=PopN_local,
              lower = c(-10000,-10000),
              upper = c(log(10000),log(10000)),
              method = 'L-BFGS-B')
  Detail <- T
  PopN_local(ss$par)
}

## run simulations
scen <- read.csv(here("inputscen.csv")) ## setup

for(i in 1:nrow(scen)){
  Detail=F
  Prop = scen[i,'PROPR']
  Steep <- c(scen[i,'H1'],scen[i,'H2'])
  a50 <- scen[i,'H1']
  ## build surface
  FFs <- expand.grid(seq(0,5,0.05),seq(0,5,0.05)) ## continuous F, will dictate range of yield plot
  surface <- array(NA, dim = c(nrow(FFs),15,2), 
                   dimnames = list(c(1:nrow(FFs)),c("FF_Area1","FF_Area2",
                                                    "estRbar","estRprop",
                                                    "Yield_A1","Yield_A2",
                                                    "SB_A1","SB_A2",
                                                    "SB0_A1","SB0_A2",
                                                    "expR_A1","expR_A2",
                                                    "obsR_A1","obsR_A2","tyield"),
                                   c('local','global'))) ## each slice is old or new
  for(i in 1:nrow(FFs)){
    pStayt <- as.numeric(c(scen[s,'PSTAY_A1'],scen[s,'PSTAY_A2']))
    if(length(grep('WAA', SCENARIO)) == 0 ){
      dat <- makeDat(wa = NULL, ## default wa
                     mort = natM,
                     h = c(0.7,0.7),
                     input_prop = Prop,
                     fec_a50 = c(6,6),
                     fec_a95 = c(12,12),
                     slx_a50=slx_a50t,
                     slx_a95=slx_a95t,
                     pStay=pStayt)
    } else{
      dat <- makeDat(wa = c(10,5), ## higher
                     mort = natM,
                     fec_a50 = c(6,6),
                     fec_a95 = c(12,12),
                     slx_a50=slx_a50t,
                     slx_a95=slx_a95t,
                     pStay=pStayt)
    }
    
    ## generate the surface
    SBPF0 <- PopN_local(getSBPF0=T, 
                        slx_a50 = 9, 
                        slx_a95=13,
                        h = Steep,
                        pstay_a1 = dat[,'proportion_stay',1],
                        pstay_a2 = dat[,'proportion_stay',2],
                        par = log(as.numeric(c(FFs[i,])))) ## F=0
    tmp_local <- PopN_global(returnVals = T, 
                             slx_a50 = 9, 
                             slx_a95=13,
                             h = Steep,
                             pstay_a1 = dat[,'proportion_stay',1],
                             pstay_a2 = dat[,'proportion_stay',2],
                             par = log(as.numeric(c(FFs[i,]))))
    
    SBPF0 <- 480.904 
    tmp_global <- PopN_local(returnVals = T, par =  log(as.numeric(c(FFs[i,]) )))
    
    surface[i,'FF_Area1',] <- FFs[i,1]
    surface[i,'FF_Area2',] <- FFs[i,2]
    surface[i,'tyield','local'] <- tmp_local$tyield
    surface[i,'tyield','global'] <- tmp_global$tyield
  }
  
  
  ## global surface ----
  tmp_global <- PopN_local(returnVals = T, par =  log(as.numeric(c(FFs[i,]) )))
  SBPF0 <- PopN_local(c(-10000,-10000), getSBPF0=T) ## F=0
  # cat(Prop,"\t",SBPF0,"\t", sum(SBPF0),"\n")
  Detail <- F
  ss <- optim(par=c(log(0.1),log(0.1)),
              fn=PopN_local, 
              lower = c(-10000,-10000),
              upper = c(log(10000),log(10000)),
              method = 'L-BFGS-B')
  ss <- optim(par=ss$par,
              fn=PopN_local,
              lower = c(-10000,-10000),
              upper = c(log(10000),log(10000)),
              method = 'L-BFGS-B')
  Detail <- T
  PopN_local(ss$par)
  ## global optim

  surface[i,'tyield','local'] <- tmp_local$tyield
  surface[i,'tyield','global'] <- tmp_global$tyield
  ## local surface
  tmp_local <- PopN_global(returnVals = T, par = log(as.numeric(c(FFs[i,]) )))
  
  ## local optim
  SBPF0 <- PopN_local(c(-10000,-10000), getSBPF0=T) ## F=0
  # cat(Prop,"\t",SBPF0,"\t", sum(SBPF0),"\n")
  Detail <- F
  ss <- optim(par=c(log(0.1),log(0.1)),
              fn=PopN_local, 
              lower = c(-10000,-10000),
              upper = c(log(10000),log(10000)),
              method = 'L-BFGS-B')
  ss <- optim(par=ss$par,
              fn=PopN_local,
              lower = c(-10000,-10000),
              upper = c(log(10000),log(10000)),
              method = 'L-BFGS-B')
  Detail <- T
  PopN_local(ss$par)
}

## generate surface for plot
FFs <- expand.grid(seq(0,5,0.05),seq(0,5,0.05)) ## continuous F, will dictate range of yield plot
surface <- array(NA, dim = c(nrow(FFs),15,2), 
                 dimnames = list(c(1:nrow(FFs)),c("FF_Area1","FF_Area2",
                                                  "estRbar","estRprop",
                                                  "Yield_A1","Yield_A2",
                                                  "SB_A1","SB_A2",
                                                  "SB0_A1","SB0_A2",
                                                  "expR_A1","expR_A2",
                                                  "obsR_A1","obsR_A2","tyield"),
                                 c('local','global'))) ## each slice is old or new
for(i in 1:nrow(FFs)){
  
  ## generate the surface
  tmp_local <- PopN_global(returnVals = T, par = log(as.numeric(c(FFs[i,]) )))
  tmp_global <- PopN_local(returnVals = T, par =  log(as.numeric(c(FFs[i,]) )))
  surface[i,'FF_Area1',] <- FFs[i,1]
  surface[i,'FF_Area2',] <- FFs[i,2]
  surface[i,'tyield','local'] <- tmp_local$tyield
  surface[i,'tyield','global'] <- tmp_global$tyield
}

data.frame(surface[,,'local']) %>%
  select(FF_Area1,FF_Area2, Yield_Total =tyield) %>%  
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  # mutate(Area = substr(variable,7,8), yield = value) %>%
  mutate(yield = value) %>%
  select(-variable,-value) %>% 
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = yield)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() +
  theme(legend.position = 'top',plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  scale_fill_gradient2(low = "white", high = "grey11")+
  geom_point(data = out2_new, aes(x = out2_new[which.max(out2_new[,'tyield']),'FF_Area1'],
                                  y = out2_new[which.max(out2_new[,'tyield']),'FF_Area2']), 
             fill = NA, color = 'gold', size = 2, pch =15)+
  annotate('text',
           x =0.6*maxf1,
           y = 0.85*maxf1,
           size = 5,
           color = 'gold',
           label = as.expression(bquote(MSY[Local]~
                                          "="~.(round(out2_new[which.max(out2_new[,'tyield']),
                                                               'tyield']))))) +
  annotate('text',
           x =0.6*maxf1,
           y = 0.8*maxf1,
           size = 5,
           color ='gold',
           label = as.expression(bquote(F[MSY_Local]~"="~.(round(out2_new[which.max(out2_new$tyield),'FF_Area1'],2))~"Area 1, "~.
                                        (round(out2_new[which.max(out2_new$tyield),'FF_Area2'],2))~"Area 2"))) +
  
  labs(x = 'F in Area 1',   y = 'F in Area 2', fill = 'Total Yield',  title = SCENARIO) 


