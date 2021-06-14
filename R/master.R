## Code to replicate Kapur et al (202X)
## Spatial reference points for next-gen assessment models

## packages ----
rm(list = ls())
require(dplyr)
require(here)
require(ggplot2);require(ggsidekick);require(patchwork)
require(reshape2)
require(png);require(grid);require(gridExtra)

source(here("R","fnxs.R"))

s = 1

## run simulations

scen <- read.csv(here("inputscen.csv")) ## setup


FF.vec <- seq(0,5,0.5)
FFs <- expand.grid(FF.vec,FF.vec) 


## things to track
coln <- c( "FMSY_LOCAL","FMSY_GLOBAL","FPROP_LOCAL","FPROP_GLOBAL",
           "MSY_LOCAL","MSY_GLOBAL", "SBMSY_LOCAL","SBMSY_GLOBAL", 
           "A1SB0_LOCAL",'A2SB0_LOCAL',
           "A1SB0_old",'A2SB0_old',
           "A1DEPL_LOCAL", "A1DEPL_GLOBAL","A2DEPL_LOCAL", "A2DEPL_GLOBAL" )
scen <- cbind(scen, setNames( lapply(coln, function(x) x=NA), coln) )

for(s in 1:5){ #nrow(scen)){
  SCENARIO = scen[s,'SCENARIO_NAME']
  steeps <- c(scen[s,'H1'], scen[s,'H2'])
  
  surface <- array(NA, dim = c(nrow(FFs),3,2), 
                   dimnames = list(c(1:nrow(FFs)),
                                   c("FF_Area1","FF_Area2","tyield"),
                                   c('local','global'))) 
  
  for(i in 1:nrow(FFs)){
    

    surface[i,'FF_Area1',] <- FFs[i,1];  surface[i,'FF_Area2',] <- FFs[i,2]
    pStayt <- as.numeric(c(scen[s,'PSTAY_A1'],scen[s,'PSTAY_A2']))
    if(length(grep('WAA', SCENARIO)) == 0 ){
      dat <- makeDat(wa = NULL, ## default wa
                     mort = -log(scen[s,'NATM']),
                     h = steeps,
                     input_prop = scen[s,'PROPR'],
                     fec_a50 = c(6,6),
                     fec_a95 = c(12,12),
                     slx_a50= c(as.numeric(scen[s,'SLX_A50_A1']),9),
                     slx_a95= c(as.numeric(scen[s,'SLX_A95_A1']),13),
                     pStay=pStayt)
    } else{
      dat <- makeDat(wa = "NOTNULL",
                     mort = natM,
                     h = steeps,
                     input_prop = scen[s,'PROPR'],
                     fec_a50 = c(6,6),
                     fec_a95 = c(12,12),
                     slx_a50=slx_a50t,
                     slx_a95=slx_a95t,
                     pStay=pStayt)
    }

    
    ## fill in surface
    useFs <- as.numeric(c(FFs[i,]))
    tyields <-  runSim(par =useFs, dat, ret = 'vals', assume = NA)
      
    surface[i,'tyield','local'] <- tyields['tyield_local']
    surface[i,'tyield','global'] <- tyields['tyield_global']
  } ## end nrow FFs


  
    ## find MSY ----
  ulim = 1e2
    ss_global <- optim(par = c(0.5,0.5),
                dat= dat,
                assume = 'GLOBAL',
                ret = 'optim',
                lower = c(0,0),
                upper = c(ulim,ulim),
                method = 'L-BFGS-B',
                fn=runSim,
                control = list(
                  maxit = 500,
                  ndeps = rep(1e-4,2)))
    ulim = 3
    ss_local <- optim(par = c(0.5,0.5),
                dat= dat,
                assume = 'LOCAL',
                ret = 'optim',
                lower = c(0,0),
                upper = c(ulim,ulim),
                method = 'L-BFGS-B',
                fn=runSim,
                control = list(
                  maxit = 500,
                  ndeps = rep(1e-4,2)))
    
    ## pull out values at FMSY
    refpts_local <-  runSim(par =ss_local$par,dat, ret = 'vals', assume = NA)
    refpts_global <-  runSim(par =ss_global$par,dat, ret = 'vals', assume = NA)
    
    cat(scen[s,'PROPR'],"\n")
    print(refpts_global)
    print(refpts_local)
    
    scen[s,'FMSY_LOCAL'] <- sum(ss_local$par)
    scen[s,'MSY_LOCAL'] <- refpts_local['tyield_local']
    scen[s,'SBMSY_LOCAL'] <- refpts_local['local_tssb']
    scen[s,'A1DEPL_LOCAL'] <- NA
    scen[s,'A2DEPL_LOCAL'] <- NA
    scen[s,'A1SB0_LOCAL'] <-  NA
    scen[s,'A2SB0_LOCAL'] <-  NA
    scen[s,'A1SB0_old'] <-  NA
    scen[s,'A2SB0_old'] <-  NA
    scen[s,'FMSY_GLOBAL'] <- sum(ss_global$par)
    scen[s,'MSY_GLOBAL'] <-  refpts_global['tyield_global']
    scen[s,'SBMSY_GLOBAL'] <- refpts_global['global_tssb']
    scen[s,'SBMSY_A1_RATIO'] <- NA
    scen[s,'SBMSY_A2_RATIO'] <- NA
    scen[s,'A1DEPL_GLOBAL'] <- NA
    scen[s,'A2DEPL_GLOBAL'] <- NA
    

  maxf1 <- max(data.frame(surface[,"FF_Area1",'global']))
    global <- data.frame(surface[,,'global']) %>%
      ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = tyield)) +
      geom_tile() +
      coord_equal() +
      ggsidekick::theme_sleek() +
      theme(legend.position = 'top')+  
      scale_fill_viridis_c() +
      geom_point(data = NULL,
                 aes(x = ss_global$par[1], y = ss_global$par[2]),
                 fill = NA, color = 'blue', size = 2, pch =15)+
      annotate('text',
               x =0.6*maxf1,
               y = 0.85*maxf1,
               size = 5,
               color = 'blue',
               label = as.expression(bquote(MSY[global]~ "="~.(round(refpts_global['tyield_global'],2))))) +
      annotate('text',
               x =0.6*maxf1,
               y = 0.8*maxf1,
               size = 5,
               color ='blue',
               label = as.expression(bquote(F[MSY_global]~"="~.(round(ss_global$par[1],2))~"Area 1, "~.(round(ss_global$par[2],2))~"Area 2"))) +
    labs(x = 'F in Area 1',   y = 'F in Area 2', fill = 'Total Yield',  title = SCENARIO)
    
    locl <- data.frame(surface[,,'local']) %>%
      ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = tyield)) +
      geom_tile() +
      coord_equal() +
      ggsidekick::theme_sleek() +
      theme(legend.position = 'top')+     
      scale_fill_viridis_c() +
    geom_point(data = NULL,
               aes(x = ss_local$par[1], y = ss_local$par[2]),
               fill = NA, color = 'blue', size = 2, pch =15)+
    annotate('text',
             x =0.6*maxf1,
             y = 0.85*maxf1,
             size = 5,
             color = 'blue',
             label = as.expression(bquote(MSY[Local]~ "="~.(round(refpts_local['tyield_local'],2))))) +
    annotate('text',
             x =0.6*maxf1,
             y = 0.8*maxf1,
             size = 5,
             color ='blue',
             label = as.expression(bquote(F[MSY_Local]~"="~.(round(ss_local$par[1],2))~"Area 1, "~.(round(ss_local$par[2],2))~"Area 2"))) +
    labs(x = 'F in Area 1',   y = 'F in Area 2', fill = 'Total Yield',  title = SCENARIO)

    filetemp <- here('output',paste0(Sys.Date(),"-h=",paste0(steeps[1],"_",steeps[2]),"-",SCENARIO))
    dir.create(filetemp)
    ggsave(locl   | global,
           file = paste0(filetemp,"/heatmap.png"),
           width = 8, height = 6, unit = 'in', dpi = 520)
    save(ss_local, file = paste0(filetemp,'/ss_local.RDATA'))
    save(ss_global, file = paste0(filetemp,'/ss_local.RDATA'))
    save(surface, file = paste0(filetemp,'/surface.RDATA'))
    rm(ss_local); rm(ss_global); rm(surface); rm(locl); rm(global)
} ## end s in scen

data.frame(surface[,,'global']) %>%
  filter(FF_Area2 == 5) %>%
  ggplot(., aes(x = (FF_Area1), y = tyield)) + 
  geom_point()
  
