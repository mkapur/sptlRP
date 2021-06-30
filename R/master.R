## Code to replicate Kapur et al (202X)
## Spatial reference points for next-gen assessment models

## packages ----
rm(list = ls())
require(dplyr)
require(here)
require(ggplot2); require(patchwork)
require(reshape2)
require(png);require(grid);require(gridExtra)

# source("fnxs.R")
source(here('R','fnxs.R'))
## run simulations
scen <- read.csv(here("inputscen.csv")) ## setup
GLOBAL_R0 = 1
# dim(FFs)

## things to track
coln <- c( 
  "FMSY_GLOBAL_A1",
  "FMSY_GLOBAL_A2",
  "FMSY_LOCAL_A1",
  "FMSY_LOCAL_A2",
  "FMSY_LOCAL", 
  "FMSY_GLOBAL",
  "MSY_LOCAL",
  "MSY_GLOBAL",
  "SBMSY_LOCAL",
  "SBMSY_GLOBAL", 
  "A1SB0_LOCAL",
  'A2SB0_LOCAL',
  "A1SB0_GLOBAL",
  'A2SB0_GLOBAL',
  "A1DEPL_LOCAL",
  "A1DEPL_GLOBAL",
  "A2DEPL_LOCAL",
  "A2DEPL_GLOBAL" )
scen <- cbind(scen, setNames( lapply(coln, function(x) x=NA), coln) )

for(s in 1:nrow(scen)){

  SCENARIO = scen[s,'SCENARIO_NAME']
  steeps <- c(scen[s,'H1'], scen[s,'H2'])
  pStayt <- as.numeric(c(scen[s,'PSTAY_A1'],scen[s,'PSTAY_A2']))
  if(length(grep('WAA', SCENARIO)) == 0 ){
    dat <- makeDat(wa = NULL, ## default wa
                   mort = scen[s,'NATM'],
                   h = steeps,
                   input_prop = scen[s,'PROPR'],
                   fec_a50 = c(6,6),
                   fec_a95 = c(12,12),
                   slx_a50= c(as.numeric(scen[s,'SLX_A50_A1']),9),
                   slx_a95= c(as.numeric(scen[s,'SLX_A95_A1']),13),
                   pStay=pStayt)
  } else{
    dat <- makeDat(wa = "NOTNULL",
                   mort = scen[s,'NATM'],
                   h = steeps,
                   input_prop = scen[s,'PROPR'],
                   fec_a50 = c(6,6),
                   fec_a95 = c(12,12),
                   slx_a50= c(as.numeric(scen[s,'SLX_A50_A1']),9),
                   slx_a95= c(as.numeric(scen[s,'SLX_A95_A1']),13),
                   pStay=pStayt)
  }
 #print(dat)
  
  
  FMAX <- scen[s,'FMAX']
  FF.vec = seq(0,FMAX,0.05)
  # FF.vec = seq(0,5,0.05)
  FFs <- expand.grid(FF.vec,FF.vec) 
  
  surface <- array(NA, dim = c(nrow(FFs),7,2), 
                   dimnames = list(c(1:nrow(FFs)),
                                   c("FF_Area1","FF_Area2",'tSSB', 'req','req_prop', 'tSSB0',"tyield"),
                                   c('local','global'))) 
  for(i in 1:nrow(FFs)){
    if(i %% 100 ==0) cat(i,"\n")
    surface[i,'FF_Area1',] <- FFs[i,1];  surface[i,'FF_Area2',] <- FFs[i,2]
    ## fill in surface
    useFs <- log(as.numeric(c(FFs[i,])))
    tyields <-  runSim(par =useFs, dat, ret = 'vals', assume = NA)
    surface[i,'tSSB','local'] <- tyields['local_tssb']
    surface[i,'tSSB','global'] <-  tyields['global_tssb'] #tyields$global_tssb
    surface[i,'tSSB0','local'] <- tyields['local_tssb0'] #tyields$local_tssb0
    surface[i,'tSSB0','global'] <- tyields['global_tssb0'] #tyields$global_tssb0
    surface[i,'req','local'] <- tyields['req_local'] #tyields$req_local[1]
    surface[i,'req_prop','local'] <- tyields['req_local_prop'] #tyields$req_local[1]
    surface[i,'req','global'] <- tyields['req_global'] #tyields$req_global
    surface[i,'req_prop','global'] <- dat$input_prop
    surface[i,'tyield','local'] <- tyields['tyield_local'] #tyields$tyield_local
    surface[i,'tyield','global'] <- tyields['tyield_global'] #tyields$tyield_global
  } ## end nrow FFs
  head(surface)
  
  
  ## find MSY ----
  # ulim = log(10)
  ss_global <- optim(par = c(0.41,0.41),
                     dat= dat,
                     assume = 'GLOBAL',
                     ret = 'optim',
                     # lower = c(,0),
                     # upper = c(ulim,ulim),
                     # method = 'L-BFGS-B',
                     fn=runSim,
                     control = list(
                       maxit = 1000,
                       ndeps = rep(1e-4,2)))
  ss_global <- optim(par = ss_global$par,
                     dat= dat,
                     assume = 'GLOBAL',
                     ret = 'optim',
                     # lower = c(,0),
                     # upper = c(ulim,ulim),
                     # method = 'L-BFGS-B',
                     fn=runSim,
                     control = list(
                       maxit = 1000,
                       ndeps = rep(1e-4,2)))
  ss_local <- optim(par =  ss_global$par,
                    dat= dat,
                    assume = 'LOCAL',
                    ret = 'optim',
                    # lower = c(-1000,-1000),
                    # upper = c(1,1),
                    # method = 'L-BFGS-B',
                    fn=runSim,
                    control = list(
                      maxit = 1000,
                      ndeps = rep(1e-4,2)))


  cat( exp(ss_global$par),"\n")
  cat( exp(ss_local$par),"\n")
  print("test")

  
  ## pull out values at FMSY
  refpts_local <-  runSim(par =ss_local$par,dat, ret = 'vals', assume = NA)
  refpts_global <-  runSim(par =ss_global$par,dat, ret = 'vals', assume = NA)

  scen[s,'FMSY_LOCAL_A1'] <- exp(ss_local$par)[1]
  scen[s,'FMSY_LOCAL_A2'] <- exp(ss_local$par)[2]
  scen[s,'FMSY_GLOBAL_A1'] <-exp(ss_global$par)[1]
  scen[s,'FMSY_GLOBAL_A2'] <- exp(ss_global$par)[2]

  scen[s,'FMSY_LOCAL'] <- sum(exp(ss_local$par))
  scen[s,'MSY_LOCAL'] <- refpts_local['tyield_local']
  scen[s,'SBMSY_LOCAL'] <- refpts_local['local_tssb']
  scen[s,'A1SB0_LOCAL'] <-  refpts_local['local_tssb0']*refpts_local['req_local_prop']
  scen[s,'A2SB0_LOCAL'] <-  refpts_local['local_tssb0']*(1-refpts_local['req_local_prop'])

  scen[s,'A1DEPL_LOCAL'] <-  (refpts_local['local_tssb']*refpts_local['req_local_prop'])/  scen[s,'A1SB0_LOCAL']
  scen[s,'A2DEPL_LOCAL'] <- (refpts_local['local_tssb']*(1-refpts_local['req_local_prop']))/   scen[s,'A2SB0_LOCAL']

  scen[s,'A1SB0_GLOBAL'] <-   refpts_global['global_tssb0']*dat$input_prop
  scen[s,'A2SB0_GLOBAL'] <-  refpts_global['global_tssb0']*(1-dat$input_prop)
  scen[s,'FMSY_GLOBAL'] <- sum(exp(ss_global$par))
  scen[s,'MSY_GLOBAL'] <-  refpts_global['tyield_global']
  scen[s,'SBMSY_GLOBAL'] <- refpts_global['global_tssb']
  scen[s,'SBMSY_A1_RATIO'] <- refpts_global['global_a1ssb']/refpts_local['local_a1ssb']
  scen[s,'SBMSY_A2_RATIO'] <- refpts_global['global_a2ssb']/refpts_local['local_a2ssb']
  scen[s,'A1DEPL_GLOBAL'] <- refpts_global['global_tssb']*dat$input_prop/  scen[s,'A1SB0_GLOBAL']
  scen[s,'A2DEPL_GLOBAL'] <-  refpts_global['global_tssb']*(1-dat$input_prop)/  scen[s,'A2SB0_GLOBAL']

  maxf1 <- max(data.frame(surface[,"FF_Area1",'global']))
  global <- data.frame(surface[,,'global']) %>%
    filter(FF_Area1 <= maxf1 & FF_Area2 <= maxf1) %>%
    ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = tyield)) +
    geom_tile() +
    coord_equal() +
    #ggsidekick::theme_sleek() +
    theme(legend.position = 'top')+
    scale_fill_viridis_c() +
    scale_x_continuous(limits = c(NA,maxf1), breaks = seq(0,maxf1, 0.25), expand = c(0,0)) +
    scale_y_continuous(limits = c(NA,maxf1), breaks = seq(0,maxf1, 0.25), expand = c(0,0)) +
    geom_point(data = NULL,
               aes(x = exp(ss_global$par[1]), y = exp(ss_global$par[2])),
               fill = NA, color = 'blue', size = 2, pch =15) +
    annotate('text',
             x =0.6*maxf1,
             y = 0.85*maxf1,
             size = 3,
             color = 'blue',
             label = as.expression(bquote(MSY[Global]~ "="~.(round(refpts_global['tyield_global'],2))))) +
    annotate('text',
             x =0.6*maxf1,
             y = 0.8*maxf1,
             size = 3,
             color ='blue',
             label = as.expression(bquote(F[MSY_Global]~"="~.(round(exp(ss_global$par[1]),2))~"Area 1, "~.(round(exp(ss_global$par[2]),2))~"Area 2"))) +
    labs(x = 'F in Area 1',   y = 'F in Area 2', fill = 'Total Yield',  title = SCENARIO)



  locl <- data.frame(surface[,,'local']) %>%
    filter(FF_Area1 <= maxf1 & FF_Area2 <= maxf1) %>%
    ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = tyield)) +
    geom_tile() +
    coord_equal() +
    #ggsidekick::theme_sleek() +
    theme(legend.position = 'top')+
    scale_fill_viridis_c() +
    scale_x_continuous(limits = c(NA,maxf1), breaks = seq(0,maxf1, 0.25), expand = c(0,0)) +
    scale_y_continuous(limits = c(NA,maxf1), breaks = seq(0,maxf1, 0.25), expand = c(0,0)) +
    geom_point(data = NULL,
               aes(x = exp(ss_local$par[1]), y = exp(ss_local$par[2])),
               fill = NA, color = 'blue', size = 2, pch =15)+
    annotate('text',
             x =0.6*maxf1,
             y = 0.85*maxf1,
             size = 3,
             color = 'blue',
             label = as.expression(bquote(MSY[Local]~ "="~.(round(refpts_local['tyield_local'],2))))) +
    annotate('text',
             x =0.6*maxf1,
             y = 0.8*maxf1,
             size = 3,
             color ='blue',
             label = as.expression(bquote(F[MSY_Local]~"="~.(round(exp(ss_local$par[1]),2))~"Area 1, "~.(round(exp(ss_local$par[2]),2))~"Area 2"))) +
    labs(x = 'F in Area 1',   y = 'F in Area 2', fill = 'Total Yield',  title = SCENARIO)


  ## viewing plots in this manner requires the patchwork() package
  locl   | global

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


## format all scenarios in to table and save
data.frame(scen) %>%
  mutate(WA = "Linear increasing function; identical between areas") %>%
  mutate(
    NATM = round(as.numeric(-log(NATM)),2),
    'GLOBAL_FMSY_A1' = FMSY_GLOBAL_A1,
    'LOCAL_FMSY_A1' = FMSY_LOCAL_A1,
    'GLOBAL_FMSY_A2' = FMSY_GLOBAL_A2,
    'LOCAL_FMSY_A2'= FMSY_LOCAL_A2,
    'GLOBALFMSY' = paste(round(GLOBAL_FMSY_A1,2),round(GLOBAL_FMSY_A2,2), sep = ", "),
    'LOCALFMSY' = paste(round(LOCAL_FMSY_A1,2),round(LOCAL_FMSY_A2,2), sep = ", "),

    'MSY_RATIO' =  round(as.numeric(MSY_GLOBAL)/as.numeric(MSY_LOCAL),2),
    'GLOBAL_SBMSY' = round(as.numeric(SBMSY_GLOBAL),2),
    'LOCAL_SBMSY' = as.numeric(SBMSY_LOCAL),

    'SBMSY_RATIO' = round(GLOBAL_SBMSY/LOCAL_SBMSY,2),
    'SBMSY_A1_RATIO' = round(as.numeric(SBMSY_A1_RATIO),2),
    'SBMSY_A2_RATIO' = round(as.numeric(SBMSY_A2_RATIO),2),

    'GLOBAL_SB0' = as.numeric(A1SB0_GLOBAL)+as.numeric(A2SB0_GLOBAL),
    'LOCAL_SB0' = as.numeric(A1SB0_LOCAL)+as.numeric(A2SB0_LOCAL),
    'GLOBAL_DEPL_TOTAL' = round(GLOBAL_SBMSY/GLOBAL_SB0,2),
    'LOCAL_DEPL_TOTAL' = round(LOCAL_SBMSY/LOCAL_SB0,2),
    'SteepnessH' =  paste(H1,H2,sep = ", "),
    'Movement' = ifelse(PSTAY_A1 == '0.9', 'Fig. 1B',
                        ifelse(PSTAY_A1 == 1, "Fig. 1C",  "Fig. 1D")),
    'Selectivity' = ifelse(SLX_A50_A1  == 9, 'Fig. 1E',
                           ifelse(SLX_A50_A1  ==7, "Fig. 1F","Fig. 1G"))) %>%
  select('Scenario' = SCENARIO_NAME,
         'PropR' = PROPR,
         'Natural Mortality M' = NATM,
         Movement, Selectivity,
         SteepnessH,
         GLOBALFMSY,
         LOCALFMSY,
         SBMSY_RATIO_TOTAL =SBMSY_RATIO,
         # SBMSY_A1_RATIO,
         # SBMSY_A2_RATIO,
         MSY_RATIO,
         GLOBAL_DEPL_TOTAL,
         LOCAL_DEPL_TOTAL) %>%
  write.csv(., file = here('output',paste0(Sys.Date(),'-results.csv')), row.names = FALSE)

# data.frame(surface[,,'global']) %>%
#   filter(FF_Area2 == 1) %>%
#   ggplot(., aes(x = (FF_Area1), y = tyield)) +
#   geom_point()
# 
# data.frame(surface[,,'local']) %>%
#   filter(FF_Area2 == 0) %>%
#   ggplot(., aes(x = (FF_Area1), y = tyield)) +
#   geom_point()
