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

## settings and storage----
R0_global <- 1 ## used in SRR
scen <- read.csv(here("inputscen.csv")) ## setup

## things to track
coln <- c( "FMSY_NEW","FMSY_GLOBAL","FPROP_NEW","FPROP_GLOBAL",
           "MSY_NEW","MSY_GLOBAL", "SBMSY_NEW","SBMSY_GLOBAL", 
           "A1SB0_new",'A2SB0_new',
           "A1SB0_old",'A2SB0_old',
           "A1DEPL_NEW", "A1DEPL_GLOBAL","A2DEPL_NEW", "A2DEPL_GLOBAL" )
scen <- cbind(scen, setNames( lapply(coln, function(x) x=NA), coln) )
SCENNAMES <- scen$SCENARIO_NAME
datlist <- list()


## run simulations ----
# for(s in 1:nrow(scen)){
for(s in 1:7){
  SCENARIO <- scen[s,'SCENARIO_NAME']
  slx_a50t <- as.numeric(c(scen[s,'SLX_A50_A1'],9))
  slx_a95t <- as.numeric(c(scen[s,'SLX_A95_A1'],13))
  natM <- as.numeric(scen[s,'NATM'])
  Rprop_input <<- as.numeric(scen[s,'PROPR'])
  h <<- as.numeric(c(scen[s,'H1'],scen[s,'H2']))
  pStayt <- as.numeric(c(scen[s,'PSTAY_A1'],scen[s,'PSTAY_A2']))
  if(length(grep('WAA', SCENARIO)) == 0 ){
    dat <- makeDat(wa = NULL, ## default wa
                   mort = natM,
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
  # filetemp <- here('output',paste0(Sys.Date(),"-h=",paste0(h[1],"_",h[2]),"-",SCENARIO))
  # load(paste0(filetemp,'/dat.rdata'))
  # lapply(list.files(filetemp, pattern = "*.RDATA", full.names = T), load,environment())
  # lapply(list.files(filetemp, pattern = "*.rdata", full.names = T), load,environment())
  datlist[[s]] <- dat
  FFs <- expand.grid(seq(0,5,0.05),seq(0,5,0.05)) ## continuous F, will dictate range of yield plot
  out <- makeOut(dat, FFs)
  propmsytemp <- getMSY()
  out2 <- makeOut2(propmsy=propmsytemp)
  ## save the max to master table
  scen[s,'FMSY_NEW'] <- out2[which.max(out2[,'tyield','new']),'FMSY','new']
  scen[s,'FPROP_NEW'] <- out2[which.max(out2[,'tyield','new']),'Fprop','new']
  scen[s,'MSY_NEW'] <- out2[which.max(out2[,'tyield','new']),'tyield','new']
  scen[s,'SBMSY_NEW'] <- sum(out2[which.max(out2[,'tyield','new']),"SB_A1",'new'],
                             out2[which.max(out2[,'tyield','new']),"SB_A2",'new'])
  scen[s,'A1DEPL_NEW'] <- out2[which.max(out2[,'tyield','new']),"SB_A1",'new']/
    out2[which.max(out2[,'tyield','new']),"SB0_A1",'new']
  scen[s,'A2DEPL_NEW'] <- out2[which.max(out2[,'tyield','new']),"SB_A2",'new']/
    out2[which.max(out2[,'tyield','new']),"SB0_A2",'new']
  scen[s,'A1SB0_new'] <-  out2[which.max(out2[,'tyield','new']),"SB0_A1",'new']
  scen[s,'A2SB0_new'] <-  out2[which.max(out2[,'tyield','new']),"SB0_A2",'new']
  scen[s,'A1SB0_old'] <-  out2[which.max(out2[,'tyield','new']),"SB0_A1",'old']
  scen[s,'A2SB0_old'] <-  out2[which.max(out2[,'tyield','new']),"SB0_A2",'old']
  scen[s,'FMSY_GLOBAL'] <- out2[which.max(out2[,'tyield','old']),'FMSY','old']
  scen[s,'FPROP_GLOBAL'] <- out2[which.max(out2[,'tyield','old']),'Fprop','old']
  scen[s,'MSY_GLOBAL'] <- out2[which.max(out2[,'tyield','old']),'tyield','old']
  scen[s,'SBMSY_GLOBAL'] <- sum(out2[which.max(out2[,'tyield','old']),"SB_A1",'old'],
                                out2[which.max(out2[,'tyield','old']),"SB_A2",'old'])

  scen[s,'SBMSY_A1_RATIO'] <- out2[which.max(out2[,'tyield','old']),"SB_A1",'old']/
                                out2[which.max(out2[,'tyield','new']),"SB_A1",'new']

  scen[s,'SBMSY_A2_RATIO'] <- out2[which.max(out2[,'tyield','old']),"SB_A2",'old']/
                                  out2[which.max(out2[,'tyield','new']),"SB_A2",'new']

  scen[s,'A1DEPL_GLOBAL'] <- out2[which.max(out2[,'tyield','old']),"SB_A1",'old']/
    out2[which.max(out2[,'tyield','old']),"SB0_A1",'old']
  scen[s,'A2DEPL_GLOBAL'] <- out2[which.max(out2[,'tyield','old']),"SB_A2",'old']/
    out2[which.max(out2[,'tyield','old']),"SB0_A2",'old']
  
  ## save stuff; looks to SCENARIO for filename
  # filetemp <- here('output',paste0(Sys.Date(),"-h=",paste0(h[1],"_",h[2]),"-",SCENARIO))
  # dir.create(filetemp)
  source(here('R','figs.R'))
  # save(out, file = paste0(filetemp,'/out.RDATA'))
  # save(out2, file =  paste0(filetemp, '/out2.RDATA'))
  # save(propmsytemp, file =  paste0(filetemp, '/propmsy.RDATA'))
  # save(dat, file = paste0(filetemp,'/dat.rdata'))
  # rm(out2);rm(out);rm(propmsytemp);rm(dat)
}

## save all input data ----
save(datlist, file = here('output',paste0(Sys.Date(),'datlist.rdata')))
save(scen, file = here('output',paste0(Sys.Date(),'scen.rdata')))

## Make table 2 ----
data.frame(scen) %>%
  mutate(WA = "Linear increasing function; identical between areas") %>%
  mutate(
    NATM = round(as.numeric(-log(NATM)),2),
    'GLOBAL_FMSY_A1' = as.numeric(FMSY_GLOBAL)*as.numeric(FPROP_GLOBAL),
    'LOCAL_FMSY_A1' = as.numeric(FMSY_NEW)*as.numeric(FPROP_NEW),
    'GLOBAL_FMSY_A2' = as.numeric(FMSY_GLOBAL)*(1-as.numeric(FPROP_GLOBAL)),
    'LOCAL_FMSY_A2'= as.numeric(FMSY_NEW)*(1-as.numeric(FPROP_NEW)),
    'GLOBALFMSY' = paste(round(GLOBAL_FMSY_A1,2),round(GLOBAL_FMSY_A2,2), sep = ", "),
    'LOCALFMSY' = paste(round(LOCAL_FMSY_A1,2),round(LOCAL_FMSY_A2,2), sep = ", "),
    
    'MSY_RATIO' =  round(as.numeric(MSY_GLOBAL)/as.numeric(MSY_NEW),2), 
    'GLOBAL_SBMSY' = round(as.numeric(SBMSY_GLOBAL),2),
    'LOCAL_SBMSY' = as.numeric(SBMSY_NEW),
    
    'SBMSY_RATIO' = round(GLOBAL_SBMSY/LOCAL_SBMSY,2),
    'SBMSY_A1_RATIO' = round(as.numeric(SBMSY_A1_RATIO),2),
    'SBMSY_A2_RATIO' = round(as.numeric(SBMSY_A2_RATIO),2),
    
    'GLOBAL_SB0' = as.numeric(A1SB0_old)+as.numeric(A2SB0_old),
    'LOCAL_SB0' = as.numeric(A1SB0_new)+as.numeric(A2SB0_new),
    'GLOBAL_DEPL_TOTAL' = round(GLOBAL_SBMSY/GLOBAL_SB0,2),
    'LOCAL_DEPL_TOTAL' = round(LOCAL_SBMSY/LOCAL_SB0,2),
    'SteepnessH' =  paste(H1,H2,sep = ", "),
    'Movement' = ifelse(PSTAY_A1 == '0.9', 'Fig. 1B',
                        ifelse(PSTAY_A1 == 1, "Fig. 1C",  "Fig. 1D")),
    'Selectivity' = ifelse(SLX_A50_A1  == '9', 'Fig. 1E', 
                           ifelse(SLX_A50_A1  ==7, "Fig. 1F","Fig. 1G"))) %>%
                          
  
  select('Scenario' = SCENARIO_NAME,
         'PropR' = PROPR,
         'Natural Mortality M' = NATM, 
         Movement, Selectivity,
         SteepnessH,
         GLOBALFMSY,
         LOCALFMSY,
         SBMSY_RATIO_TOTAL =SBMSY_RATIO,
         SBMSY_A1_RATIO,
         SBMSY_A2_RATIO,
         MSY_RATIO,
         GLOBAL_DEPL_TOTAL,
         LOCAL_DEPL_TOTAL) %>% 
  write.csv(., file = here('output',paste0(Sys.Date(),'-results.csv')), row.names = FALSE) 

