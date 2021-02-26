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

## matrix of scnearios, including name
scen <- matrix(NA, nrow = 4, ncol = 5)
colnames(scen) <- c("SCENARIO_NAME",'SLX_A50_A1','SLX_A95_A1','PSTAY_A1','PSTAY_A2') ## scenarios are defined by differeniating
scen[,'SCENARIO_NAME'] <- c('no_move','just_move','just_slx','move_slx') ## scen 3 is AAF
scen[,'SLX_A50_A1'] <- c(9,9,5,5) ## lower slx when different
scen[,'SLX_A95_A1'] <- c(13,13,10,10) ## lower slx when different
scen[,'PSTAY_A1'] <- c(1,0.9,1,0.9) 
scen[,'PSTAY_A2'] <- c(1,0.6,1,0.6) 
scen[,2:5] <- as.numeric(scen[,2:5])
## Scenario 1 ----

## build datasets to spec (will autosave figure)
for(s in 1:nrow(scen)){
  SCENARIO <- scen[s,'SCENARIO_NAME']
  slx_a50t <- as.numeric(c(scen[s,'SLX_A50_A1'],9))
  slx_a95t <- as.numeric(c(scen[s,'SLX_A95_A1'],13))
  pStayt <- as.numeric(c(scen[s,'PSTAY_A1'],scen[s,'PSTAY_A2']))
  
  dattemp <- makeDat(wa = c(5,5),
                     fec_a50 = c(6,6),
                     fec_a95 = c(12,12),
                     slx_a50=slx_a50t,
                     slx_a95=slx_a95t,
                     pStay=pStayt)
  FFs <- expand.grid(seq(0,1,0.05),seq(0,1,0.05))
  out <- makeOut(dat=dattemp, FFs)
  propmsytemp<- getMSY()
  out2 <- makeOut2(propsmy=propmsytemp)
  source(here('R','figs.R')) ## looks to global SCENARIO for filename
  rm(out2);rm(out);rm(propmsytemp);rm(dattemp)
  
  
}




