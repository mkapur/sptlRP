## Code to replicate Kapur et al (202X)
## Spatial reference points for next-gen assessment models
rm(list = ls())
require(dplyr)
require(here)
require(ggplot2);require(ggsidekick);require(patchwork)
require(reshape2)
require(png);require(grid);require(gridExtra)
source(here("R","fnxs.R"))

## settings
R0_global <- 4 ## used in SRR
scen <- read.csv(here("inputscen.csv")) ## setup
## things to track
coln <- c( "FMSY_NEW","FMSY_GLOBAL","FPROP_NEW","FPROP_GLOBAL",
           "MSY_NEW","MSY_GLOBAL", "SBMSY_NEW","SBMSY_GLOBAL", 
           "A1SB0_new",'A2SB0_new',
           "A1SB0_old",'A2SB0_old',
           "A1DEPL_NEW", "A1DEPL_GLOBAL","A2DEPL_NEW", "A2DEPL_GLOBAL", 'NEW_R0_A1','NEW_R0_A2') ## scenarios are defined by differeniating
scen <- cbind(scen, setNames( lapply(coln, function(x) x=NA), coln) )
SCENNAMES <- scen$SCENARIO_NAME
datlist <- list()
## build datasets to spec (will autosave figure)
for(s in 1:nrow(scen)){
  SCENARIO <- scen[s,'SCENARIO_NAME']
  slx_a50t <- as.numeric(c(scen[s,'SLX_A50_A1'],9))
  slx_a95t <- as.numeric(c(scen[s,'SLX_A95_A1'],13))
  natM <- as.numeric(scen[s,'NATM'])
  
  ## global overwrite based on input
  Rprop_input <<- as.numeric(scen[s,'PROPR'])
  h <<- as.numeric(c(scen[s,'H1'],scen[s,'H2']))
  
  pStayt <- as.numeric(c(scen[s,'PSTAY_A1'],scen[s,'PSTAY_A2']))
  dat <- makeDat(wa = c(5,5),
                 mort = natM,
                     fec_a50 = c(6,6),
                     fec_a95 = c(12,12),
                     slx_a50=slx_a50t,
                     slx_a95=slx_a95t,
                     pStay=pStayt)
  # filetemp <- here('figs',paste0("2021-05-25-h=",paste0(h[1],"_",h[2]),"-",SCENARIO))
  # load(paste0(filetemp,'/dat.rdata'))
  datlist[[s]] <- dat
  FFs <- expand.grid(seq(0,1,0.05),seq(0,1,0.05)) ## instF
  FFs <- expand.grid(seq(0,5,0.1),seq(0,5,0.1)) ## continuous F, will dictate range of yield plot
  out <- makeOut(dat, FFs)
  # out_use <- data.frame(out[,,'new']) ;
  # out_use[which.max(out_use$tyield),]
  # out_use %>% filter(FF_Area2 < 20) %>% select(FF_Area1, FF_Area2, tyield) %>% View()# View(out_use)
  # outold <- data.frame(out[,,'old'])  ;outold %>% select(FF_Area1, FF_Area2, tyield) %>% View()#
  # with(subset(out_use), plot(FF_Area1 +FF_Area2, tyield))
  # with(subset(out_use), plot(FF_Area1 +FF_Area2, tyield))
  # with(subset(outold), plot(FF_Area1 +FF_Area2, tyield))
  propmsytemp <- getMSY()
  out2 <- makeOut2(propmsy=propmsytemp)
  # ## save the max to master table
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
  scen[s,'A1DEPL_GLOBAL'] <- out2[which.max(out2[,'tyield','old']),"SB_A1",'old']/
    out2[which.max(out2[,'tyield','old']),"SB0_A1",'old']
  scen[s,'A2DEPL_GLOBAL'] <- out2[which.max(out2[,'tyield','old']),"SB_A2",'old']/
    out2[which.max(out2[,'tyield','old']),"SB0_A2",'old']
  scen[s,'NEW_R0_A1'] <- out[1,'estRbar','new']*out[1,'estRprop','new']
  scen[s,'NEW_R0_A2'] <- out[1,'estRbar','new']*(1-out[1,'estRprop','new'])

  # ## save stuff; looks to global SCENARIO for filename
  filetemp <- here('output',paste0(Sys.Date(),"-h=",paste0(h[1],"_",h[2]),"-",SCENARIO))
  dir.create(filetemp)
  # lapply(list.files(filetemp, pattern = "*.RDATA", full.names = T), load,environment())
  
  source(here('R','figs.R'))
  save(out, file = paste0(filetemp,'/out.RDATA'))
  save(out2, file =  paste0(filetemp, '/out2.RDATA'))
  save(propmsytemp, file =  paste0(filetemp, '/propmsy.RDATA'))
  save(dat, file = paste0(filetemp,'/dat.rdata'))
  rm(out2);rm(out);rm(propmsytemp);rm(dat)
}
## save all input data
save(datlist, file = here('output',paste0(Sys.Date(),'datlist.rdata')))

## make table 2 using scen
scen[,2:ncol(scen)] <- as.numeric(scen[,2:ncol(scen)] )
scend <- data.frame(scen)
scend[,2:ncol(scen)] <- as.numeric(scend[,2:ncol(scen)] )

data.frame(scen) %>%
  mutate(WA = "Linear increasing function; identical between areas") %>%
  mutate(
    NATM = round(as.numeric(NATM),2),
    'GLOBAL_FMSY_A1' = as.numeric(FMSY_GLOBAL)*as.numeric(FPROP_GLOBAL),
    'LOCAL_FMSY_A1' = as.numeric(FMSY_NEW)*as.numeric(FPROP_NEW),
    'GLOBAL_FMSY_A2' = as.numeric(FMSY_GLOBAL)*(1-as.numeric(FPROP_GLOBAL)),
    'LOCAL_FMSY_A2'= as.numeric(FMSY_NEW)*(1-as.numeric(FPROP_NEW)),
    'GLOBALFMSY' = paste(round(GLOBAL_FMSY_A1,2),round(GLOBAL_FMSY_A2,2), sep = ", "),
    'LOCALFMSY' = paste(round(LOCAL_FMSY_A1,2),round(LOCAL_FMSY_A2,2), sep = ", "),
    'MSY_RATIO' =  round(as.numeric(MSY_GLOBAL)/as.numeric(MSY_NEW),2),
    'GLOBAL_SBMSY' = round(as.numeric(SBMSY_GLOBAL),2),
    'LOCAL_SBMSY' = as.numeric(SBMSY_NEW),
    'GLOBAL_SB0' = as.numeric(A1SB0_old)+as.numeric(A2SB0_old),
    'LOCAL_SB0' = as.numeric(A1SB0_new)+as.numeric(A2SB0_new),
    'GLOBAL_DEPL_TOTAL' = round(GLOBAL_SBMSY/GLOBAL_SB0,2),
    'LOCAL_DEPL_TOTAL' = round(LOCAL_SBMSY/LOCAL_SB0,2)) %>%
  
  select('Scenario' = SCENARIO_NAME,
         'PropR' = 
         # "Localized R0 (Area 1, Area 2)" = LOCALR0,
         # "Weight at age" =WA, 
         'Natural Mortality M' = NATM, 
         'Movement' = PSTAY_A1,
         'Selectivity' = SLX_A50_A1,
         "Steepness h" = H1, 
         GLOBALFMSY,
        LOCALFMSY,
         MSY_RATIO,
         GLOBAL_DEPL_TOTAL,
         LOCAL_DEPL_TOTAL) %>% 
 write.csv(., file = here('figs',paste0(Sys.Date(),'-results.csv')), row.names = FALSE) 



par(mfrow = c(2,4), mar = c(4,4,1,1))

plot(datlist[[1]][,'2',1] ~ age, type = 'p', pch = 19, xlab='age', ylab = vals[v],
     col = alpha('black',0.5), 
     # main = scennames2[1],
     ylim = c(0,ifelse(vals[v]!='weight',1.1,1100)))
  legend('bottomright', legend = c('Area 1','Area 2'), cex = 1.2,
         pch = 19, col = alpha(c('black','blue'),0.5))
points(datlist[[s]][,v,2] ~ age, type = 'p', pch = 19,col = alpha('blue',0.5),)
text(x = 10, y = 1.05, label = LETTERS[1], cex = 1.5)

age <- 0:100
vals <- c('age','proportion_stay','weight','maturity',
          'fishery_selectivity','mortality') ## things to enter into data frame
scennames2 <- SCENNAMES[c(1:3,5:9)]

plotme = c(4,0,1,2,1,2,0,0)
idx = 1
age = 0:100
for(s in 1:length(scennames2)){
  if(plotme[s] == 0) next()
  if(s == 1){
    for(v in 2:5){
     
    }
  } else{
    plot(datlist[[s]][,plotme[s],1] ~ age, type = 'p', pch = 19, xlab='age', ylab = vals[v],
         col = alpha('black',0.5),
         ylim = c(0,ifelse(vals[v]!='weight',1.1,1100)))
    if(s == 6){
      legend('bottomright', legend = c('Area 1','Area 2'), cex = 1.2,
             pch = 19, col = alpha(c('black','blue'),0.5))
    }
    points(datlist[[s]][,plotme[s],2] ~ age, type = 'p', pch = 19,col = alpha('blue',0.5),)
  }

  idx = idx+1
}


 
p1 <- rasterGrob(readPNG("C:/Users/mkapur/Dropbox/UW/sptlRP/figs/2021-05-25-h=0.7_0.7-no movement/no movement-FvsYield_compare.png"  ))

png(here('figs','results_panel.png'), 
    height = 12, width = 6, units = 'in', res = 520)
grid.arrange(rasterGrob(readPNG("C:/Users/mkapur/Dropbox/UW/sptlRP/figs/2021-05-25-h=0.7_0.7-no movement/no movement-FvsYield_compare.png"  ),
                        width = unit(6,"in"), height=unit(3,"in")),
             rasterGrob(readPNG("C:/Users/mkapur/Dropbox/UW/sptlRP/figs/2021-05-25-h=0.7_0.7-A1 sink/A1 sink-FvsYield_compare.png"),
                        width = unit(6,"in"), height=unit(3,"in")),
             rasterGrob(readPNG("C:/Users/mkapur/Dropbox/UW/sptlRP/figs/2021-05-25-h=0.7_0.7-lowerM/lowerM-FvsYield_compare.png" ),
                        width = unit(6,"in"), height=unit(3,"in")))
graphics.off()

pngs <- here('figs',paste0("2021-05-25-",SCENNAMES[c(1:3,5:9)],"-inputDat.png"))

png(here('figs','scenPanel1.png'), 
    height = 8, width = 6, units = 'in', res = 520)
grid.arrange(rasterGrob(readPNG(pngs[1])),
             rasterGrob(readPNG(pngs[2])),
             rasterGrob(readPNG(pngs[3])),
             ncol=3)
graphics.off()
png(here('figs','scenPanel2.png'), 
    height = 8, width = 6, units = 'in', res = 520)
grid.arrange(rasterGrob(readPNG(pngs[4])),
             rasterGrob(readPNG(pngs[5])),
             rasterGrob(readPNG(pngs[6])),
             ncol=3)
graphics.off()


png(here('figs',paste0(Sys.Date(),"-",SCENARIO,'-inputDat.png')),
    width = 8, height = 8, units = 'in', res = 400)
par(mfrow = c(2,2), mar = c(4,4,1,1))
for(v in 2:5){
  age <- 0:100
  plot(dat[,v,1] ~ age, type = 'p', pch = 19, xlab='age', ylab = vals[v],
       col = alpha('black',0.5),
       ylim = c(0,ifelse(vals[v]!='weight',1.1,1100)))
  
  text(x = 10, y = ifelse(vals[v]!='weight',1.05,1050), label = LETTERS[v-1], cex = 1.5)
  if(v == 2){
    legend('bottomright', legend = c('Area 1','Area 2'), cex = 1.2,
           pch = 19, col = alpha(c('black','blue'),0.5))
  }
  points(dat[,v,2] ~ age, type = 'p', pch = 19,col = alpha('blue',0.5),)
}
dev.off()


## master plot with compare
library(grid)
library(png)
library(ggplot2)
library(gridExtra)

plots <- lapply(ll <- list.files(path = here('figs','knitfigs'),
                                 recursive = TRUE,
                                 patt="compare",
                                 full.names = TRUE),
                function(x){
  img <- as.raster(readPNG(x))
  rasterGrob(img, interpolate = FALSE)
})


ggsave(here("figs", paste0(Sys.Date(),"FvsYield_all.png")),
       width=16, height=12, dpi = 520,
       marrangeGrob(grobs = plots, nrow=2, ncol=1,top=NULL))

## master method comparison
scen2 <- scen[,-c(2:5)] %>%
  data.frame() %>%
  melt(id = c("SCENARIO_NAME")) %>%
  mutate(value = as.numeric(value),
         SRC =
           ifelse(is.na(stringr::word(variable,3,sep = "_")),
                  stringr::word(variable,2,sep = "_"),
                  stringr::word(variable,3,sep = "_")),
         variable = stringr::word(variable,1,sep = "_")) %>%
  group_by(SCENARIO_NAME, variable) %>%
  summarise(normVal = value/sum(value), SRC) %>%
  mutate(SCENf = factor(SCENARIO_NAME, levels = SCENNAMES),
         SRC = recode(SRC,GLOBAL = 'SS (global)',
                      NEW = 'Proposed')) 
  ggplot(scen2, aes(x = variable, y = normVal, fill = SRC)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  ggsidekick::theme_sleek() +
  scale_fill_manual(values = c('dodgerblue3','grey66'))+
  labs(x ="", y = "normalized value", fill = '')+
  facet_wrap(~SCENf)
  
ggsave(last_plot(), width = 10, height = 8, dpi= 520,
       file= here("figs", paste0(Sys.Date(),"-MasterCompare.png")))

scen2 %>% 
  group_by(variable,SCENf, SRC) %>%
  filter(variable %in% c('A1DEPL', 'A2DEPL', 'SBMSY')) %>%
  summarise(mean(normVal))
## it's conditional on prop
out2_new %>% filter(Fprop == 0.5)
out2_global %>% filter(Fprop == 0.5)
out2_new %>% filter(FMSY > 0.88 & FMSY < 0.883)
out2_global  %>% filter(FMSY > 0.88 & FMSY < 0.883)

out2[which.max(out2$tyield),]

## sanity checking Ralston O'Farrel 2008  CJFAS
data.frame(out[,,1]) %>%
  ggplot(., aes(x = ralstonR_A1, color = FF_Area1)) +
  geom_point( aes(y =expR_A1 ) )+
  geom_point( aes( y = obsR_A1)) +
  geom_abline(xintercept = 0, slope = 1, col = 'red')

data.frame(out[,,2]) %>%
  ggplot(., aes(x = ralstonR_A1, color = FF_Area1)) +
  geom_point( aes(y =expR_A1 ) )+
  geom_point( aes( y = obsR_A1)) +
  geom_abline(xintercept = 0, slope = 1, col = 'red')


data.frame(out[,,1]) %>%
  ggplot(., aes(x = ralstonR_A2, color = FF_Area2)) +
  geom_point( aes(y =expR_A2 ) )+
  geom_point( aes( y = obsR_A2)) +
  geom_abline(xintercept = 0, slope = 2, col = 'red')

## sorting out why there's a turnaround ----
out2_new <- data.frame(out2[,,'new'])

out2_new[which(out2_new$FF_Area2 > 0.62 ),] %>%
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = tyield)) +
  geom_tile() 

merge(out2[,,'new'] %>% 
  data.frame() %>% 
  select(F_Sys= FMSY  , 
         F_A1= FF_Area1  , 
         F_A2=FF_Area2   ) %>%
  melt() %>%
  mutate(area = substr(variable,3,5)) ,
  out2[,,'new'] %>% 
  data.frame() %>% 
  select(  Y_A1=Yield_A1   ,
           Y_A2= Yield_A2 ,
           Y_Sys= tyield   ) %>%
melt() %>%
  mutate(area = substr(variable,3,5)) ,
by = 'area', all = FALSE) %>%
  select(area, FF = value.x, Yield = value.y) %>%
  ggplot(., aes(x = FF, y = Yield, group = area )) +
  geom_line()



iso1b <- out[,,'new'] %>% data.frame() %>% 
  # filter(FF_Area2== 0.5) %>%
  # select(Yield_A1 )%>%
  mutate(tyield = SB_A1+SB_A2) %>%
  select(FF_Area1,FF_Area2,tyield  ) %>%
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(yield = value) %>%
  select(-variable,-value) 





iso1[which.max(iso1$yield),'FF_Area2']

iso2 <- out[,,'new'] %>% data.frame() %>%
  # filter(FF_Area1== 0.5) %>%
  # select(Yield_A2)%>%
  select(FF_Area1,FF_Area2,tyield) %>%  
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,7,8), yield = value) %>%
  select(-variable,-value) #%>%
  filter( FF_Area2 > 0.62) 

## create a vector which indicates whether this isocline's pieak
## is above or below the true FMSY for all combos
## should show that 
peak_max <- iso1 %>%
  group_by(FF_Area2) %>%
  summarise( )

peak_max2 <- iso2 %>%
  group_by(FF_Area1) %>%
  summarise(my=max(yield) )
iso2 <- merge(iso2, peak_max2, by = "FF_Area1", all.y = FALSE)

ggplot(data = iso2, aes(y = yield)) +
  geom_line(aes(x = FF_Area2, color = FF_Area1, group = FF_Area1), lwd = 1.1) +
  # geom_line(data = iso2, aes(x = FF_Area2, color = FF_Area1, group = FF_Area1), lwd = 1.1) +
  ggsidekick::theme_sleek() +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,100)) +
  # scale_color_manual(values = c('grey44','purple'), labels = c('Area 1','Area 2')) +
  labs(x = 'F in Area 2', title = 'Yield Isoclines by Area',
       # subtitle = 'Alternative Area F = 0.05; removed runs with any negative yield',
       y = 'Yield Total', color = 'F in Area 1 (isocline)') +
  geom_vline( data= iso1, col = 'red',
              aes(  xintercept  = out2_new[which.max(out2_new[,'tyield']),
                                           'FF_Area2'])) +
  geom_hline( data= peak_max2, col = 'purple',
              aes(  yintercept  = my)) +
  # geom_vline( data= iso1, col = 'blue',
  #             aes(  xintercept  = 0.62)) +
  # geom_text(data = iso2,aes(x= 0.8, y= 80, label = round(my,0)), 
  #           check_overlap = TRUE) +
  # geom_point(data = peak_max2,aes( x= FF_Area1, y= my), color = 'purple') +
  facet_wrap(~FF_Area1)
  
ggsave(last_plot(),
       height = 10, width = 8, dpi = 520,
       file = here('figs',paste0(Sys.Date(),"-Isocline_By_Area.png")))
# 
# 
#   
# ggplot(propmsy, aes(y = FMSY,x = Fprop)) +
#   geom_line(lwd = 1.1) +
#   scale_y_continuous(limits = c(0.5,1)) +
#   theme_sleek() +
#   labs(x = 'Proportion F applied to Area 1', y = 'FMSY')
# ggsave(last_plot(), file = here('figs',paste0(Sys.Date(),'propFvsMSY.png')))
# 
# 
# out_use2 %>%
#   ggplot(., aes(y = FMSY,x = Fprop, color =tyield )) +
#   geom_point() +
#   scale_y_continuous(limits = c(0.5,1)) +
#   scale_color_viridis_c(na.value = 'white') +
#   theme_sleek() +
#   labs(x = 'Proportion F applied to Area 1', y = 'FMSY', color = 'Total Yield') +
#   geom_vline(xintercept = out_use2[which.max(out_use2$tyield),'Fprop'], linetype = 'dashed' ) +
#   annotate('text', 
#            x = out_use2[which.max(out_use2$tyield),'Fprop']*1.1,
#            y = out_use2[which.max(out_use2$tyield),'FMSY']*1.1, 
#            size = 3,
#            color ='seagreen',
#            label = as.expression(bquote(MSY~"="~.(round(out_use2[which.max(out_use2$tyield),'tyield']))))) +
#   annotate('text', 
#            x = out_use2[which.max(out_use2$tyield),'Fprop']*1.1,
#            y = out_use2[which.max(out_use2$tyield),'FMSY']*1.09, 
#            size = 3,
#            color ='seagreen',
#            label = as.expression(bquote(F[MSY]~"="~.(round(out_use2[which.max(out_use2$tyield),'FMSY'],2))))) +
#   
#   ggsave(last_plot(), file = here('figs',paste0(Sys.Date(),'propFvsMSY_tyield.png')))




