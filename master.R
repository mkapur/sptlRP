## Code to replicate Kapur et al (202X)
## Spatial reference points for next-gen assessment models
rm(list = ls())
require(dplyr)
require(here)
require(ggplot2);require(ggsidekick);require(patchwork)
require(reshape2)

source(here("R","fnxs.R"))

## settings, unchanged 
R0_global <- 4
Rprop_input <- 0.65
steep = c(0.7,0.7)

## matrix of scnearios, including name
## some notes on scenarios:
## slx only are like AAF. 
## if name is "move" it uses the A1_SINK setup.
## A2 SINK is simply the reverse of A1 sink
## move light has less lopsitded movement w A1 still as SINK
SCENNAMES <- c('Symmetrical Movement','A1 Sink','A2 Sink',
               'A1 Low Selex', 'A1 Low Selex + Move','A1 Hi Selex + Move')

scen <- matrix(NA, nrow = length(SCENNAMES), ncol =17)
colnames(scen) <- c("SCENARIO_NAME",'SLX_A50_A1','SLX_A95_A1','PSTAY_A1','PSTAY_A2',
                    "FMSY_NEW","FMSY_GLOBAL","FPROP_NEW","FPROP_GLOBAL",
                    "MSY_NEW","MSY_GLOBAL", "SB_MSY_NEW","SB_MSY_GLOBAL",
                    "A1DEPL_NEW", "A1DEPL_GLOBAL","A2DEPL_NEW", "A2DEPL_GLOBAL") ## scenarios are defined by differeniating
scen[,'SCENARIO_NAME'] <-SCENNAMES
scen[,'SLX_A50_A1'] <- c(9,9,9,7,7,11) ## lower slx when different
scen[,'SLX_A95_A1'] <- c(13,13,13,11,11,15) ## lower slx when different
scen[,'PSTAY_A1'] <- c(0.5,0.9,0.6,0.5,0.9,0.9) 
scen[,'PSTAY_A2'] <- c(0.5,0.6,0.9,0.5,0.6,0.6) 
scen[,2:ncol(scen)] <- as.numeric(scen[,2:ncol(scen)])

## build datasets to spec (will autosave figure)
for(s in 1:nrow(scen)){
  SCENARIO <- scen[s,'SCENARIO_NAME']
  slx_a50t <- as.numeric(c(scen[s,'SLX_A50_A1'],9))
  slx_a95t <- as.numeric(c(scen[s,'SLX_A95_A1'],13))
  pStayt <- as.numeric(c(scen[s,'PSTAY_A1'],scen[s,'PSTAY_A2']))
  dat <- makeDat(wa = c(5,5),
                     fec_a50 = c(6,6),
                     fec_a95 = c(12,12),
                     slx_a50=slx_a50t,
                     slx_a95=slx_a95t,
                     pStay=pStayt)
  FFs <- expand.grid(seq(0,0.9999,0.05),seq(0,0.9999,0.05))
  out <- makeOut(dat, FFs)
  propmsytemp <- getMSY()
  out2 <- makeOut2(propmsy=propmsytemp)
  
  ## save the max to master table
  scen[s,'FMSY_NEW'] <- out2[which.max(out2[,'tyield','new']),'FMSY','new']
  scen[s,'FPROP_NEW'] <- out2[which.max(out2[,'tyield','new']),'Fprop','new']
  scen[s,'MSY_NEW'] <- out2[which.max(out2[,'tyield','new']),'tyield','new']
  scen[s,'SB_MSY_NEW'] <- sum(out2[which.max(out2[,'tyield','new']),"SB_A1",'new'],
                              out2[which.max(out2[,'tyield','new']),"SB_A2",'new'])
  scen[s,'A1DEPL_NEW'] <- out2[which.max(out2[,'tyield','new']),"SB_A1",'new']/
                              out2[which.max(out2[,'tyield','new']),"SB0_A1",'new']
  scen[s,'A2DEPL_NEW'] <- out2[which.max(out2[,'tyield','new']),"SB_A2",'new']/
    out2[which.max(out2[,'tyield','new']),"SB0_A2",'new']
  
  
  scen[s,'FMSY_GLOBAL'] <- out2[which.max(out2[,'tyield','old']),'FMSY','old']
  scen[s,'FPROP_GLOBAL'] <- out2[which.max(out2[,'tyield','old']),'Fprop','old']
  scen[s,'MSY_GLOBAL'] <- out2[which.max(out2[,'tyield','old']),'tyield','old']
  scen[s,'SB_MSY_GLOBAL'] <- sum(out2[which.max(out2[,'tyield','old']),"SB_A1",'old'],
                              out2[which.max(out2[,'tyield','old']),"SB_A2",'old'])
  scen[s,'A1DEPL_GLOBAL'] <- out2[which.max(out2[,'tyield','old']),"SB_A1",'old']/
    out2[which.max(out2[,'tyield','old']),"SB0_A1",'old']
  scen[s,'A2DEPL_GLOBAL'] <- out2[which.max(out2[,'tyield','old']),"SB_A2",'old']/
    out2[which.max(out2[,'tyield','old']),"SB0_A2",'old']
  ## save stuff; looks to global SCENARIO for filename
  filetemp <- here('figs',paste0(Sys.Date(),"-h=",paste0(steep[1],"_",steep[2]),"-",SCENARIO))
  dir.create(filetemp)
  source(here('R','figs.R')) 
  save(out, file = paste0(filetemp,'/out.RDATA'))
  save(out2, file =  paste0(filetemp, '/out2.RDATA'))
  save(propmsytemp, file =  paste0(filetemp, '/propmsy.RDATA'))
  
  rm(out2);rm(out);rm(propmsytemp);rm(dat)
}

## master method comparison
scen[,-c(2:5)] %>%
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
         SRC = recode(SRC,GLOBAL = 'SS (global)',NEW = 'Proposed')) %>%
  ggplot(., aes(x = variable, y = normVal, fill = SRC)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  ggsidekick::theme_sleek() +
  scale_fill_manual(values = c('goldenrod','seagreen4'))+
  labs(x ="", y = "normalized value", fill = '')+
  facet_wrap(~SCENf)
  
ggsave(last_plot(), width = 10, height = 8, dpi= 520,
       file= here("figs", paste0(Sys.Date,"-MasterCompare.png")))


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




