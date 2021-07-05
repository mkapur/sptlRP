## Wtage----
png(paste0(filetemp,"/",SCENARIO,"-wtage.png"), width = 6, height = 4, unit = 'in', res = 420)
plot(dat[,'weight',1], ylim = c(0,max(dat[,'weight',1])), xlab = 'age',ylab = 'weight');lines(dat[,'weight',2])
legend('bottomright', legend = c('area1','area2'), pch = c(1,NA), lty = c(NA,1))
dev.off()



out_use %>% mutate(SB_A1+ SB_A2) %>% head()
outglobal %>% mutate(SB_A1+ SB_A2) %>% head()

out2_local <- data.frame(surface[,,'local'])
out2_global <- data.frame(surface[,,'global'])

out_use <- data.frame(surface[,,'local']) 
outglobal <- data.frame(surface[,,'global']) 

## Isocline yield curve ----

iso_local <- data.frame(surface[,,'local']) %>%
  filter( round(FF_Area2,1) == 
            round(out2_local[which.max(out2_local[,'tyield']),c('FF_Area2')],1) ) %>%
  select(FF_Area1,FF_Area2,tyield) %>%
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,7,8), yield = value) %>%
  select(-variable,-value)

iso_global <- data.frame(surface[,,'global']) %>%
  filter( round(FF_Area2,1) == 
            round(out2_local[which.max(out2_local[,'tyield']),c('FF_Area2')],1) ) %>%
  select(FF_Area1,FF_Area2,tyield) %>%
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,7,8), yield = value) %>%
  select(-variable,-value) 

ggplot(data = iso_local, aes(y = yield)) +
  geom_line(aes(x = FF_Area1, linetype = 'LOCAL', color = 'LOCAL'), lwd = 1.1) +
  geom_line(data = iso_global, aes(x = FF_Area1, linetype = 'GLOBAL', color = 'GLOBAL'), lwd = 1.1) +
  ggsidekick::theme_sleek() +
  geom_vline(aes(xintercept = out2_local[which.max(out2_local[,'tyield']),c('FF_Area1')],
                 lty = 'LOCAL', color = 'LOCAL')) +
  geom_vline(aes(xintercept = out2_global[which.max(out2_global[,'tyield']),c('FF_Area1')],
                 lty = 'GLOBAL', color = 'GLOBAL')) +
  scale_color_manual(values = c('black','blue'), labels = c('GLOBAL','LOCAL')) +
  # scale_x_continuous(limits = c(0,1.5)) +
  # scale_y_continuous(limits = c(0,1500)) +
  labs(x = 'F in Area 1 (F Area 2 = FMSY Area 2)', 
       title = paste0(SCENARIO,' Yield Isocline Conditional on FA1'),
       subtitle = 'vertical line indicates FMSY in Area',
       y = 'Yield Total', linetype = "assumption") 

ggsave(last_plot(),
       height = 10, width = 8, dpi = 520,
       file =  paste0(filetemp,"/",SCENARIO,"-F1Yield_F2FMSY.png"))

## Local assumption panel ----
# https://stackoverflow.com/questions/24237399/how-to-select-the-rows-with-maximum-values-in-each-group-with-dplyr

out_use <- data.frame(surface[,,'local']) 
# maxf1 <- out2_local[which.max(out2_local[,'tyield']),c('FF_Area1')]*1.5
# maxf1 = max(out_use$FF_Area1)
maxf1 = 1.5

local <- out_use %>%
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
  scale_x_continuous(expand = c(0,0), limits = c(0,maxf1)) +
  scale_y_continuous(expand = expansion(add = c(0, 0)), limits = c(0,maxf1)) +
  # scale_fill_viridis_c(option = 'magma',na.value = 'white') +
  # scale_color_viridis_c(option = 'magma',na.value = 'white') +
  scale_fill_gradient2(low = "white", high = "grey11")+
  scale_color_gradient2()+
  ## add the locations of FMSY from local method
  # geom_point(data = out2_local, aes(x = FF_Area1, y = FF_Area2), 
  #            fill = NA,
  #            size = 2, alpha = 0.3) +
  geom_point(data = out2_local, aes(x = out2_local[which.max(out2_local[,'tyield']),'FF_Area1'],
                                    y = out2_local[which.max(out2_local[,'tyield']),'FF_Area2']), 
             fill = NA, color = 'gglobal', size = 2, pch =15)+
  annotate('text',
           x =0.6*maxf1,
           y = 0.85*maxf1,
           size = 5,
           color = 'gglobal',
           label = as.expression(bquote(MSY[Local]~
                                          "="~.(round(out2_local[which.max(out2_local[,'tyield']),
                                                                 'tyield']))))) +
  annotate('text',
           x =0.6*maxf1,
           y = 0.8*maxf1,
           size = 5,
           color ='gglobal',
           label = as.expression(bquote(F[MSY_Local]~"="~.(round(out2_local[which.max(out2_local$tyield),'FF_Area1'],2))~"Area 1, "~.
                                        (round(out2_local[which.max(out2_local$tyield),'FF_Area2'],2))~"Area 2"))) +
  
  labs(x = 'F in Area 1',   y = 'F in Area 2', fill = 'Total Yield',  title = SCENARIO) 

## global assumption panel ----
out_use <- data.frame(surface[,,'global']) 
out_use[out_use < 0] <- 0
# maxf1 <- out2_global[which.max(out2_global[,'tyield']),c('FF_Area1')]*1.5
# maxf1 = max(out_use$FF_Area1)
maxf1 = 1.5
global <- out_use %>%
  select(FF_Area1,FF_Area2, Yield_Total =tyield) %>%  
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(yield = value) %>%
  select(-variable,-value) %>% 
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = yield)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() + theme(legend.position = 'top',
                                    plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  scale_x_continuous(expand = c(0,0), limits = c(0,maxf1)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,maxf1)) +
  # scale_fill_viridis_c(option = 'viridis') +
  scale_fill_gradient2(low = "white", high = "grey11")+
  # scale_color_gradient2(low = "black", high = "white")+
  ## add the locations of FMSY from global method
  # geom_point(data = out2_global, aes(x = FF_Area1, y = FF_Area2),fill = NA, 
  #            color = 'navy', size = 2, alpha = 0.3) +
  geom_point(data = out2_global, aes(x = out2_global[which.max(out2_global[,'tyield']),'FF_Area1'],
                                     y = out2_global[which.max(out2_global[,'tyield']),'FF_Area2']),
             fill = NA, color = 'gglobal', size = 2, pch =15)+
  annotate('text',
           x =0.6*maxf1,
           y = 0.85*maxf1,
           size = 5,
           color ='gglobal',
           label = as.expression(bquote(MSY[Global]~
                                          "="~.(round(out2_global[which.max(out2_global[,'tyield']),
                                                                  'tyield']))))) +
  annotate('text',
           x =0.6*maxf1,
           y = 0.8*maxf1,
           size = 5,
           color ='gglobal',
           label = as.expression(bquote(F[MSY_Global]~"="~.(round(out2_global[which.max(out2_global$tyield),'FF_Area1'],2))~"Area 1, "~.
                                        (round(out2_global[which.max(out2_global$tyield),'FF_Area2'],2))~"Area 2"))) +
  labs(x = 'F in Area 1',   y = 'F in Area 2', fill = 'Total Yield',  title = SCENARIO) 

ggsave(local    | global,
       height = 8, width = 10, dpi = 520,
       file =   paste0(filetemp,"/",SCENARIO,"-FvsYield_compare.png"))
cat(paste0('saved ',Sys.Date(),"-",SCENARIO,"-FvsYield_compare.png", '\n'))


## NAA Panel ---- 
SCENNAMES = scen[,1]
for(s in 1:nrow(scen)){
  png(here('figs',paste0(scen[s,1],'-testdoPR-movement-PANEL.png')),
      width = 10, height =8, unit = 'in', res = 520)
  
  layout.matrix <- matrix(c(1:6), ncol = 3, byrow = T)
  
  layout(mat = layout.matrix) 
  
  WAA <-  matrix(c(dat$dat[Ages+1,'weight',1], dat$dat[Ages+1,'weight',2]),nrow=2,ncol=Nages, byrow = T)
  Sel <- matrix(c(dat$dat[Ages+1,'fishery_selectivity',1], dat$dat[Ages+1,'fishery_selectivity',2]),nrow=2,ncol=Nages, byrow = T)
  Fec <- matrix(c(Sel[1,]*WAA[1,], Sel[2,]*WAA[2,]),nrow=2,ncol=Nages, byrow = T)
  
  tmp <- doNAA(F1=0,F2=0, usedat =datlist[[s]], Sel)
  # N_Z_F <- doNAA(F1, F2, usedat = dat, Sel)
  # tmp <- doPR(, FF = c(0,0)) ## defaults, no fishing
  par(mar = c(0,4,1.5,1))
  ## spawn-src F0
  plot(tmp$N[1,,1], lwd = 2,
       ylim = c(0,1),   
       xlab = 'Age',
       xaxt = 'n',
       type = 'l',   
       cex.main = 1.5,
       main = as.expression(bquote(Spawned~"in"~"a1,"~F~"="~0)),
       ylab = 'Numbers-per-recruit' );
  lines(tmp$N[2,,1], lwd = 2, col = 'blue') ## should look reasonable
  axis(side = 2, at = seq(0,1,0.2), labels =seq(0,1,0.2) )
  text(x = 50, y = 0.7, label = SCENNAMES[s], cex = 1.5)
  
  par(mar = c(0,0,1.5,1))
  plot(tmp$N[1,,2],type = 'l',
       cex.main = 1.5,
       main = as.expression(bquote(Spawned~"in"~"a2,"~F~"="~0)),
       ylim = c(0,1),
       ylab = 'Numbers-per-recruit',
       xaxt = 'n',
       yaxt = 'n',
       lwd = 2,  )
  lines(tmp$N[2,,2],lwd = 2, col = 'blue') ## should look reasonable
  text(x = 50, y = 0.7, label = SCENNAMES[s], cex = 1.5)
  
  ## total Ns F0
  par(mar = c(0,4,1.5,1))
  plot(rowSums(tmp$N[1,,]),type = 'l',
       main = as.expression(bquote(Total~"Numbers,"~F~"="~0)),
       ylim = c(0,1),
       cex.main = 1.5,
       ylab = 'Numbers-at-Age',
       xaxt = 'n',
       lwd = 2,  )
  lines(rowSums(tmp$N[2,,]),lwd = 2, col = 'blue', type = 'p') ## should look reasonable
  legend('topright',lwd = 2, legend = c('Present in a2', 'Present in a1'), col = rev(c('black','blue')))
  text(x = 50, y = 0.7, label = SCENNAMES[s], cex = 1.5)
  
  FFvec <- c(scen[s,'FMSY_LOCAL_A1'],scen[s,'FMSY_LOCAL_A2'])
  
  tmp <- doNAA(F1=FFvec[1],F2=FFvec[2], usedat =datlist[[s]], Sel)
  
  ## spawn-src fmsy
  par(mar = c(4,4,1.5,1))
  plot(tmp$N[1,,1], lwd = 2,
       ylim = c(0,1),   xlab = 'Age',type = 'l',     cex.main = 1.5,
       main = as.expression(bquote(Spawned~"in"~"a1,"~F~"="~F[MSY])),
       ylab = 'Numbers-per-recruit' );
  lines(tmp$N[2,,1], lwd = 2, col = 'blue') ## should look reasonable
  axis(side = 2, at = seq(0,1,0.2), labels =seq(0,1,0.2) )
  text(x = 50, y = 0.7, label = SCENNAMES[s], cex = 1.5)
  par(mar = c(4,0,1.5,1))
  plot(tmp$N[1,,2],type = 'l',
       cex.main = 1.5,
       main = as.expression(bquote(Spawned~"in"~"a2,"~F~"="~F[MSY])),
       ylim = c(0,1),
       yaxt = 'n',
       ylab = 'Numbers-per-recruit',
       xlab = 'Age',
       lwd = 2,  )
  lines(tmp$N[2,,2],lwd = 2, col = 'blue') ## should look reasonable
  text(x = 50, y = 0.7, label = SCENNAMES[s], cex = 1.5)
  
  ## TOTAL NS FMSY
  par(mar = c(4,4,1.5,1))
  plot(rowSums(tmp$N[1,,]),type = 'l',
       main = as.expression(bquote(Total~"Numbers,"~F~"="~F[MSY])),
       ylim = c(0,1),
       cex.main = 1.5,
       ylab = 'Numbers-at-Age',
       # xaxt = 'n',
       lwd = 2,  )
  lines(rowSums(tmp$N[2,,]),lwd = 2, col = 'blue', type = 'p') ## should look reasonable
  legend('topright',lwd = 2, legend = c('Present in a2', 'Present in a1'), col = rev(c('black','blue')))
  text(x = 50, y = 0.7, label = SCENNAMES[s], cex = 1.5)
  
  dev.off()
}

