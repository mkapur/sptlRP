
# 
# FSB <- out_use %>%
#   select(FF_Area1,FF_Area2,SB_A1,SB_A2) %>%
#   reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
#   mutate(Area = substr(variable,4,5), SB= value) %>%
#   select(-variable,-value) %>%
#   ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = SB)) +
#   geom_tile() +
#   coord_equal() +
#   ggsidekick::theme_sleek() + 
#   scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
#   scale_fill_viridis_c(na.value = 'white' ) +
#   facet_wrap(~Area) +
#   labs(x = 'F in Area 1', y = 'F in Area 2', fill = 'SB in area') 
# 
# FYIELD <- out_use %>%
#   select(FF_Area1,FF_Area2,Yield_A1,Yield_A2) %>%
#   reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
#   mutate(Area = substr(variable,7,8), yield = value) %>%
#   select(-variable,-value) %>%
#   ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = yield)) +
#   geom_tile() +
#   coord_equal() +
#   ggsidekick::theme_sleek() + 
#   scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
#   scale_fill_viridis_c(option = 'magma',na.value = 'white') +
#   facet_wrap(~Area) +
#   labs(x = 'F in Area 1', y = 'F in Area 2', fill = 'Yield in area') 
# 
# FSB/FYIELD 
# ggsave(last_plot(),
#        height = 10, width = 8, dpi = 520,
#        file = here('figs',paste0(Sys.Date(),"-FvsSBandYield_byArea.png")))
# 
# 
# ##  F vs Composite SSB and Yield plots ----
# ## heatmap with msy overlaid
# FSB <- out_use %>%
#   mutate(SB_Total = SB_A1+SB_A2) %>%
#   select(FF_Area1,FF_Area2, SB_Total) %>%
#   reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
#   mutate(Area = substr(variable,4,5), SB= value) %>%
#   select(-variable,-value) %>%
#   ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = SB)) +
#   geom_tile() +
#   coord_equal() +
#   ggsidekick::theme_sleek() + theme(legend.position = 'top') +  
#   scale_x_continuous(expand = c(0,0)) + 
#   scale_y_continuous(expand = c(0,0)) +
#   scale_fill_viridis_c(na.value = 'white') +
#   labs(x = 'F in Area 1', 
#        y = 'F in Area 2', 
#        fill = 'Total SB') 
# 
# FYIELD <-
# out_use %>%
#   mutate(Yield_Total = Yield_A1+Yield_A2) %>%
#   select(FF_Area1,FF_Area2, Yield_Total) %>%  
#   reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
#   mutate(Area = substr(variable,7,8), yield = value) %>%
#   select(-variable,-value) %>% 
#   ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = yield)) +
#   geom_tile() +
#   coord_equal() +
#   ggsidekick::theme_sleek() + theme(legend.position = 'top') +
#   scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
#   scale_fill_viridis_c(option = 'magma',na.value = 'white') +
#   scale_color_viridis_c(option = 'magma',na.value = 'white') +
#   ## add the locations of FMSY
#   geom_point(data = out_use2, aes(x = FF_Area1, y = FF_Area2), color = 'grey66', size = 2)+
#   geom_point(data = out_use2, aes(x = out_use2[which.max(tyield),'FF_Area1'],
#                               y = out_use2[which.max(tyield),'FF_Area2']), 
#              color = 'purple', size = 2, pch =15)+
#   annotate('text',
#            x = out_use2[which.max(out_use2$tyield),'FF_Area1']*1.15,
#            y = out_use2[which.max(out_use2$tyield),'FF_Area2']*1.15, 
#            size = 3,
#            color ='purple',
#            label = as.expression(bquote(MSY[Total]~"="~.(round(out_use2[which.max(out_use2$tyield),'tyield']))))) +
#   annotate('text', 
#            x = out_use2[which.max(out_use2$tyield),'FF_Area1']*1.17,
#            y = out_use2[which.max(out_use2$tyield),'FF_Area2']*1.07, 
#            size = 3,
#            color ='purple',
#            label = as.expression(bquote(F[MSY_Total]~"="~.(round(out_use2[which.max(out_use2$tyield),'FMSY'],2))))) +
#   labs(x = 'F in Area 1',   y = 'F in Area 2', fill = 'Total Yield') 
# 
# 
# 
# FSB |FYIELD
# ggsave(last_plot(),
#        height = 10, width = 8, dpi = 520,
#        file = here('figs',paste0(Sys.Date(),"-FvsSBandYield_Total.png")))



out2_new <- data.frame(out2[,,'new'])
# out2_new[out2_new < 0] <- 0
out2_global <- data.frame(out2[,,'old'])
# out2_global[out2_global < 0] <- 0

out_use <- data.frame(out[,,'new']) 
outold <- data.frame(out[,,'old']) 
# out_use[out_use < 0] <- 0


depl <- data.frame(out[,,'old'])  %>%
  mutate(depl_A1 = ((SB_A1)/(SB0_A1)),
         depl_A2 = ((SB_A2)/(SB0_A2))) %>%  
  select(FF_Area1,FF_Area2,depl_A1,depl_A2) %>%
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,6,7), depl = value) %>%
  mutate(SRC = 'GLOBAL') %>%
  select(-variable,-value) %>% 
  rbind(.,
        out_use %>%
          mutate(depl_A1 = ((SB_A1)/(SB0_A1)),
                 depl_A2 = ((SB_A2)/(SB0_A2))) %>%  
          select(FF_Area1,FF_Area2,depl_A1,depl_A2) %>%
          reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
          mutate(Area = substr(variable,6,7), depl = value) %>%
          mutate(SRC = 'PROPOSED') %>%
          select(-variable,-value)) %>%
  
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = depl)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() +
  theme(legend.position = 'right') +
  # scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = expansion(add = c(0, 0)), limits = c(0,1)) +
  scale_x_continuous(expand = c(0,0)) + 
  scale_fill_viridis_c(option = 'magma',na.value = 'white') +
  scale_color_viridis_c(option = 'magma',na.value = 'white') +
  facet_wrap(~Area+SRC) +
  labs(x = 'F in Area 1', y = 'F in Area 2', fill = 'Depletion (SB/SB0)',
       title = SCENARIO)
ggsave(depl,
       height = 10, width = 8, dpi = 520,
       file =  paste0(filetemp,"/",SCENARIO,"depletion.png"))




iso1 <- out_use %>%
  select(FF_Area1,FF_Area2,tyield) %>%
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,7,8), yield = value) %>%
  select(-variable,-value)

ggplot(data = iso1, aes(y = yield)) +
  geom_line(aes(x = FF_Area1, color = FF_Area2, group = FF_Area2), lwd = 1.1) +
  # geom_line(data = iso2, aes(x = FF_Area2, color = FF_Area1, group = FF_Area1), lwd = 1.1) +
  ggsidekick::theme_sleek() +
  # scale_x_continuous(expand = c(0,0), limits = c(0,2)) +
  # scale_y_continuous(expand = c(0,0), limits = c(0,100)) +
  # scale_color_manual(values = c('grey44','purple'), labels = c('Area 1','Area 2')) +
  labs(x = 'F in Area 1', title = paste0(SCENARIO,' Yield Isocline Conditional on FA1'),
       # subtitle = 'For low values of F1 there is some interchange among high F2;
       # recall that the FMSY needs to be at the peak',
       y = 'Yield Total', color = 'F in Area 2 (isocline)') +
  geom_point(data = out2_new,
             aes(x = FF_Area1, y = tyield,
                 color = FF_Area2), size = 2) +
  # geom_text(data = out2_new, aes(x = FF_Area1, y = tyield, label = round(FF_Area2,2)),
  #           color = 'white',
  #           size = 2) 

ggsave(last_plot(),
       height = 10, width = 8, dpi = 520,
       file =  paste0(filetemp,"/",SCENARIO,"-Isocline_rollwave.png"))

# https://stackoverflow.com/questions/24237399/how-to-select-the-rows-with-maximum-values-in-each-group-with-dplyr
# may=iso1 %>%
#   group_by(FF_Area1) %>%
#   filter(yield == max(yield)) %>%
#   arrange(FF_Area1, FF_Area2) %>% select(-Area)
out_use <- data.frame(out[,,'new']) 

new <- out_use %>%
  select(FF_Area1,FF_Area2, Yield_Total =tyield) %>%  
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  # mutate(Area = substr(variable,7,8), yield = value) %>%
  mutate(yield = value) %>%
  select(-variable,-value) %>% 
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = yield)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() +
  theme(legend.position = 'top') +

  scale_x_continuous(expand = c(0,0), limits = c(0,max(out_use$FF_Area1))) +
  scale_y_continuous(expand = expansion(add = c(0, 0)), limits = c(0,max(out_use$FF_Area2))) +
  # scale_x_continuous(expand = c(0,0), limits = c(0,1)) +
  scale_fill_viridis_c(option = 'magma',na.value = 'white') +
  scale_color_viridis_c(option = 'magma',na.value = 'white') +
  ## add the locations of FMSY from new method
  # geom_point(data = out2_new, aes(x = FF_Area1, y = FF_Area2), 
  #            fill = NA,
  #            size = 2, alpha = 0.3) +
  geom_point(data = out2_new, aes(x = out2_new[which.max(out2_new[,'tyield']),'FF_Area1'],
                                  y = out2_new[which.max(out2_new[,'tyield']),'FF_Area2']), 
             fill = NA, color = 'purple', size = 2, pch =15)+
  annotate('text',
           x =0.8*max(out_use$FF_Area1),
           y = 0.85*max(out_use$FF_Area2),
           size = 3,
           color = 'gold',
           label = as.expression(bquote(MSY[Local]~
                                          "="~.(round(out2_new[which.max(out2_new[,'tyield']),
                                                               'tyield']))))) +
  annotate('text',
           x =0.8*max(out_use$FF_Area1),
           y = 0.8*max(out_use$FF_Area2),
           size = 3,
           color ='gold',
           label = as.expression(bquote(F[MSY_Local]~"="~.
                                        (round(out2_new[which.max(out2_new$tyield),'FMSY'],2))))) +
 
  labs(x = 'F in Area 1',   y = 'F in Area 2', fill = 'Total Yield',  title = SCENARIO) 

##  F vs recruitment
fr_new <- out_use %>%
  # mutate(obsR_total = obsR_A1+obsR_A2) %>%
  mutate(obsR_total = expR_A1+expR_A2) %>%
  select(FF_Area1,FF_Area2, obsR_total) %>%  
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,7,8), obsR = value) %>%
  select(-variable,-value) %>% 
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = obsR)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() + theme(legend.position = 'top') +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(option = 'magma',na.value = 'white') +
  labs(x = 'F in Area 1',   y = 'F in Area 2', fill = 'Recruitment',  title = SCENARIO) 


ggsave(fr_new  ,
       height = 10, width = 8, dpi = 520,
       file =  paste0(filetemp,"/",SCENARIO,"-FvsR_Total.png"))
cat(paste0('saved ', Sys.Date(),"-",SCENARIO,"-FvsR_Total.png", '\n'))

out_use <- data.frame(out[,,'old']) 
out_use[out_use < 0] <- 0
global <- out_use %>%
  select(FF_Area1,FF_Area2, Yield_Total =tyield) %>%  
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  # mutate(Area = substr(variable,7,8), yield = value) %>%
  mutate(yield = value) %>%
  select(-variable,-value) %>% 
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = yield)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() + theme(legend.position = 'top') +
  scale_x_continuous(expand = c(0,0), limits = c(0,max(out_use$FF_Area1))) +
  scale_y_continuous(expand = c(0,0), limits = c(0,max(out_use$FF_Area2))) +
  # scale_x_continuous(expand = c(0,0)) + 
  # scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(option = 'cividis') +
  
  ## add the locations of FMSY from global method
  # geom_point(data = out2_global, aes(x = FF_Area1, y = FF_Area2),fill = NA, 
  #            color = 'navy', size = 2, alpha = 0.3) +
  geom_point(data = out2_global, aes(x = out2_global[which.max(out2_global[,'tyield']),'FF_Area1'],
                                  y = out2_global[which.max(out2_global[,'tyield']),'FF_Area2']),
             fill = NA, color = 'navy', size = 2, pch =15)+
  annotate('text',
           x =0.8*max(out_use$FF_Area1),
           y = 0.85*max(out_use$FF_Area2),
           size = 3,
           color ='gold',
           label = as.expression(bquote(MSY[Global]~
                                          "="~.(round(out2_global[which.max(out2_global[,'tyield']),
                                                               'tyield']))))) +
  annotate('text',
           x =0.8*max(out_use$FF_Area1),
           y = 0.8*max(out_use$FF_Area2),
           size = 3,
           color ='gold',
           label = as.expression(bquote(F[MSY_Global]~"="~.
                                        (round(out2_global[which.max(out2_global$tyield),'FMSY'],2))))) +
  labs(x = 'F in Area 1',   y = 'F in Area 2', fill = 'Total Yield',  title = SCENARIO) 

ggsave(new    | global,
       height = 8, width = 10, dpi = 520,
       file =   paste0(filetemp,"/",SCENARIO,"-FvsYield_compare.png"))
cat(paste0('saved ',Sys.Date(),"-",SCENARIO,"-FvsYield_compare.png", '\n'))


# 
