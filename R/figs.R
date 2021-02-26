
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
out2_new[out2_new < 0] <- NA
out2_global <- data.frame(out2[,,'old'])
out2_global[out2_global < 0] <- NA

out_use <- data.frame(out[,,'new']) 
out_use[out_use < 0] <- NA
new <- out_use %>%
  select(FF_Area1,FF_Area2, Yield_Total =tyield) %>%  
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  # mutate(Area = substr(variable,7,8), yield = value) %>%
  mutate(yield = value) %>%
  select(-variable,-value) %>% 
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = yield)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() + theme(legend.position = 'top') +
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(option = 'magma',na.value = 'white') +
  scale_color_viridis_c(option = 'magma',na.value = 'white') +
  ## add the locations of FMSY from new method
  geom_point(data = out2_new, aes(x = FF_Area1, y = FF_Area2), fill = NA,
             color = 'purple', size = 2, alpha = 0.3) +
  geom_point(data = out2_new, aes(x = out2_new[which.max(out2_new[,'tyield']),'FF_Area1'],
                                  y = out2_new[which.max(out2_new[,'tyield']),'FF_Area2'],
                                  fill = tyield),
             color = 'purple', size = 2, pch =15)+
  annotate('text',
           x = out2_new[which.max(out2_new[,'tyield']),'FF_Area1']*0.85,
           y = out2_new[which.max(out2_new[,'tyield']),'FF_Area2']*1, 
           size = 3,
           color = 'purple',
           label = as.expression(bquote(MSY[Optim]~
                                          "="~.(round(out2_new[which.max(out2_new[,'tyield']),
                                                               'tyield']))))) +
  annotate('text',
           x = out2_new[which.max(out2_new$tyield),'FF_Area1']*0.85,
           y = out2_new[which.max(out2_new$tyield),'FF_Area2']*0.9,
           size = 3,
           color ='purple',
           label = as.expression(bquote(F[MSY_Optim]~"="~.
                                        (round(out2_new[which.max(out2_new$tyield),'FMSY'],2))))) +
  ## add the locations of FMSY from global method
  # geom_point(data = out2_global, aes(x = FF_Area1, y = FF_Area2, fill = tyield), 
  #            color = 'navy', size = 2, alpha = 0.3) +
  # geom_point(data = out2_global, aes(x = out2_global[which.max(out2_global[,'tyield']),'FF_Area1'],
  #                                    y = out2_global[which.max(out2_global[,'tyield']),'FF_Area2'],
  #                                    fill = tyield),
  #            color = 'navy', size = 2, pch =15)+
  # annotate('text',
  #          x = out2_global[which.max(out2_global[,'tyield']),'FF_Area1']*1.1,
  #          y = out2_global[which.max(out2_global[,'tyield']),'FF_Area2']*1.15, 
  #          size = 3,
  #          color ='navy',
  #          label = as.expression(bquote(MSY[Global]~
  #                                         "="~.(round(out2_global[which.max(out2_global[,'tyield']),
  #                                                                 'tyield']))))) +
  # annotate('text',
  #          x = out2_global[which.max(out2_global$tyield),'FF_Area1']*1.12,
  #          y = out2_global[which.max(out2_global$tyield),'FF_Area2']*0.95,
  #          size = 3,
  #          color ='navy',
  #          label = as.expression(bquote(F[MSY_Global]~"="~.
  #                                       (round(out2_global[which.max(out2_global$tyield),'FMSY']))))) +
  labs(x = 'F in Area 1',   y = 'F in Area 2', fill = 'Total Yield') 

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
  scale_fill_viridis_c(option = 'magma',na.value = 'white') 



ggsave(fr_new  ,
       height = 10, width = 8, dpi = 520,
       file = here('figs',paste0(Sys.Date(),"-",SCENARIO,"-FvsR_Total.png")))
cat(paste0('saved ', Sys.Date(),"-",SCENARIO,"-FvsR_Total.png", '\n'))

out_use <- data.frame(out[,,'old']) 
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
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(option = 'cividis') +
  ## add the locations of FMSY from new method
  # geom_point(data = out2_new, aes(x = FF_Area1, y = FF_Area2, fill = tyield), 
  #            color = 'purple', size = 2, alpha = 0.3) +
  # geom_point(data = out2_new, aes(x = out2_new[which.max(out2_new[,'tyield']),'FF_Area1'],
  #                                 y = out2_new[which.max(out2_new[,'tyield']),'FF_Area2'],
  #                                 fill = tyield),
  #            color = 'purple', size = 2, pch =15)+
  # annotate('text',
  #          x = out2_new[which.max(out2_new[,'tyield']),'FF_Area1']*0.85,
  #          y = out2_new[which.max(out2_new[,'tyield']),'FF_Area2']*1, 
  #          size = 3,
  #          color = 'purple',
  #          label = as.expression(bquote(MSY[Optim]~
  #                                         "="~.(round(out2_new[which.max(out2_new[,'tyield']),
  #                                                              'tyield']))))) +
  # annotate('text',
  #          x = out2_new[which.max(out2_new$tyield),'FF_Area1']*0.85,
  #          y = out2_new[which.max(out2_new$tyield),'FF_Area2']*0.9,
  #          size = 3,
  #          color ='purple',
  #          label = as.expression(bquote(F[MSY_Optim]~"="~.
  #                                       (round(out2_new[which.max(out2_new$tyield),'FMSY'],2))))) +
  ## add the locations of FMSY from global method
  geom_point(data = out2_global, aes(x = FF_Area1, y = FF_Area2), 
             color = 'navy', size = 2, alpha = 0.3) +
  geom_point(data = out2_global, aes(x = out2_global[which.max(out2_global[,'tyield']),'FF_Area1'],
                                  y = out2_global[which.max(out2_global[,'tyield']),'FF_Area2']),
             color = 'navy', size = 2, pch =15)+
  annotate('text',
           x = out2_global[which.max(out2_global[,'tyield']),'FF_Area1']*1.1,
           y = out2_global[which.max(out2_global[,'tyield']),'FF_Area2']*1.15, 
           size = 3,
           color ='navy',
           label = as.expression(bquote(MSY[Global]~
                                          "="~.(round(out2_global[which.max(out2_global[,'tyield']),
                                                               'tyield']))))) +
  annotate('text',
           x = out2_global[which.max(out2_global$tyield),'FF_Area1']*1.12,
           y = out2_global[which.max(out2_global$tyield),'FF_Area2']*0.95,
           size = 3,
           color ='navy',
           label = as.expression(bquote(F[MSY_Global]~"="~.
                                        (round(out2_global[which.max(out2_global$tyield),'FMSY']))))) +
  labs(x = 'F in Area 1',   y = 'F in Area 2', fill = 'Total Yield') 

ggsave(new    | global,
       height = 10, width = 8, dpi = 520,
       file = here('figs',paste0(Sys.Date(),"-",SCENARIO,"-FvsYield_compare.png")))
cat(paste0('saved ',Sys.Date(),"-",SCENARIO,"-FvsYield_compare.png", '\n'))


# 
# ## isoclines (not as useful) ---
# 
# iso1 <- out_use %>%
#   filter(FF_Area2== 0.5) %>%
#   select(FF_Area1,Yield_A1) %>%
#   reshape2::melt(id = c("FF_Area1")) %>%
#   mutate(Area = substr(variable,7,8), yield = value) %>%
#   select(-variable,-value) 
#   
# iso2 <- out_use %>%
#           filter(FF_Area1== 0.5) %>%
#           select(FF_Area2,Yield_A2) %>%
#           reshape2::melt(id = c("FF_Area2")) %>%
#           mutate(Area = substr(variable,7,8), yield = value) %>%
#           select(-variable,-value)
# 
# ggplot(iso1, aes(x = FF_Area1, y = yield, color = Area)) +
#   geom_line(lwd = 1.1) +
#   geom_line(data = iso2, aes(x = FF_Area2), lwd = 1.1) +
#   ggsidekick::theme_sleek() + 
#   scale_x_continuous(expand = c(0,0)) + 
#   scale_y_continuous(expand = c(0,0)) +
#   scale_color_manual(values = c('grey44','purple'), labels = c('Area 1','Area 2')) +
#   labs(x = 'F', title = 'Yield Isoclines by Area',
#        subtitle = 'Alternative Area F = 0.05; removed runs with any negative yield',
#        y = 'Yield', color = '') 
# 
# ggsave(last_plot(),
#        height = 10, width = 8, dpi = 520,
#        file = here('figs',paste0(Sys.Date(),"-Isocline_Total.png")))
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
