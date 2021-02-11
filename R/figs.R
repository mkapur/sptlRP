out[out < 0] <- NA

FSB <- out %>%
  select(FF_Area1,FF_Area2,SB_A1,SB_A2) %>%
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,4,5), SB= value) %>%
  select(-variable,-value) %>%
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = SB)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() + 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(na.value = 'white' ) +
  facet_wrap(~Area) +
  labs(x = 'F in Area 1', y = 'F in Area 2', fill = 'SB in area') 

FYIELD <- out %>%
  select(FF_Area1,FF_Area2,Yield_A1,Yield_A2) %>%
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,7,8), yield = value) %>%
  select(-variable,-value) %>%
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = yield)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() + 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(option = 'magma',na.value = 'white') +
  facet_wrap(~Area) +
  labs(x = 'F in Area 1', y = 'F in Area 2', fill = 'Yield in area') 

FSB/FYIELD 
ggsave(last_plot(),
       height = 10, width = 8, dpi = 520,
       file = here('figs',paste0(Sys.Date(),"-FvsSBandYield_byArea.png")))


##  F vs Composite SSB and Yield plots ----
FSB <- out %>%
  mutate(SB_Total = SB_A1+SB_A2) %>%
  select(FF_Area1,FF_Area2, SB_Total) %>%
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,4,5), SB= value) %>%
  select(-variable,-value) %>%
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = SB)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(na.value = 'white') +
  labs(x = 'F in Area 1', 
       y = 'F in Area 2', 
       fill = 'Total SB') 

FYIELD <- out %>%
  mutate(Yield_Total = Yield_A1+Yield_A2) %>%
  select(FF_Area1,FF_Area2, Yield_Total) %>%  
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,7,8), yield = value) %>%
  select(-variable,-value) %>% 
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = yield)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() + 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(option = 'magma',na.value = 'white') +
  labs(x = 'F in Area 1', 
       y = 'F in Area 2',
       fill = 'Total Yield') 



FSB/FYIELD
ggsave(last_plot(),
       height = 10, width = 8, dpi = 520,
       file = here('figs',paste0(Sys.Date(),"-FvsSBandYield_Total.png")))



## isoclines (not as useful) ---

iso1 <- out %>%
  filter(FF_Area2== 0.5) %>%
  select(FF_Area1,Yield_A1) %>%
  reshape2::melt(id = c("FF_Area1")) %>%
  mutate(Area = substr(variable,7,8), yield = value) %>%
  select(-variable,-value) 
  
iso2 <- out %>%
          filter(FF_Area1== 0.5) %>%
          select(FF_Area2,Yield_A2) %>%
          reshape2::melt(id = c("FF_Area2")) %>%
          mutate(Area = substr(variable,7,8), yield = value) %>%
          select(-variable,-value)

ggplot(iso1, aes(x = FF_Area1, y = yield, color = Area)) +
  geom_line(lwd = 1.1) +
  geom_line(data = iso2, aes(x = FF_Area2), lwd = 1.1) +
  ggsidekick::theme_sleek() + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_color_manual(values = c('grey44','blue'), labels = c('Area 1','Area 2')) +
  labs(x = 'F', title = 'Yield Isoclines by Area',
       subtitle = 'Alternative Area F = 0.05; removed runs with any negative yield',
       y = 'Yield', color = '') 

ggsave(last_plot(),
       height = 10, width = 8, dpi = 520,
       file = here('figs',paste0(Sys.Date(),"-Isocline_Total.png")))
