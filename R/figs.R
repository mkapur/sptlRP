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
  scale_fill_viridis_c() +
  facet_wrap(~Area) +
  labs(x = 'F in Area 1 (age 2)', y = 'F in Area 2 (age 2)', fill = 'SB in area') 

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
  scale_fill_viridis_c(option = 'magma') +
  facet_wrap(~Area) +
  labs(x = 'F in Area 1 (age 2)', y = 'F in Area 2 (age 2)', fill = 'Yield in area') 

FSB/FYIELD 
ggsave(last_plot(),
       height = 10, width = 8, dpi = 520,
       file = here('figs',paste0(Sys.Date(),"-FvsSBandYield_byArea.png")))