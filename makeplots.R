## inputs by area ----
png( here('figs','inputs_by_area.png'),
     width = 6, height = 8, unit = 'in', res = 520)
par(mfrow = c(narea,3), 
    mar = c(4,5,1,1))
for(a in 1:narea){
  dattemp <- as.data.frame(dat[,,a])
  names(dattemp) = names(dat0)
  ## F Fecundity
  with(dattemp, plot(Fecundity ~ Age, type = 'l', lwd = 2, yaxt = 'n', ylim = c(0,1.05),
                     ylab = 'Fecundity', col = 'seagreen4',  cex.lab= 1.5))
  axis(2, at  = seq(0,1.0,0.25), labels = seq(0,1.0,0.25), cex.axis =1.5)
  ## M/F weights
  with(dattemp, plot(Wght.f. ~ Age, type = 'l', lwd = 2, ylim = c(0,3), 
                     ylab = 'Weight', col = 'seagreen4',  cex.axis =1.5, cex.lab= 1.5))
  
  
  with(dattemp, lines(Wght.m. ~ Age, lwd = 2, ylab = 'Weight', col = 'goldenrod',  cex.axis =1.5, cex.lab= 1.5))
  legend('bottomright', legend = c('Female','Male'), col = c('seagreen4','goldenrod'), lwd = 2)
  ## M/F selex
  with(dattemp, plot(Sel.f. ~ Age, type = 'l', lwd = 2,  yaxt = 'n', ylim = c(0,1.05), ylab = 'Selectivity', col = 'seagreen4',  cex.axis =1.5, cex.lab= 1.5))
  with(dattemp, lines(Sel.m. ~ Age, lwd = 2, ylab = 'Selectivity', col = 'goldenrod',  cex.axis =1.5, cex.lab= 1.5))
  axis(2, at  = seq(0,1.0,0.25), labels = seq(0,1.0,0.25), cex.axis =1.5)
  legend('bottomright', legend = c('Female','Male'), col = c('seagreen4','goldenrod'), lwd = 2)
}
dev.off()

## Xija ----
plist = list()
for(g in 1:6){ ## loop ages
  plist[[g]] <-  data.frame(X_ija[,,g]) %>% 
    mutate('FRM' = 1:3) %>%
    melt(id = 'FRM') %>%
    ggplot(., aes(x = FRM, y = variable, fill = value)) +
    geom_tile() + 
    ggsidekick::theme_sleek() +
    theme(legend.position = 'none',
          axis.ticks = element_blank(),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10)) +
    scale_x_continuous(expand = c(0,0), breaks = 1:3, labels = paste("Area",1:narea)) +
    scale_y_discrete(expand = c(0,0), labels = paste("Area",1:narea))+
    geom_text(aes(label = value), colour = 'white', size = 10) +
    labs(x = 'Source', y = 'Sink', 
         title = ifelse(g < 6, paste('Age ',g), "Ages 6+")) 
  
}

ggsave(Rmisc::multiplot(plotlist = plist, 
                        layout = matrix(c(1,2,3,4,5,6),nrow = 2, byrow = TRUE) ),
       file = here('figs','X_ija.png'),
       width = 10, height = 8, unit = 'in', dpi = 520)

## Nums at age ----
doNage(s = 1,  Fv = rep(0,narea), eq_method == 'STD')[,1:3] %>%
  data.frame() %>%
  mutate(Age = 1:nages) %>%
  reshape2::melt(id = 'Age') %>%
  mutate(Area = substr(variable,2,2)) %>%
  ggplot(., aes(x = Age, y = value, col = Area)) +
  geom_line(lwd = 1.1) + 
  scale_color_grey(labels = paste("Area",1:3)) +
  labs(x = 'Age', y = 'Relative Numbers', color = '') +
  theme_sleek() + 
  theme(legend.position = c(0.8,0.9), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 20))

ggsave(last_plot(), 
       file = here('figs','Nage_STD.png'),
       width = 6, height = 4, unit = 'in', dpi = 520)

doNage(s = 1,  Fv = rep(0,narea), eq_method = 'STB')[,1:3] %>%
  data.frame() %>%
  mutate(Age = 1:nages) %>%
  reshape2::melt(id = 'Age') %>%
  mutate(Area = substr(variable,2,2)) %>%
  ggplot(., aes(x = Age, y = value, col = Area)) +
  geom_line(lwd = 1.1) + 
  scale_color_grey(labels = paste("Area",1:3)) +
  labs(x = 'Age', y = 'Relative Numbers', color = '') +
  theme_sleek() + 
  theme(legend.position = c(0.8,0.9), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 20))
ggsave(last_plot(), 
       file = here('figs','Nage_STB.png'),
       width = 6, height = 4, unit = 'in', dpi = 520)

rbind(
doNage(s = 1,  Fv = rep(0,narea), eq_method = 'STD')[,1:3] %>%
  data.frame() %>%
  mutate(Age = 1:nages, EQ = 'STD')%>%
  reshape2::melt( id = c('Age','EQ')),
doNage(s = 1,  Fv = rep(0,narea), eq_method = 'TIME')[,1:3] %>%
  data.frame() %>%
  mutate(Age = 1:nages, EQ = 'TIME')%>%
  reshape2::melt( id = c('Age','EQ')),
doNage(s = 1,  Fv = rep(0,narea), eq_method = 'STB')[,1:3] %>%
  data.frame() %>%
  mutate(Age = 1:nages, EQ = 'STB')%>%
  reshape2::melt( id = c('Age','EQ'))) %>%
  mutate(Area = substr(variable,2,2)) %>% 
  select(-variable) %>%
  mutate(group = paste0("Area ",Area," ",EQ)) %>%
  ggplot(., aes(x = Age, y = value, col = Area, linetype = group)) +
  geom_line(lwd = 1.1) + 
  scale_color_grey() +
  scale_linetype_manual(values = rep(c('solid','dashed','dotted'),3))+
  scale_y_continuous(limits = c(0,1.5)) +
  labs(x = 'Age', y = 'Relative Numbers', color = '', linetype = "") +
  theme_sleek() + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 20)) +
  facet_wrap(~EQ)

ggsave(last_plot(), 
       file = here('figs','Nage_All.png'),
       width = 10, height = 8, unit = 'in', dpi = 520)
## B0 ----
doNage(s = 1)[,7:9] %>%
  data.frame() %>%
  mutate(Age = 1:nages) %>%
  reshape2::melt(id = 'Age') %>%
  mutate(Area = substr(variable,2,2)) %>%
  ggplot(., aes(x = Age, y = value, col = Area)) +
  geom_line(lwd = 1.1) + 
  scale_color_grey(labels = paste("Area",1:3)) +
  labs(x = 'Age', y = 'Unfished Biomass', color = '') +
  theme_sleek() + 
  theme(legend.position = c(0.8,0.9), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 20))
ggsave(last_plot(), 
       file = here('figs','biomass.png'),
       width = 6, height = 4, unit = 'in', dpi = 520)

## SB0 ----
doNage(s = 1)[,10:12] %>%
  data.frame() %>%
  mutate(Age = 1:nages) %>%
  reshape2::melt(id = 'Age') %>%
  mutate(Area = substr(variable,2,2)) %>%
  ggplot(., aes(x = Age, y = value, col = Area)) +
  geom_line(lwd = 1.1) + 
  scale_color_grey(labels = paste("Area",1:3)) +
  labs(x = 'Age', y = 'Spawning Biomass', color = '') +
  theme_sleek() + 
  theme(legend.position = c(0.8,0.9), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 20))
ggsave(last_plot(), 
       file = here('figs','SSB.png'),
       width = 6, height = 4, unit = 'in', dpi = 520)

## Brute Y----
brute <- data.frame('FV_sys' = NA, "yield_1" = NA, "yield_2" = NA,"yield_3" = NA,
                    "ypr_1" = NA, "ypr_2" = NA,"ypr_3" = NA)
Fv_test <- seq(0,1,0.01)
for( v in 1:length(Fv_test)){
  temp <- masterFunc(SRR = 1, h = steep, Fv = rep(Fv_test[v],narea))
  temp2 <- doYPR(Fv = rep(Fv_test[v],narea))
  brute[v,'FV_sys'] = Fv_test[v]
  for(a in 1:narea) brute[v,a+1] = temp$yield[a]
  for(a in 1:narea) brute[v,a+4] = temp2[[2]][a]
}
brute[,c(1:4)] %>%
  melt(id = 'FV_sys') %>%
  ggplot(., aes(x = FV_sys, y = value, color = variable)) +
  geom_line(lwd = 1.1) + 
  scale_color_grey(labels = paste("Area",1:3)) +
  labs(x = 'F in area', y = 'Yield', color = '') +
  scale_y_continuous(limits = c(0,0.3)) +
  # scale_x_continuous(limits = c(0,0.5)) +
  theme_sleek() + 
  theme(legend.position = c(0.8,0.9), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 20))

ggsave(last_plot(), 
       file = here('figs','bruteYield.png'),
       width = 6, height = 4, unit = 'in', dpi = 520)
## YPR - should look logistic-ish ----
# brute[,c(1,5:7)] %>%
#   melt(id = 'FV_sys') %>%
#   ggplot(., aes(x = FV_sys, y = value, color = variable)) +
#   geom_line(lwd = 1.1) + 
#   scale_color_grey(labels = paste("Area",1:3)) +
#   labs(x = 'F in area', y = 'YPR', color = '') +
#   # scale_y_continuous(limits = c(0,0.5)) +
#   theme_sleek() + 
#   theme(legend.position = c(0.8,0.9), 
#         axis.title = element_text(size = 16),
#         axis.text = element_text(size = 16),
#         legend.text = element_text(size = 20))
## Brute Config ----
## takes about 4 mins with 5k subset
brute_config <- data.frame(expand.grid(Area = 1:3, FA1 = seq(0,0.5,0.1),
                                       FA2 =  seq(0,0.5,0.1), 
                                       FA3 =  seq(0,0.5.,0.1)),
                           "yield_1" = NA, "yield_2" = NA,"yield_3" = NA)
# brute_config <- sample_n(brute_config, 5000)
# brute_config <- brute_config[1:5000,] ## xy need to be increasing
for(i in 1:nrow(brute_config)){
  temp <- masterFunc(SRR = 1, h = steep, Fv = with(brute_config[i,], c(FA1,FA2,FA3)))
  # temp2 <- doYPR(Fv = rep(Fv_test[i],narea))
  for(a in 1:narea) brute_config[i,a+4] = temp$yield[a]
  # for(a in 1:narea) brute[i,a+4] = temp2[[2]][a]
  if(i %% 100 == 0) cat(i,"\n")
}
brute_config$IDX <- 1:nrow(brute_config)


## try a 3d plot
require(plot3D)
with(
  subset(brute_config ),
  scatter3D(
    x = FA3,
    y = FA1,
    z = yield_3,
    pch = 18,
    theta = 40,
    # type = 'l',
    phi = 20,
    colvar = yield_3
  )
)


# with(brute_config, scatter3D(FA2,FA3,yield_2, pch = 18, theta = 15, phi = 20))

# plot_ly(brute_config, x=~FA1,y = ~FA2, z = ~FA3, type = 'surface')
bc <- cbind(brute_config[,c(2:4,8)] %>%
              melt(id = 'IDX') %>%
              mutate(Area = substr(variable,3,3)) %>% select(value, Area),
            
            brute_config[,c(5:8)] %>%
              melt(id = 'IDX') %>%
              mutate(Area = substr(variable,7,7)) %>% select(value, Area, IDX))  
names(bc)[c(1,3)] <- c("Fv","Yield")
bc <- bc[,c(1:3,5)]
# bc <- bc[!is.na(bc)]


ggplot(bc, aes(x = Fv, y = Yield, color = IDX))+
  # geom_line(lwd = 1.1) +
  geom_point() +
  # scale_color_grey(labels = paste("Area",1:3)) +
  labs(x = 'F in area', y = 'Yield', color = '') +
  # scale_y_continuous(limits = c(0,0.5)) +
  theme_sleek() + 
  theme(legend.position = c(0.8,0.9), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 20)) +
  facet_wrap(~factor(Area))

