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
# doNage(s = 1,  Fv = rep(0,narea), eq_method == 'STD')[,1:3] %>%
doNage( Fv = rep(0,narea), X = X_ija)$N_ai %>%
  data.frame() %>%
  mutate(Age = 1:nages) %>%
  reshape2::melt(id = 'Age') %>%
  mutate(Area = substr(variable,2,2)) %>%
  ggplot(., aes(x = Age-1, y = value, col = Area)) +
  geom_line(lwd = 1.1) + 
  scale_color_grey(labels = paste("Area",1:3)) +
  labs(x = 'Age', y = 'Total Numbers', color = '') +
  theme_sleek() + 
  theme(legend.position = c(0.8,0.9), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 20))

ggsave(last_plot(), 
       file = here('figs','Nage_Rick_NoMovEqualRec.png'),
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
doNage( Fv = rep(0,narea), X = X_ija_NULL)$B_ai %>%
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
       file = here('figs','biomass_rick_NoMov.png'),
       width = 6, height = 4, unit = 'in', dpi = 520)

## SB0 ----
doNage( Fv = rep(0,narea), X = X_ija)$SB_ai %>%
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

brute[,c(1:4)] %>%
  melt(id = 'FV_sys') %>%
  ggplot(., aes(x = FV_sys, y = value, color = variable)) +
  geom_line(lwd = 1.1) + 
  scale_color_grey(labels = paste("Area",1:3)) +
  labs(x = 'F in area', y = 'Yield', color = '') +
  scale_y_continuous(limits = c(0,1)) +
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

## plot Radj ----

png(here('figs','R_eq_iterations_v3.png'),
    height = 8.5, width = 11, unit = 'in', res = 600)

plotseq = seq(1,length(Ftest),10)
par(mfrow = c(5,ceiling(length(plotseq)/5)), 
    mar = c(5,5,1.5,1.5))
for(j in plotseq){
  plot(radj[,j,1], col = 'black', 
       type = 'l', ylim = c(0,800), 
       xlab = 'Iteration No.',ylab =  'R_eq')
  text(x = maxiter*0.5, y = 600,
       cex = 1.5, label = paste0('F = ',Ftest[j]))
  ## niter x fv x areas
  for(i in 2:narea){
    points(radj[,j,i], col = c('blue','red')[i-1], type = 'l')
  }
}
plot.new()
legend('center',col = c('black','blue','red'), 
       legend = paste('Area',1:3), lty = 1, cex = 1.5)

dev.off()

## R_eq_method

R_eq_i[,4] <- rowSums(R_eq_i)
 data.frame(R_eq_i) %>%
   mutate(Fv = Ftest)


## Rick's plots ----
p1 <- ggplot(current, aes(x = Fv, y = Yield)) + 
  geom_line(lwd = 1.1, aes(color = 'current')) + 
  geom_line(data = proposed, lwd = 1.1,linetype = 'dashed', aes(color = 'proposed')) +
  scale_color_manual(values = c('seagreen','goldenrod')) +
  theme_sleek() +theme(legend.position = 'none') #+ ggtitle("high oscillation problem -- conclude on 99th iteration")


p3 <- ggplot(current, aes(x = B, y = Yield)) + 
  geom_line(lwd = 1.1, aes(color = 'current')) + 
  geom_line(data = proposed, lwd = 1.1,linetype = 'dashed', aes(color = 'proposed')) +
  scale_color_manual(values = c('seagreen','goldenrod')) +
  labs(color = "Approach") + 
  theme_sleek() +theme(legend.position = c(0.8,0.8)) #+ ggtitle("high oscillation problem -- conclude on 99th iteration")


p1  | p3

ggsave(last_plot(),
       file = here('figs',"Yield_Comparison_Movement_R.png"),
       width = 8, height = 6, unit = 'in', dpi = 420)

## Ricks plots by area ----
p4 <- ggplot( ) +
  geom_line(data = data.frame(proposed_i[,,1]), aes(x = Fv, y = Yield, col = 'Area 1') ) +
  geom_line(data = data.frame(proposed_i[,,2]), aes(x = Fv, y = Yield, col = 'Area 2') ) +
  geom_line(data = data.frame(proposed_i[,,3]), aes(x = Fv, y = Yield, col = 'Area 3') ) +
  labs(x = 'F', color = 'Area', title = 'Yield vs F by Area') + 
  theme_sleek() +theme(legend.position = 'none') 
# geom_vline(xintercept = Ftest[which(proposed_i[,2,1] > proposed_i[,3,1])[1]]) #+ ## where Y > B in area 1
# geom_vline(xintercept = Ftest[which.max(proposed_i[,2,3])]) ## where Y > B in area 1

# b <- which(proposed_i[,2,2] > proposed_i[,3,2])[1]
# b <- which.max(proposed_i[,2,3])

p5 <- ggplot( ) +
  geom_line(data = data.frame(proposed_i[,,1]), aes(x = B, y = Yield, col = 'Area 1') ) +
  geom_line(data = data.frame(proposed_i[,,2]), aes(x = B, y = Yield, col = 'Area 2') ) +
  geom_line(data = data.frame(proposed_i[,,3]), aes(x = B, y = Yield, col = 'Area 3') ) +
  labs(x = 'B', color = 'Area', title = 'Yield  vs B by Area') +
  theme_sleek() #+

# geom_vline(xintercept = Ftest[which.max(proposed_i[,2,3])]) ## where Y > B in area 1

p6 <- ggplot( ) +
  geom_line(data = data.frame(proposed_i[,,1]), aes(x = Fv, y = B, col = 'Area 1') ) +
  geom_line(data = data.frame(proposed_i[,,2]), aes(x = Fv, y = B, col = 'Area 2') ) +
  geom_line(data = data.frame(proposed_i[,,3]), aes(x = Fv, y = B, col = 'Area 3') ) +
  labs(x = 'F', color = 'Area', title = 'B vs F by Area') +
  theme_sleek() +theme(legend.position = c(0.8,0.8))

(p4  |p5  |p6)

ggsave(last_plot(),
       file = here('figs',"Yield_by_area_Movement_v3.png"),
       width = 10, height = 8, unit = 'in', dpi = 420)

## SRR ----
bind_rows(rick %>%
            select(Fv, SBeqtotal2, R_ESUMB) %>%
            mutate(SB = SBeqtotal2, value =R_ESUMB,App = "R=E(sum(B_a)), current") %>%
            select(Fv, SB, value, App),
          rick %>%select(Fv, SBeqtotal, R_SUMEBA) %>%
            mutate(SB = SBeqtotal, value =R_SUMEBA,App =  "R=sum(E(B_a)), proposed") %>%  
            select(Fv, SB, value, App)) %>% 
  # rick %>%
  #   select(-Fv) %>%
  #   melt(id = c('SBeqtotal', 'SBeqtotal2')) %>% 
  ggplot(., aes(x = SB, y = value, color = App)) +
  geom_point(alpha = 0.2) +
  theme_sleek() + theme(legend.position = c(0.75,0.1)) +
  scale_color_manual(values = c('seagreen','goldenrod'))+
  labs(x = "Total_SB", y = 'Recruitment', color = 'Approach')

ggsave(last_plot(),
       file = here('figs',"SRR_by_approach_NoMovement100iter.png"),
       width = 6, height = 4, unit = 'in', dpi = 420)

## SBPR vs R by FV----
a1 <- data.frame("Req" = R_eq_i[,1],"SBPR" = SB_Ri[,1],Ftest) %>% 
  ggplot(., aes(x = SBPR, y = Req, color = Ftest)) +
  geom_point() + labs(color = 'F', title = 'Area 1') +
  theme_sleek()+theme(legend.position = 'none')+
  geom_hline(yintercept = R0[1], lwd = 1.1, color = 'red') +
  scale_y_continuous(limits = c(0,500))

a2 <- data.frame("Req" = R_eq_i[,2],"SBPR" = SB_Ri[,2],Ftest) %>% 
  ggplot(., aes(x = SBPR, y = Req, color = Ftest)) +
  geom_point() + labs(color = 'F', title = 'Area 2') +
  theme_sleek() +theme(legend.position = 'none')+
  geom_hline(yintercept = R0[2], lwd = 1.1, color = 'red') +
  scale_y_continuous(limits = c(0,500))
a3 <- data.frame("Req" = R_eq_i[,3],"SBPR" = SB_Ri[,3],Ftest) %>% 
  ggplot(., aes(x = SBPR, y = Req, color = Ftest)) +
  geom_point() + 
  labs(color = 'F', title = 'Area 3') +
  theme_sleek() +
  geom_hline(yintercept = R0[3], lwd = 1.1, color = 'red') +
  scale_y_continuous(limits = c(0,500))
a1 |a2 |a3

ggsave(last_plot(),
       file = here('figs','ReqvsSBPR_F_area.png'),
       width = 6, height = 4, unit = 'in', dpi = 520)


rick %>%
  select(-SBeqtotal) %>%
  melt(id = 'Fv') %>%
  ggplot(., aes(x = Fv, y = value, color = variable)) +
  geom_point() +
  theme_sleek() +
  scale_color_manual(values = c('goldenrod','seagreen'),
                     labels =c("R=sum(E(B_a))", "R=E(sum(Ba))"))+
  labs(x = "Fv", y = 'Recruitment', color = 'Approach')


proposed_i[,,3] %>% as.data.frame() %>% filter(B < Yield)
## AREA SPECIFIC YIELD CURVES
plot(proposed_i[,,3], xlim = c(0,1), col = 'red')
points(proposed_i[,,2]) ## ANYTHING GREATER THAN ROW 75 HAS yield > B
points(proposed_i[,,1])

plot(proposed_i[,c(1,3),3], ylim = c(0,1500), col = 'red')
points(proposed_i[,c(1,3),2])
points(proposed_i[,c(1,3),1])


## Brute Config ----


## try a 3d plot
require(plot3D)

## build theme
# source("buildBruteConfigs.R")
lapply(list.files(here("rdata"),full.names = TRUE),source)

## all three colored by sysyield
png(here('figs','FAll_sysyield.png'), height = 4, width = 9, unit = 'in', res = 420)

par(mfrow = c(1,3), mar = rep(0.5,4))
for(e in list(brute_config_STD, brute_config_TIME, brute_config_STB)){
  # brute_temp <- load(here("rdata",e))
with(
  subset(e, sysyield > 0),
  scatter3D(
    x = FA1, #matrix(FA3),
    y = FA2, # matrix(FA1),
    z = FA3, #matrix(sysyield),
    pch = 19,
    cex = 2,
    theta = 20,
    phi = 20,
    colvar = sysyield,
    col = rev(heat.colors(100)),
    xlab = 'F Area 1',
    ylab = 'F Area 2',
    zlab = 'F Area 3')
  )
}
dev.off()

png(here('figs','FA1_FA2_sysyield.png'), height = 4, width = 4, unit = 'in', res = 420)
## just 2 with z as sysyield
with(
  subset(brute_config, sysyield > 0),
  scatter3D(
    x = FA1, #matrix(FA3),
    y = FA2, # matrix(FA1),
    z = sysyield, #matrix(sysyield),
    pch = 19,
    cex = 2,
    theta = 20,
    phi = 20,
    colvar = sysyield,
    col = rev(heat.colors(100)),
    xlab = 'F Area 1',
    ylab = 'F Area 2',
    zlab = 'System Yield'
  )
)
dev.off()

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

