## inputs by area ----
png( here('figs','inputs_by_area.png'),
     width = 8, height = 4, unit = 'in', res = 520)
par(mfrow = c(1,3), 
    mar = c(4,5,1,1))
for(a in 1:1){
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
doNage( Fv = rep(0,narea), X = X_ija_MIX,
        refR = c(700,300), rdist = c(1,1))$N_ai %>%
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
       file = here('figs','Nage_RR1.png'),
       width = 6, height = 4, unit = 'in', dpi = 520)


doNage( Fv = rep(0.25,narea), X = X_ija_MIX2,
        refR = c(998,0.2), rdist = c(1,1))$Yield_i

doNage( Fv = rep(0.25,narea), X = X_ija_MIX2,
        refR = c(998,2), rdist = c(1,1))$Yield_i

## Nums at age panel ----
nagelist <- nagelist2 <- list()
for(RR in 1:4){
  nagelist[[RR]] <- doNage( Fv = rep(0,narea), X = X_ija,
                            refR = R0_list[[RR]])$N_ai %>%
    data.frame() %>%
    mutate(Age = 1:nages) %>%
    reshape2::melt(id = 'Age') %>%
    mutate(Area = substr(variable,2,2)) %>%
    ggplot(., aes(x = Age-1, y = value, col = Area)) +
    geom_line(lwd = 1.1) + 
    scale_color_grey(labels = paste("Area",1:3)) +
    labs(x = 'Age', y = 'Total Numbers', color = '') +
    theme_sleek() +  theme(legend.position = 'none')
    # theme(legend.position = c(0.5,0.5), 
    #       axis.title = element_text(size = 16),
    #       axis.text = element_text(size = 16),
    #       legend.text = element_text(size = 20))
}
for(RR in 1:4){
nagelist2[[RR]] <- ggdraw() +
  draw_plot(nagelist[[RR]]) +
  draw_plot(barlist[[RR]]+
              annotate('text', x = 2, y = 500, cex = 4,
                       label = 'Initial Recruitment'), 
            x = 0.6, y = 0.5,
            width = .3, height = .3)
}

ggsave(Rmisc::multiplot(plotlist = nagelist2,  cols = 2), 
       file = here('figs','Nage_panel.png'),
       width = 10, height = 8, unit = 'in', dpi = 520)

# doNage(s = 1,  Fv = rep(0,narea), eq_method = 'STB')[,1:3] %>%
#   data.frame() %>%
#   mutate(Age = 1:nages) %>%
#   reshape2::melt(id = 'Age') %>%
#   mutate(Area = substr(variable,2,2)) %>%
#   ggplot(., aes(x = Age, y = value, col = Area)) +
#   geom_line(lwd = 1.1) + 
#   scale_color_grey(labels = paste("Area",1:3)) +
#   labs(x = 'Age', y = 'Relative Numbers', color = '') +
#   theme_sleek() + 
#   theme(legend.position = c(0.8,0.9), 
#         axis.title = element_text(size = 16),
#         axis.text = element_text(size = 16),
#         legend.text = element_text(size = 20))
# ggsave(last_plot(), 
#        file = here('figs','Nage_STB.png'),
#        width = 6, height = 4, unit = 'in', dpi = 520)

# rbind(
# doNage(s = 1,  Fv = rep(0,narea), eq_method = 'STD')[,1:3] %>%
#   data.frame() %>%
#   mutate(Age = 1:nages, EQ = 'STD')%>%
#   reshape2::melt( id = c('Age','EQ')),
# doNage(s = 1,  Fv = rep(0,narea), eq_method = 'TIME')[,1:3] %>%
#   data.frame() %>%
#   mutate(Age = 1:nages, EQ = 'TIME')%>%
#   reshape2::melt( id = c('Age','EQ')),
# doNage(s = 1,  Fv = rep(0,narea), eq_method = 'STB')[,1:3] %>%
#   data.frame() %>%
#   mutate(Age = 1:nages, EQ = 'STB')%>%
#   reshape2::melt( id = c('Age','EQ'))) %>%
#   mutate(Area = substr(variable,2,2)) %>% 
#   select(-variable) %>%
#   mutate(group = paste0("Area ",Area," ",EQ)) %>%
#   ggplot(., aes(x = Age, y = value, col = Area, linetype = group)) +
#   geom_line(lwd = 1.1) + 
#   scale_color_grey() +
#   scale_linetype_manual(values = rep(c('solid','dashed','dotted'),3))+
#   scale_y_continuous(limits = c(0,1.5)) +
#   labs(x = 'Age', y = 'Relative Numbers', color = '', linetype = "") +
#   theme_sleek() + 
#   theme(legend.position = 'none', 
#         axis.title = element_text(size = 10),
#         axis.text = element_text(size = 10),
#         legend.text = element_text(size = 20)) +
#   facet_wrap(~EQ)

# ggsave(last_plot(), 
#        file = here('figs','Nage_All.png'),
#        width = 10, height = 8, unit = 'in', dpi = 520)
## B0 ----
doNage( Fv = rep(0,narea), X = X_ija, 
        refR = R0_list[[1]])$B_ai %>%
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
 doNage( Fv = rep(0.55,narea), X = X_ija, refR = c(108.2182,85.02972,64.41718))$SB_ai %>%
  data.frame() %>%
  mutate(Age = 1:nages) %>%
  reshape2::melt(id = 'Age') %>%
  mutate(Area = substr(variable,2,2)) %>%
  ggplot(., aes(x = Age, y = value, col = Area)) +
  geom_line(lwd = 1.1) + 
  scale_color_grey(labels = paste("Area",1:3)) +
  labs(x = 'Age', y = 'Spawning Biomass', color = '',
       title = 'Unfished Spawning Biomass',
       subtitle = paste0("Area ",1:3," R0 = " ,R0_list[[1]], collapse = ", ")) +
  theme_sleek() + 
  # scale_y_continuous(limits = c(0,50)) +
  theme(legend.position = c(0.8,0.9), 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 16))
# R0_list[[1]]
 
ggsave(last_plot(), 
       file = here('figs','SSB_A3comp.png'),
       width = 8, height = 6, unit = 'in', dpi = 520)

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

# png(here('figs','R_eq_iterations_v3.png'),
#     height = 8.5, width = 11, unit = 'in', res = 600)
# 
# plotseq = seq(1,length(Ftest),5)
# par(mfrow = c(5,ceiling(length(plotseq)/5)),
#     mar = c(5,5,1.5,1.5))
# for(j in plotseq){
#   plot(radj[,j,1], col = 'black',
#        type = 'l', ylim = c(0,800),
#        xlab = 'Iteration No.',ylab =  'R_eq')
#   text(x = maxiter*0.5, y = 600,
#        cex = 1.5, label = paste0('F = ',Ftest[j]))
#   ## niter x fv x areas
#   for(i in 2:narea){
#     points(radj[,j,i], col = c('blue','red')[i-1], type = 'l')
#   }
# }
# plot.new()
# legend('center',col = c('black','blue','red'),
#        legend = paste('Area',1:3), lty = 1, cex = 1.5)
# 
# dev.off()
png(here('figs','R_eq_iterations_v4_xr.png'),
    height = 8.5, width = 11, unit = 'in', res = 600)

plotseq = c(10:15)
par(mfrow = c(dim(rRef_proposed_radj)[4],length(plotseq)),
    mar = c(5,5,1.5,1.5))

# plist = list()
for(i in 1:dim(rRef_proposed_radj)[4]){

  radj <- rRef_proposed_radj[,,,i]
    for(j in plotseq){
      if(i == dim(rRef_proposed_radj)[4] & j == max(plotseq)) next()
      plot(radj[,j,1], col = 'black', 
           type = 'l', ylim = c(0,800), 
           xlab = 'Iteration No.',
           ylab =  'R_eq')
      text(x = maxiter*0.5, y = 600,
           cex = 1.5, label = paste0('F = ',Ftest[j]))
      ## niter x fv x areas
      for(k in 2:narea){
        lines(radj[,j,k], col = c('blue','red')[k-1])
      }
    } ## end j (Fs)
  } 

plot.new()
legend('center',
       col = c('black','blue','red'), 
       legend = paste('Area',1:3), lty = 1, cex = 1)

dev.off()


## exploring convergence draa ----
plot(rRef_proposed_radj[,12,3,4],type = 'p', col = 'red', 
     ylim = c(0,500))
lines(rRef_proposed_radj[,12,2,4],type = 'p', col = 'blue')
lines(rRef_proposed_radj[,12,1,4],type = 'p', col = 'black')


plot(rRef_proposed_SBi[,11,3,1],type = 'l', col = 'red',ylim= c(0,80))
lines(rRef_proposed_SBi[,11,2,1],type = 'l', col = 'blue')
lines(rRef_proposed_SBi[,11,1,1],type = 'l', col = 'black')


plot(rRef_proposed_sbpr[,16,3,1],type = 'l', col = 'red',
     ylim = c(0,max(rRef_proposed_sbpr[,16,3,1])))
lines(rRef_proposed_sbpr[,16,2,1],type = 'l', col = 'blue')
lines(rRef_proposed_sbpr[,16,1,1],type = 'l', col = 'black')

plot(rRef_proposed_radj[2:101,12,3,1] ~rRef_proposed_radj[1:100,12,3,1])

plot(rRef_proposed_SBi[2:101,12,3,1] ~rRef_proposed_SBi[1:100,12,3,1])





plot(rRef_proposed_SBi[1:100,12,3,1],
       rRef_proposed_radj[2:101,12,3,1])

## small SB and rRef lead to huge SBPR (divide small #s)
ggplot(data = NULL, aes(x = rRef_proposed_radj[1:100,12,3,1],
                        y = rRef_proposed_SBi[1:100,12,3,1],
                        color = rRef_proposed_sbpr[1:100,12,3,1])) +
  geom_point() +
  geom_text(label = list(round(rRef_proposed_sbpr[1:101,12,3,1] ,2),
                         round(rRef_proposed_radj[2:101,12,3,1] ,4),
                         round(rRef_proposed_SBi[1:101,12,3,1] ,4))[[2]],
            check_overlap = TRUE)

# as input Rref gets lower, next rRef gets higher,
## and vice versa
## the highest SBPRs occur at the lowest rRefs bc div by large nums
ggplot(data = NULL, aes(x = rRef_proposed_radj[1:100,12,3,1],
                        y = rRef_proposed_radj[2:101,12,3,1],
                        color = list(factor(2:101),
                                     rRef_proposed_sbpr[2:101,12,3,1],
                                     c('black',
                                       rRef_proposed_SBi[2:101,12,3,1]))[[1]])) +
  geom_point() + #scale_color_grey() +
  geom_text(label = list(round(rRef_proposed_sbpr[2:101,12,3,1] ,2),
                         round(rRef_proposed_radj[1:101,12,3,1] ,4),
                         round(rRef_proposed_SBi[1:101,12,3,1] ,4),
                        paste('iter',1:101))[[1]],
            # check_overlap = TRUE,
            color = 'black') +
  labs(x = "r eq input iter", y = "next r_eq", color = "sbpr at next r_eq")

rRef_proposed_radj[1:101,12,3,1][
  which(round(rRef_proposed_sbpr[1:101,12,3,1] ,2) == 0.27)]

## there are only 2 values of SBPR
ggplot(data = NULL, aes(x = rRef_proposed_SBi[1:101,12,3,1],
                        y = rRef_proposed_sbpr[1:101,12,3,1],
                        color = rRef_proposed_radj[1:101,12,3,1])) +
  geom_point()


## does A3 always have the highest SBPR
for(v in 1:length(Ftest)){
  for(k in 1:maxiter){
    if(which.max(rRef_proposed_sbpr[k,v,,1]) != 3) stop("error RR1")
    if(which.max(rRef_proposed_sbpr[k,v,,2]) != 3) stop("error RR2")
    if(which.max(rRef_proposed_sbpr[k,v,,3]) != 3) stop("error RR3")
    if(which.max(rRef_proposed_sbpr[k,v,,4]) != 3) stop("error RR4")
    
  }}

## sanity check calcs happened correctly
(rRef_proposed_SBi[1:101,12,3,1]/rRef_proposed_radj[1:101,12,3,1]) ==
  rRef_proposed_sbpr[1:101,12,3,1]
## it's a rounding issue
plot((rRef_proposed_SBi[1:101,12,3,1]/rRef_proposed_radj[1:101,12,3,1]) ~
       rRef_proposed_sbpr[1:101,12,3,1], ylim = c(0,1), xlim = c(0,1))
abline(0,1, col = 'red')

## 24 is where oscillations start
which(rRef_proposed_SBi[1:100,12,3,1] <rRef_proposed_SBi[1:100,12,1,1])
## R_eq_method

R_eq_i[,4] <- rowSums(R_eq_i)
 data.frame(R_eq_i) %>%
   mutate(Fv = Ftest)


## Rick's plots ----
# p1 <- ggplot(current, aes(x = Fv, y = Yield)) + 
#   geom_line(lwd = 1.1, aes(color = 'current')) + 
#   geom_line(data = proposed, lwd = 1.1,linetype = 'dashed', aes(color = 'proposed')) +
#   scale_color_manual(values = c('seagreen','goldenrod')) +
#   theme_sleek() +theme(legend.position = 'none') #+ ggtitle("high oscillation problem -- conclude on 99th iteration")
# 
# 
# p3 <- ggplot(current, aes(x = B, y = Yield)) + 
#   geom_line(lwd = 1.1, aes(color = 'current')) + 
#   geom_line(data = proposed, lwd = 1.1,linetype = 'dashed', aes(color = 'proposed')) +
#   scale_color_manual(values = c('seagreen','goldenrod')) +
#   labs(color = "Approach") + 
#   theme_sleek() +theme(legend.position = c(0.8,0.8)) #+ ggtitle("high oscillation problem -- conclude on 99th iteration")
# 
# 
# p1  | p3
# 
# ggsave(last_plot(),
#        file = here('figs',paste0("Yield_Comparison_Movement_",paste(R0_list[[1]], collapse = "-")),".png"),
#        width = 8, height = 6, unit = 'in', dpi = 420)

 p1list <- plist2 <-  barlist <- list()
 for(i in 1:4){
   labs = round(c(sysopt$FsysMSY,sysopt_curr$FsysMSY),3)
   current <- data.frame(rRef_current[,,i])
   names(current) <- c('Fv','Yield','B')
   proposed <-  data.frame(rRef_proposed[,,i])
   names(proposed) <- c('Fv','Yield','B')
   p1list[[i]] <- ggplot(current, aes(x = Fv, y = Yield)) +
     geom_line(lwd = 1.1, aes(color = 'current')) +
     geom_line(data = proposed, lwd = 1.1,linetype = 'dashed', aes(color = 'proposed')) +
     scale_color_manual(values = c('seagreen','goldenrod')) +
     labs(x = 'F', y = ifelse(i == 1, 'Yield',""), color = "") +
     theme_sleek() +
     annotate('text', x = 0.75, y = 95,
              color ='seagreen',
              label = as.expression(bquote(F[MSY]~"="~.(labs[i])))) +
     
     annotate('text', x = 0.75, y = 85,
              color ='goldenrod',
              label = as.expression(bquote(F[MSY]~"="~.(labs[i+4])))) +
     
     annotate('text', x = 0.75, y = 75,
              color ='seagreen',
              label = as.expression(bquote(MSY~"="~
                                             .(round(sysopt_curr[i,'YsysMSY'],2))))) +
     annotate('text', x = 0.75, y = 65,
              color ='goldenrod',
              label = as.expression(bquote(MSY~"="~
                                             .(round(sysopt[i,'YsysMSY'],2)))))+
     
     theme(legend.position = if(i < 4) 'none' else c(0.75,0.5)) #+ ggtitle("high oscillation problem -- conclude on 99th iteration")
 
   
  barlist[[i]] <- melt(data.frame(R0_list[[i]])) %>%
     mutate(Area = 1:3) %>%
   ggplot(., aes(x = Area, y = value, fill = factor(Area))) +
     geom_histogram(stat = 'identity',
                    boundary = 0)+
     scale_fill_grey()  +
    annotate('text', x = 1:3, y = 200,
             label = paste('Area',1:3),
             color = c("grey88","grey33","grey22"), size = 3)+
    scale_y_continuous(limits = c(0,550)) +
     theme_void()+
    theme(legend.position = 'none')
   
   }

 for(RR in 1:4){
   plist2[[RR]] <- ggdraw() +
     draw_plot(p1list[[RR]] +   
                 annotate('text', x = 0, y = 95, 
                          label = toupper(letters[RR]), cex = 6)) +
     draw_plot(barlist[[RR]]+
                 annotate('text', x = 2, y = 545, cex = 3,
                          label = 'Initial Recruitment'), 
               x = 0.7, y = 0.15,
               width = .25, height = .25) 
   
   
 }
 
 ggsave(  Rmisc::multiplot(plotlist = plist2,
                           cols = 2),
        file = here('figs',"Yield_comparison_Rref_Buffer=0.005_fmsy_21iter.png"),
        width = 10, height = 8, unit = 'in', dpi = 420)
 


 
 
## Ricks plots by area ----
 p1list = list()
 blist= list()
 A1 = round(c(areaopt[,2],sysopt_curr$FsysMSY),3)
 A2 = round(c(areaopt[,3],sysopt_curr$FsysMSY),3)
 A3 = round(c(areaopt[,4],sysopt_curr$FsysMSY),3)
 
 
 for(i in 1:4){
   
   proposed_i <-  rRef_proposed_i[,,,i]
   propi1 <- data.frame(proposed_i[,1:3,1])
   names(propi1) <- c('Fv','Yield','B')
   propi2 <- data.frame(proposed_i[,1:3,2])
   names(propi2) <- c('Fv','Yield','B')
   propi3 <- data.frame(proposed_i[,1:3,3])
   names(propi3) <- c('Fv','Yield','B')
   
   p1list[[i]] <- ggplot( ) +
     geom_line(data = propi1, lwd = 1.1,
               aes(x = Fv, y = Yield, col = 'Area 1') ) +
     geom_line(data = propi2, lwd = 1.1,
               aes(x = Fv, y = Yield, col = 'Area 2') ) +
     geom_line(data = propi3, lwd = 1.1,
               aes(x = Fv, y = Yield, col = 'Area 3') ) +
     labs(x = 'F', y = ifelse(i == 1, 'Yield',""), color = "") +
     scale_color_grey() + 
     scale_y_continuous(limits = c(0,65)) +
     
     annotate('text', x = 0.75, y = 60,
              color ='grey66',
              label = as.expression(bquote(F[MSY]~"="~.(A1[i])))) +
     annotate('text', x = 0.75, y = 55,
              color ='grey44',
              label = as.expression(bquote(F[MSY]~"="~.(A2[i])))) +
     annotate('text', x = 0.75, y = 50,
              color ='grey22',
              label = as.expression(bquote(F[MSY]~"="~.(A3[i])))) +
     annotate('text', x = 0.75, y = 45,
              color ='blue',
              label = as.expression(bquote(Total~Yield~"="~.(sum(areaopt[i,5:7]))))) +
     
     theme_sleek() +
     theme(legend.position = 'none')
     # theme(legend.position = if(i < 4) 'none' else c(0.8,0.2)) #+ ggtitle("high oscillation problem -- conclude on 99th iteration")
   
   blist[[i]] <-  ggplot( ) +
     geom_line(data = propi1, lwd = 1.1,
               aes(x = Fv, y = B, col = 'Area 1') ) +
     geom_line(data = propi2, lwd = 1.1,
               aes(x = Fv, y = B, col = 'Area 2') ) +
     geom_line(data = propi3, lwd = 1.1,
               aes(x = Fv, y = B, col = 'Area 3') ) +
     labs(x = 'F', y = ifelse(i == 1, 'B',""), color = "") +
     scale_color_grey() + 
     scale_y_continuous(limits = c(0,625)) +
     theme_sleek() +
     theme(legend.position = 'none') #+ ggtitle("high oscillation problem -- conclude on 99th iteration")
   
 }

 for(RR in 1:4){
   plist2[[RR]] <- ggdraw() +
     draw_plot(p1list[[RR]] +   
                 annotate('text', x = 0, y = 60, 
                          label = toupper(letters[RR]), cex = 6)) +
     draw_plot(barlist[[RR]]+
                 annotate('text', x = 2, y = 500, cex = 4,
                          label = 'Initial Recruitment'), 
               x = 0.65, y = 0.15,
               width = .3, height = .3) 
   
   
 }
 
 ggsave( Rmisc::multiplot(plotlist = plist2,
                          cols = 2)
          ,
         file = here('figs',"YieldvF_Area_Rref_Buffer=0.005_Fopt.png"),
         width = 10, height = 8, unit = 'in', dpi = 420) 
 

 

 
## Yields vs F config ----
ycomp <- data.frame(expand.grid('RR' = 1:4, 'eq_method' = c('current','proposed'),
                                "F_method" = c('system','config'),
                                'Area' = 1:3)) %>% mutate('yield' = NA)
ycomp$Area[ycomp$eq_method == 'current'] <- 'Total'

for(RR in 1:4){
  
  ycomp$yield[ycomp$RR == RR & ycomp$eq_method == 'current'][1] <- sysopt_curr$YsysMSY[RR]
  ycomp$yield[ycomp$RR == RR & 
                ycomp$eq_method == 'proposed' & 
                ycomp$F_method == 'system'] <- sysopt[RR,5:7]
  
  ycomp$yield[ycomp$RR == RR & 
                ycomp$eq_method == 'proposed' & 
                ycomp$F_method == 'config'] <- areaopt[RR,5:7]
 
}

ylist <- ylist2 <- list()
for(R in 1:4){
  tmp <- ycomp %>% filter(RR == R)
  

  ylist[[R]] <- ggplot(tmp, aes(x = interaction(eq_method, F_method), y = yield, fill = Area)) +
   geom_bar(position = 'stack', stat = 'identity') +
   scale_fill_manual(values = c('grey22','grey44','grey66','seagreen4')) +
   scale_x_discrete(limits = c('current.system', 'proposed.system', 'proposed.config'),
                    labels = c('Current',expression(Proposed~F[system]),
                               expression(Proposed ~F[config]))) +
    labs(x = ifelse(R == 1,'Equilibrium and F approach',""),
         y = ifelse(R == 1, 'Yield',""), color = "") +
    theme_sleek() +
    theme(legend.position = if(R < 4) 'none' else c(0.5,1), 
          legend.direction = 'horizontal',
          axis.text.x = element_text(size = 8),
          legend.background = element_rect(fill=alpha('white', 1)))
  
   
}



ggsave(  Rmisc::multiplot(plotlist = ylist2,
                          cols = 2)  ,
         file = here('figs',"yield_area_method.png"),
         width = 12, height = 8, unit = 'in', dpi = 420) 
  
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
       file = here('figs',"Yield_by_area_Movement_v4.png"),
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
       file = here('figs',"SRR_by_approach_WithPenalty.png"),
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

