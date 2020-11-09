plot_yield_curves <- function(sys_matrix,
                              rRef_proposed_i = NULL,
                              byarea = FALSE, 
                              splitid = 1){
  
  barlist <- list()
  for(RR in 1:length(R0_list)){
    barlist[[RR]] <- melt(data.frame(R0_list[[RR]])) %>%
      mutate(Area = 1:narea) %>%
      ggplot(., aes(x = Area, y = value, fill = factor(Area))) +
      geom_histogram(stat = 'identity',
                     boundary = 0)+
      scale_fill_grey()  +
      annotate('text', x = 1:narea, y = 100,
               label = paste('Area',1:narea),
               color = c("white","grey22","grey66")[1:narea], 
               size = 2) +
      scale_y_continuous(limits = c(0,1000)) +
      # scale_y_continuous(limits = c(0,max(R0_list[[RR]]*1.1))) +
      theme_void()+
      theme(legend.position = 'none')
  }
  
  if(byarea == FALSE){
    ## plot yield curves
    plist3a <- plist3a2 <- list(); idx = 1
    



    FMSYlabs = round(c(sysopt[,2,splitid],sysoptAB[,2,splitid],sysopt_curr[,2,splitid]),2)
    MSYlabs = round(c(sysopt[,3,splitid],sysoptAB[,3,splitid],sysopt_curr[,3,splitid]),2)
    
    
    for(RR in 1:length(R0_list)){
      tmp <- data.frame(sys_matrix[,,RR])
      names(tmp) <- c('Fv','current','proposed','proposedAB')
      plist3a[[idx]] <- tmp[,1:3] %>% melt(id = 'Fv') %>%
        ggplot(., aes(x = Fv, y = value, 
                      color = variable, linetype = variable)) +
        geom_line(lwd = 1.1) +
        scale_color_manual(values = c('seagreen','goldenrod','dodgerblue2'), 
                           labels = c('current','SS Syntax','a/b Syntax')) +
        scale_linetype_manual(values = c('solid','dashed','dotted'), 
                              labels = c('current','SS Syntax','a/b Syntax') ) +
        labs(x =  as.expression(bquote(F[Total])), 
             y = ifelse(RR == 1, 'Yield',""),
             color = "", linetype = "") +
        theme_sleek() + 
        theme(legend.position = if(RR<3) 'none' else c(0.75,0.5)) +
        
        ## FMSY LABS
        annotate('text', x = max(tmp$Fv* 0.8), y = 95, size = 2,
                 color ='seagreen',
                 label = as.expression(bquote(F[MSY]~"="~.(FMSYlabs[i+6])))) +
        annotate('text', x = max(tmp$Fv* 0.8), y = 85,size = 2,
               color ='goldenrod',
               label = as.expression(bquote(F[MSY]~"="~.(FMSYlabs[i])))) +
        # annotate('text', x = max(tmp$Fv* 0.8), y = 75,size = 2,
        #          color ='dodgerblue2',
        #          label = as.expression(bquote(F[MSY]~"="~.(FMSYlabs[i+3])))) + 
        ## MSY LABS
        annotate('text', x = max(tmp$Fv* 0.8), y = 90,size = 2,
                 color ='seagreen',
                 label = as.expression(bquote(MSY~"="~.(MSYlabs[i+6])))) +
        annotate('text', x = max(tmp$Fv* 0.8), y = 80,size = 2,
                 color ='goldenrod',
                 label = as.expression(bquote(MSY~"="~.(MSYlabs[i])))) #+
        # annotate('text', x = max(tmp$Fv* 0.8), y = 70,size = 2,
        #          color ='dodgerblue2',
        #          label = as.expression(bquote(MSY~"="~.(MSYlabs[i+3])))) 

      
      idx = idx+1
    }
    

    
    
    for(RR in 1:length(plist3a)){
      plist3a2[[RR]] <- ggdraw() +
        draw_plot(plist3a[[RR]] + 
                    annotate('text', x = 0, y = 90, 
                             label = toupper(letters[(splitid-1)*3+RR]), cex = 6)) +
        draw_plot(barlist[[ifelse(RR >length(R0_list), RR-length(R0_list), RR)]]+
                    annotate('text', x = (1+narea)/2, 
                             y = 750, 
                             cex = 2,
                             label = 'Initial Recruitment'), 
                  x = 0.7, y = 0.2,
                  width = .25, height = .25) 
    }
    # print(Rmisc::multiplot(plotlist = plist3a2,  cols = length(R0_list)))
    # return(Rmisc::multiplot(plotlist = plist3a2,  cols = length(R0_list)))
    return(plist3a2)
  }
  
  if(byarea == TRUE){
    ## Ricks plots by area ----
    p1list = list()
    blist= list()
    # A1 = round(c(areaopt[,2],sysopt_curr$FsysMSY),3)
    # A2 = round(c(areaopt[,3],sysopt_curr$FsysMSY),3)
    # A3 = round(c(areaopt[,4],sysopt_curr$FsysMSY),3)
    
    
    for(i in 1:length(R0_list)){
      propI <- data.frame( sys_matrix[,,i])
      names(propI) <- c('Fv',paste('SS Syntax Area',1:narea),paste('a/b Syntax Area',1:narea))
      p1list[[i]] <- propI[,1:3] %>% 
        melt(id = c("Fv")) %>%
        ggplot( ., aes(x = Fv, y = value, color = variable, linetype = variable)) +
        geom_line(lwd = 1.1) +
        labs(x =  as.expression(bquote(F[Total])),
             y = ifelse(i == 1, 'Yield',""),
             color = "", linetype = "") +
        scale_color_manual(values = c('grey22','grey66','grey22','grey66')) +
        scale_linetype_manual(values = c('solid','solid','dotted','dotted')) +
        scale_y_continuous(limits = c(0,50))+
        # scale_y_continuous(limits = c(0,max(propI[,2:(narea+1)])*1.1)) +
        theme_sleek() +
        # theme(legend.position = 'none')
      theme(legend.position = if(i < length(R0_list)) 'none' else c(0.75,0.8)) #+ ggtitle("high oscillation problem -- conclude on 99th iteration")
        # annotate('text', x = 0.75, y = 60,
        #          color ='grey66',
        #          label = as.expression(bquote(F[MSY]~"="~.(A1[i])))) +
        # annotate('text', x = 0.75, y = 55,
        #          color ='grey44',
        #          label = as.expression(bquote(F[MSY]~"="~.(A2[i])))) +
        # annotate('text', x = 0.75, y = 50,
        #          color ='grey22',
        #          label = as.expression(bquote(F[MSY]~"="~.(A3[i])))) +
        # annotate('text', x = 0.75, y = 45,
        #          color ='blue',
        #          label = as.expression(bquote(Total~Yield~"="~.(sum(areaopt[i,5:7]))))) +
        # 
      # blist[[i]] <-  ggplot( ) +
      #   geom_line(data = propi1, lwd = 1.1,
      #             aes(x = Fv, y = B, col = 'Area 1') ) +
      #   geom_line(data = propi2, lwd = 1.1,
      #             aes(x = Fv, y = B, col = 'Area 2') ) +
      #   geom_line(data = propi3, lwd = 1.1,
      #             aes(x = Fv, y = B, col = 'Area 3') ) +
      #   labs(x = 'F', y = ifelse(i == 1, 'B',""), color = "") +
      #   scale_color_grey() + 
      #   scale_y_continuous(limits = c(0,625)) +
      #   theme_sleek() +
      #   theme(legend.position = 'none') #+ ggtitle("high oscillation problem -- conclude on 99th iteration")
      
    }
    plist2 <- list()
    for(RR in 1:length(R0_list)){
      plist2[[RR]] <- ggdraw() +
        draw_plot(p1list[[RR]] +   
                    annotate('text', x = 0, y = 35, 
                             label = toupper(letters[(splitid-1)*3+RR]), cex = 6)) +
        draw_plot(barlist[[RR]]+
                    annotate('text', x = (narea+1)/2, 
                             y = 750,
                             cex = 2,
                             label = 'Initial Recruitment'), 
                  x = 0.65, y = 0.2,
                  width = .25, height = .25) 
      
      
    }

    return(plist2)
    
  }

}



plot_radj <- function(radj_kvar, Fidx = 10:15){
  plotseq = Fidx
  par(mfrow = c(dim(radj_kvar)[4],
                length(plotseq)),
      mar = c(5,5,1.5,1.5))
  for(i in 1:dim(radj_kvar)[4]){
    
    radj <- radj_kvar[,,,i]
    for(j in plotseq){
      if(i == dim(radj_kvar)[4] & j == max(plotseq)) next()
      tmpx=NULL
      for(a in 1:narea) tmpx[a] = min(which(is.na(radj[,j,a+1])))
      tmpx[tmpx == Inf] <- 101
      plot(radj[,j,2], col = 'black', 
           type = 'l', 
           ylim = c(0,1500), 
           xlab = 'Iteration No.',
           xlim = c(0,  max(tmpx)),
           ylab =  'R_eq')
      text(x = max(tmpx)*0.5, y = 1250,
           cex = 1.5, label = paste0('F = ',Ftest[j]))
      ## niter x fv x areas
      for(k in 3:(narea+1)){
        lines(radj[,j,k], col = c('blue','red')[k-2])
      }
    } ## end j (Fs)
  } 
  
  plot.new()
  legend('center',
         col = c('black','blue','red'), 
         legend = paste('Area',1:narea), lty = 1, cex = 1)
  
  # dev.off()
}


## Xija ----
# plist = list()
# for(g in c(1,6)){ ## loop ages
#   plist[[ifelse(g == 1,1,2)]] <-  data.frame(X_ija_MIX2b[,,g]) %>% 
#     mutate('FRM' = 1:narea) %>%
#     melt(id = 'FRM') %>%
#     ggplot(., aes(x = FRM, y = variable, fill = value)) +
#     geom_tile() + 
#     ggsidekick::theme_sleek() +
#     theme(legend.position = 'none',
#           axis.ticks = element_blank(),
#           axis.text = element_text(size = 10),
#           axis.title = element_text(size = 10)) +
#     scale_x_continuous(expand = c(0,0), breaks = 1:narea,
#                        labels = paste("Area",1:narea)) +
#     scale_y_discrete(expand = c(0,0), labels = paste("Area",1:narea))+
#     geom_text(aes(label = value), colour = 'white', size = 10) +
#     labs(x = "Area From", y = "Area To", 
#          title = ifelse(g < 6, paste('Ages 1-5'), "Ages 6+")) 
#   
# }
# 
# ggsave(Rmisc::multiplot(plotlist = plist, 
#                         layout = matrix(c(1,2),
#                                         nrow = 1, byrow = TRUE) ),
#        file = here('figs','X_ija_MIX2b.png'),
#        width = 5, height = 3, unit = 'in', dpi = 520)