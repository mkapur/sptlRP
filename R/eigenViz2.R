library(datasauRus)
library(ggplot2)
library(gganimate)
library(dplyr)
demPal <- c("#015b58", "#2c6184", "#ba7999", "#a8bbcc") ## 4 demographic regions, these are 4:1
## original
ggplot(datasaurus_dozen, aes(x=x, y=y))+
  geom_point()+
  theme_void() +
  transition_states(dataset, 3, 1) + 
  ease_aes('cubic-in-out')

## basically need 'state' and x and y

ggplot(datasaurus_dozen, aes(x=x, y=y, color = dataset))+
  geom_point()+
  theme_void() +
  transition_states(dataset, 3, 3) + 
  ease_aes('cubic-in-out')


ggplot(rbind(pop1,pop2), aes(x=x, y=y))+
  geom_point(size = 3)+
  theme_void() +
  transition_states(dataset, 3, 3) + 
  ease_aes('cubic-in-out')

data.frame(cbind(x,y,'pop' = 'A'))



## CASE 1: no growth ----

## pre-define N in area at timestep S
pmove = c(0.1,0.3);  PI = 3.14
trumat = matrix(NA, nrow = 2, ncol = 2)
trumat[1,1] = 1-pmove[1]; trumat[2,2] = 1-pmove[2]
trumat[1,2] = pmove[1]; trumat[2,1] = pmove[2]
eigen(trumat)

nsteps = 20

Nmat <- matrix(NA, nrow = nsteps, ncol = 2)
Nmat[1,] <- c(1000,1000)

mst <- data.frame(); idx = 1
for(s in 2:nrow(Nmat)){
  for(area in 1:ncol(Nmat)){
    pLeave = NCome = 0
    for(jarea in 1:ncol(Nmat)){
      if(area != jarea){
        pLeave = pLeave + pmove[area] ##  leaving for elsewhere
        NCome = NCome +pmove[jarea]*Nmat[s-1,jarea]## mortality in other area
      } # end i != j
    }  # end subareas j
    Nmat[s,area] <- (1-pLeave)*Nmat[s-1,area] + NCome
  } ## end area loop for Nmat
} ## end s loop for Nmat
Nmat <- round(Nmat) ## need to be integers
for(s in 1:nrow(Nmat)){
  for(area in 1:ncol(Nmat)){
    for(N in 1:Nmat[s,area]){
      R <- ifelse(area == 1, 1, 2)
      offs <- ifelse(area == 1, 0, 50) ## move the center 
      a = runif(1,0,100) * 2 * PI
      r = R * sqrt(runif(1,0,100))
      mst[idx,'dataset'] <- s
      ## if this individual was a stayer from last time, label it with home color
      if(s == 1)  mst[idx,'area'] <- area
      if(s >1)  mst[idx,'area'] <- ifelse(N < (1-pmove[area])*Nmat[s-1,area],area,c(1,2)[which(c(1,2)!=area)])
      mst[idx,'x'] <- r * cos(a)+offs
      mst[idx,'y'] <- r * sin(a)
      idx = idx+1
    } ## end N in Nmat
  } ## end area for positions
} ## end timestep

save(mst, file = 'nmat_noddep.rdata')

blob <- ggplot(mst, aes(x=x, y=y, color = factor(area)))+
  geom_point(size = 5)+
  theme_void() +
  theme(legend.position = 'none')+
  scale_color_manual(values = c(demPal[1],demPal[4])) +
  transition_states(dataset, 3, 2) + 
  ease_aes('cubic-in-out')


gganimate::anim_save("blob_noddep.gif", blob)

eig <- mst %>% group_by(dataset,area) %>% summarise(n= n()) %>% 
  tidyr::pivot_wider(., names_from = area, values_from = n) %>%
  plyr::rename(c(`1` = 'a1', `2` = 'a2')) %>%
  ggplot(., aes(x=a1, y=a2, col = dataset))+
  geom_point(size = 5)+
  ggsidekick::theme_sleek(base_size = 18) + 
  labs(x = 'Pop 1', y = 'Pop 2') +
  theme(legend.position = 'none')+
  scale_x_continuous(limits = c(0,2000))+
  scale_y_continuous(limits = c(0,2000))+
  geom_abline(intercept = 0,
              slope = Nmat[nrow(Nmat),2]/Nmat[nrow(Nmat),1], 
              color = 'grey88', lwd = 2) +
  
  transition_states(dataset, 3, 6) +  shadow_mark() +

  ease_aes('cubic-in-out')
gganimate::anim_save("eig_noddep.gif", eig)

## case 2: simple coefficient of growth ----
nsteps = 30
pmove = c(0.6,0.1);  PI = 3.14
pgrow = 1.01
Nmat <- matrix(NA, nrow = nsteps, ncol = 2)
Nmat[1,] <- c(1000,1000)

mst <- data.frame(); idx = 1
for(s in 2:nrow(Nmat)){
  for(area in 1:ncol(Nmat)){
    pLeave = NCome = 0
    for(jarea in 1:ncol(Nmat)){
      if(area != jarea){
        pLeave = pLeave + pmove[area] ##  leaving for elsewhere
        NCome = NCome +pmove[jarea]*Nmat[s-1,jarea]## mortality in other area
      } # end i != j
    }  # end subareas j
    Nmat[s,area] <- pgrow*(1-pLeave)*Nmat[s-1,area] + NCome
  } ## end area loop for Nmat
} ## end s loop for Nmat
Nmat <- round(Nmat) ## need to be integers
for(s in 1:nrow(Nmat)){
  for(area in 1:ncol(Nmat)){
    for(N in 1:Nmat[s,area]){
      R <- ifelse(area == 1, 1, 2)*pgrow*s ## emphasize that it's growing via r
      offs <- ifelse(area == 1, 0, 50*pgrow*s) ## move the center 
      a = runif(1,0,100) * 2 * PI
      r = R * sqrt(runif(1,0,100))
      mst[idx,'dataset'] <- s
      ## if this individual was a stayer from last time, label it with home color
      if(s == 1)  mst[idx,'area'] <- area
      if(s >1)  mst[idx,'area'] <- ifelse(N < (1-pmove[area])*Nmat[s-1,area],area,c(1,2)[which(c(1,2)!=area)])
      mst[idx,'x'] <- r * cos(a)+offs
      mst[idx,'y'] <- r * sin(a)
      idx = idx+1
    } ## end N in Nmat
  } ## end area for positions
} ## end timestep

save(mst, file = 'nmat_lineargrowth.rdata')

blob <- ggplot(mst, aes(x=x, y=y, color = factor(area)))+
  geom_point(size = 5)+
  theme_void() +
  theme(legend.position = 'none')+
  scale_color_manual(values = c(demPal[1],demPal[4])) +
  transition_states(dataset, 3, 6) + 
  ease_aes('cubic-in-out')
gganimate::anim_save("blob_lineargrowth.gif", blob)

eig <- mst %>% group_by(dataset,area) %>% summarise(n= n()) %>%
  tidyr::pivot_wider(., names_from = area, values_from = n) %>%
  plyr::rename(c(`1` = 'a1', `2` = 'a2')) %>%
  ggplot(., aes(x=a1, y=a2))+
  geom_point(size = 5)+
  ggsidekick::theme_sleek() + 
  labs(x = 'Pop 1', y = 'Pop 2') +
  theme(legend.position = 'none')+
  scale_x_continuous(limits = c(0,2500))+
  scale_y_continuous(limits = c(0,2500))+
  geom_abline(intercept = 0,
              slope = Nmat[nrow(Nmat),2]/Nmat[nrow(Nmat),1], 
              color = 'grey88', lwd = 2) +
  transition_states(dataset, 3, 6) +  shadow_mark() +
  ease_aes('cubic-in-out')
gganimate::anim_save("eig_lineargrowth.gif", eig)

## case 3: true d dep (pgrow varies by N) ----
nsteps = 200
pmove = c(0.6,0.1);  PI = 3.14

Nmat <- matrix(NA, nrow = nsteps, ncol = 2)
Nmat[1,] <- c(1000,1000)

for(s in 2:nrow(Nmat)){
  for(area in 1:ncol(Nmat)){
    pLeave = NCome = 0
    for(jarea in 1:ncol(Nmat)){
      if(area != jarea){
        pLeave = pLeave + pmove[area] ##  leaving for elsewhere
        NCome = NCome +pmove[jarea]*Nmat[s-1,jarea]## mortality in other area
      } # end i != j
    }  # end subareas j
    num <- 4*0.5*Nmat[s-1,area]/2500
    denom1 <- Nmat[s-1,area]/2500*(5*0.5-1)
    denom2 <- (1-0.5)
    Rexp = num/(denom1+denom2)
    cat(Rexp,"\n")
    Nmat[s,area] <- (1-pLeave)*(Nmat[s-1,area]+Rexp) + NCome 
  } ## end area loop for Nmat
} ## end s loop for Nmat
Nmat <- round(Nmat) ## need to be integers


eig <- 
  # mst %>% group_by(dataset,area) %>% summarise(n= n()) %>% 
  # tidyr::pivot_wider(., names_from = area, values_from = n) %>%
  # plyr::rename(c(`1` = 'a1', `2` = 'a2')) %>%
 data.frame(Nmat) %>%
  mutate(dataset = 1:nrow(Nmat)) %>%
  ggplot(., aes(x=X1, y=X2))+
  geom_point(size = 5)+
  ggsidekick::theme_sleek() + 
  labs(x = 'Pop 1', y = 'Pop 2') +
  theme(legend.position = 'none')+
  scale_x_continuous(limits = c(0,2500))+
  scale_y_continuous(limits = c(0,2500))+
  geom_abline(intercept = 0,
              slope = 6.15,#Nmat[nrow(Nmat),2]/Nmat[nrow(Nmat),1], 
              color = 'grey88', lwd = 2) +
  transition_states(dataset, 3, 6) +  shadow_mark() +
  ease_aes('cubic-in-out')
gganimate::anim_save("eig__ddep.gif", eig)

mst <- data.frame(); idx = 1
for(s in 1:nrow(Nmat)){
  for(area in 1:ncol(Nmat)){
    pgrow = Nmat[s,area]/1e5+1 # log(Nmat[s,area])/100+1
    for(N in 1:Nmat[s,area]){
      R <- ifelse(area == 1, 1, 2)*pgrow*s ## emphasize that it's growing via r
      offs <- ifelse(area == 1, 0, 50*pgrow*s) ## move the center 
      a = runif(1,0,100) * 2 * PI
      r = R * sqrt(runif(1,0,100))
      mst[idx,'dataset'] <- s
      ## if this individual was a stayer from last time, label it with home color
      if(s == 1)  mst[idx,'area'] <- area
      if(s >1)  mst[idx,'area'] <- ifelse(N < (1-pmove[area])*Nmat[s-1,area],area,c(1,2)[which(c(1,2)!=area)])
      mst[idx,'x'] <- r * cos(a)+offs
      mst[idx,'y'] <- r * sin(a)
      idx = idx+1
    } ## end N in Nmat
  } ## end area for positions
} ## end timestep

save(mst, file = 'nmat_ddep.rdata')
blob <- ggplot(mst, aes(x=x, y=y, color = factor(area)))+
  geom_point(size = 5)+
  theme_void() +
  theme(legend.position = 'none')+
  scale_color_manual(values = c(demPal[1],demPal[4])) +
  transition_states(dataset, 3, 6) + 
  ease_aes('cubic-in-out')
gganimate::anim_save("blob_ddep.gif", blob)


