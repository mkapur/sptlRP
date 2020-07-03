rm(list = ls())

## for running example
require(reshape)
require(dplyr, quietly = T)
require(here)
require(stats4)

## for plotting
require(patchwork)
require(ggsidekick)
require(ggplot2, quietly = T)

## settings
narea <- 3
nages <- 21
steep <- 0.26 ## global
recr_dist <- c(0.5,0.3,0.2) ## global recruits to areas
rec_level <- 100 ## reference recruitment level "R"
R0 <- 500 ## virgin recruitment

## load functions & initialize OM
lapply(list.files(here("R"), full.names = TRUE), source)

## Current SS Approach ----

## define virgin biomass
SB0 <- doNage( Fv = rep(0,narea), 
               X = X_ija,
               rdist = recr_dist,
               refR = rec_level)$SB_total

## Get NAA using movement. Input X_ija to turn movement OFF (smooth curve)
## applying system-wide F
Ftest <- seq(0,1,0.005)
current <- data.frame(Fv = NA, Yield = NA, B = NA)
for(v in 1:length(Ftest)){
  
  ## get values at present Fv
  curr <- doNage( Fv = rep(Ftest[v],narea), 
                  X = X_ija,
                  rdist = recr_dist,
                  refR = rec_level)

  # calc SPB/R and Yield/R
  SB_R <- curr$SB_total/rec_level
  Yield_R <- curr$Yield_total/rec_level
  
  #call Equil_Spawn_Recr_Fxn to get B_equil and R_equil from SPB/R and SR parms
  currEq <- Equil_Spawn_Recr_Fxn(steepness = steep, SSB_virgin = SB0, 
                                 Recr_virgin = R0, SPR_temp = SB_R)## L17247 ON TPL
  current[v,'Fv'] <- Ftest[v]
  current[v,'Yield'] <- Yield_R * currEq$R_equil
  current[v,'B'] <- SB_R* currEq$R_equil#*currEq$B_equil
}




## proposed approach ----

## applying system-wide F

proposed <- data.frame(Fv = NA, Yield = NA, B = NA)
proposed_i <- array(NA, dim = c(length(Ftest),3,narea), dimnames = list(NULL,c('Fv','Yield',"B"))) ## now for each area
B_eq_i <- R_eq_i <- B_eq_i_INIT <- R_eq_i_INIT <- SB_Ri <- Yield_Ri<- matrix(NA, nrow =length(Ftest), ncol = narea)
rick <- data.frame()

for(v in 1:length(Ftest)){

  for(k in 1:2){ ## Loop over steps A & B
    
    if(k == 1){
      rdistUse <- recr_dist
      rlevelUse <- rec_level
    } else{
      rdistUse <- recr_dist_adj
      rlevelUse <-   c(rec_level, rick[v,'R_ESUMB'],rick[v,'R_SUMEBA'])[1] ## should this be sum(Req?), or eq(Sum Ba)?
    }
    
    ## define virgin biomass by AREA
    SB0_i <- doNage( Fv = rep(0,narea), 
                     X = X_ija,
                     rdist = rdistUse,
                     refR = rlevelUse)$SB_i
    
    ## get values at present Fv
    prop <- doNage( Fv = rep(Ftest[v],narea), 
                    X = X_ija,
                    rdist = rdistUse,
                    refR = rlevelUse) 
    
    # call Equ_Spawn_Recr_Fxn for each area to get B_equil and R_equil from SPB/R and SR parms
    for(i in 1:narea){ ## will overwrite second time
      # calc area-specific SPB/R and Yield/R, using area-specific R
      if(length(rlevelUse) > 1){
        rleveltmp = rlevelUse[i]
      } else{
        rleveltmp = rlevelUse
      }
      
      SB_Ri[v,i] <- prop$SB_i[i]/(rleveltmp*rdistUse[i])
      Yield_Ri[v,i] <- prop$Yield_i[i]/(rleveltmp*rdistUse[i])
      ## Calc area-specific recruits using area-specific SB etc
      propEq <- Equil_Spawn_Recr_Fxn(steepness = steep, SSB_virgin = SB0_i[i],
                                     Recr_virgin = R0*rdistUse[i], SPR_temp = SB_Ri[v,i])
      B_eq_i[v,i] <- propEq$B_equil
      R_eq_i[v,i] <- propEq$R_equil
      # if(v ==75 ) cat(k," ",i," ",B_eq_i[v,i], "\n")
      if(k == 2){ ## store quantities
        proposed_i[v,'Fv',i] <- Ftest[v]
        proposed_i[v,'Yield',i] <-  Yield_Ri[v,i]*R_eq_i[v,i]
        proposed_i[v,'B',i] <-    SB_Ri[v,i] *R_eq_i[v,i]
        # if(v > 75 & i == 1) cat(Ftest[v]," ", proposed_i[v,'B',i] , "\n");
      } ## end k == 2
    } ## end areas    
   
    if(k == 1){
      # use ratio of B_equils among areas to calculate adjustment to recr_dist 
      recr_dist_adj <- B_eq_i[v,]/sum(B_eq_i[v,]);
      rick[v,"Fv"] <- Ftest[v]
      rick[v,"SBeqtotal"] <-   sum(B_eq_i[v,] )
      
      rick[v,"R_SUMEBA"]  <- sum( R_eq_i[v,]) ## sum of expected recruits in areas
      
      ## two options for calculating new R ref
      rick[v,"R_ESUMB"] <- Equil_Spawn_Recr_Fxn(steepness = steep, SSB_virgin = sum(SB0_i),
                                                Recr_virgin = R0, SPR_temp = sum(SB_Ri[v,]))$R_equil ## expected recruits given summed biomass in area
    
      
      ## rick's question
    }
    next()
    # if(v > 75 & k ==2 ) cat(Ftest[v]," ",rdistUse, "\n");

    
    
    # if(k == 1) recr_dist_adj <- B_eq_i[v,]/sum(B_eq_i[v,])*recr_dist; next()
    
  } ## end steps a/b loop
  

  proposed[v,'Fv'] <- Ftest[v]
  proposed[v,'Yield'] <- sum(Yield_Ri[v,] * R_eq_i[v,]) # sum(Yield_Ri[v,]) * sum(R_eq_i[v,])
  proposed[v,'B'] <-  sum(SB_Ri[v,] *  R_eq_i[v,i])  #sum(SB_Ri[v,]) * sum(B_eq_i[v,]) 
} ## end FV


## sanity check --
p1 <- ggplot(current, aes(x = Fv, y = Yield)) + 
  geom_line(lwd = 1.1, aes(color = 'current')) + 
  geom_line(data = proposed, lwd = 1.1, aes(color = 'proposed')) +
  scale_color_manual(values = c('seagreen','goldenrod')) +
  theme_sleek() +theme(legend.position = 'none') 

## i think the hitch is where F is cancelling out recruitment subsidy
# p2 <- ggplot(current, aes(x = Fv, y = B)) + 
#   geom_line(lwd = 1.1, aes(color = 'current')) + 
#   geom_line(data = proposed, lwd = 1.1, aes(color = 'proposed')) + 
#   theme_sleek()

p3 <- ggplot(current, aes(x = B, y = Yield)) + 
  geom_line(lwd = 1.1, aes(color = 'current')) + 
  geom_line(data = proposed, lwd = 1.1, aes(color = 'proposed')) + 
  scale_color_manual(values = c('seagreen','goldenrod')) +
  labs(color = "Approach") + 
  theme_sleek() +theme(legend.position = c(0.8,0.8))


p1  | p3
ggsave(last_plot(),
       file = here('figs',"Yield_Comparison.png"),
       width = 8, height = 6, unit = 'in', dpi = 420)


p4 <- ggplot( ) +
  geom_line(data = data.frame(proposed_i[,,1]), aes(x = Fv, y = Yield, col = 'Area 1') ) +
  geom_line(data = data.frame(proposed_i[,,2]), aes(x = Fv, y = Yield, col = 'Area 2') ) +
  geom_line(data = data.frame(proposed_i[,,3]), aes(x = Fv, y = Yield, col = 'Area 3') ) +
  labs(x = 'F', color = 'Area', title = 'Yield vs F by Area') + 
  theme_sleek() +theme(legend.position = 'none') 
  # geom_vline(xintercept = Ftest[which(proposed_i[,2,1] > proposed_i[,3,1])[1]]) #+ ## where Y > B in area 1
  # geom_vline(xintercept = Ftest[which.max(proposed_i[,2,3])]) ## where Y > B in area 1

# b <- which(proposed_i[,2,2] > proposed_i[,3,2])[1]
b <- which.max(proposed_i[,2,3])

p5 <- ggplot( ) +
  geom_line(data = data.frame(proposed_i[,,1]), aes(x = B, y = Yield, col = 'Area 1') ) +
  geom_line(data = data.frame(proposed_i[,,2]), aes(x = B, y = Yield, col = 'Area 2') ) +
  geom_line(data = data.frame(proposed_i[,,3]), aes(x = B, y = Yield, col = 'Area 3') ) +
  labs(x = 'B', color = 'Area', title = 'Yield  vs B by Area', 
       subtitle = 'Vert Line @ Area 2 Crashing & Ymax Area 3') + 
  theme_sleek() +
  geom_vline(xintercept = proposed_i[b,3,3])+theme(legend.position = 'none') 
# geom_vline(xintercept = Ftest[which.max(proposed_i[,2,3])]) ## where Y > B in area 1

p6 <- ggplot( ) +
  geom_line(data = data.frame(proposed_i[,,1]), aes(x = Fv, y = B, col = 'Area 1') ) +
  geom_line(data = data.frame(proposed_i[,,2]), aes(x = Fv, y = B, col = 'Area 2') ) +
  geom_line(data = data.frame(proposed_i[,,3]), aes(x = Fv, y = B, col = 'Area 3') ) +
  labs(x = 'F', color = 'Area', title = 'B vs F by Area') +
  theme_sleek() +theme(legend.position = c(0.8,0.8))

(p4  |p5  |p6)

ggsave(last_plot(),
       file = here('figs',"Yield_by_area.png"),
       width = 10, height = 8, unit = 'in', dpi = 420)

rick %>%
  select(-Fv) %>%
  melt(id = 'SBeqtotal') %>%
ggplot(., aes(x = SBeqtotal, y = value, color = variable)) +
  geom_point()
  theme_sleek() +
  # scale_color_manual(labels = c(expression(R=E)))
  labs(x = "Fv", y = 'Recruitment', color = 'Approach')

  
  proposed_i[,,3] %>% as.data.frame() %>% filter(B < Yield)
## AREA SPECIFIC YIELD CURVES
plot(proposed_i[,,3], xlim = c(0,1), col = 'red')
points(proposed_i[,,2]) ## ANYTHING GREATER THAN ROW 75 HAS yield > B
points(proposed_i[,,1])
  
plot(proposed_i[,c(1,3),3], ylim = c(0,1500), col = 'red')
points(proposed_i[,c(1,3),2])
points(proposed_i[,c(1,3),1])

