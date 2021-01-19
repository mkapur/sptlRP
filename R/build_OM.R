## read data, set up movement matrix 
## movement happens only below age 10, and is unidirectional from areas 1 and 2 to area 3

## load data and introduce some demographic variation among areas
dat0 <- read.table(here::here("omdata.txt"), header = T)
dat0[,4] <- dat0[,3] ## no sexual dimorphism in growth
for(a in 1:nrow(dat0)){
  ## ascending logistic function for selex [a50,a95]
  dat0[a,5:6] <- 1/ ( 1 + exp(-log(19) * (a -  5) / ( 10-5)))
  ## ascending logistic function for fec
  dat0[a,2] <- 1/ ( 1 + exp(-log(19) * (a -  3) / ( 7-3)))
}
dat0$M_age =  c(rep(0.2,10), rep(0.15,11))

dat <- array(NA, dim = c(nrow(dat0),ncol(dat0),narea)) ## placeholder
for(i in 1:narea) dat[,,i] <- as.matrix(dat0)


## plot inputs by area ----
png( here('figs','inputs_by_2area.png'),
     width = 8, height = 8, unit = 'in', res = 520)
dattemp <- as.data.frame(dat[,,1]) ## inputs are the same by area
names(dattemp) = names(dat0)

par(mfrow = c(2,2), 
    mar = c(4,4,1,1))
## F Fecundity
with(dattemp, plot(Fecundity ~ Age, type = 'l', lwd = 2,yaxt = 'n',
                   xaxt = 'n', ylim = c(0,1.05),
                   ylab = 'Fecundity (females)', col = 'grey44'))
axis(2, at  = seq(0,1.0,0.25), labels = seq(0,1.0,0.25), cex.axis =1)
axis(1, at  = seq(0,21,1), labels = seq(0,21,1), cex.axis =1)
text(0,1,"A", cex = 2, col = 'grey22')
## weight
with(dattemp, plot(Wght.f. ~ Age, type = 'l', lwd = 2, ylim = c(0,2),  yaxt = 'n',
                   xaxt = 'n',
                   ylab = 'Weight (Both Sexes)', col = 'grey44'))
axis(2, at  = seq(0,2,0.5), labels = seq(0,2,0.5), cex.axis =1)
axis(1, at  = seq(0,21,1), labels = seq(0,21,1), cex.axis =1)
text(0,2,"B", cex = 2, col = 'grey22')
## M/F selex
with(dattemp, plot(Sel.f. ~ Age, type = 'l', lwd = 2, 
                   yaxt = 'n',
                   xaxt = 'n', ylim = c(0,1), ylab = 'Selectivity (Both Sexes)',
                   col = 'grey44'))
axis(2, at  = seq(0,1.0,0.25), labels = seq(0,1.0,0.25), cex.axis =1)
axis(1, at  = seq(0,21,1), labels = seq(0,21,1), cex.axis =1)
text(0,1,"C", cex = 2, col = 'grey22')
## Mortality at age
with(dattemp, plot(M_age ~ Age, type = 'l', lwd = 2, 
                   ylim = c(0,0.5),
                   yaxt = 'n',
                   xaxt = 'n',  
                   ylab = 'Natural Mortality (Both Sexes)',
                   col = 'grey44'))
axis(2, at  = seq(0,0.5,0.1), labels = seq(0,0.5,0.1), cex.axis =1)
axis(1, at  = seq(0,21,1), labels = seq(0,21,1), cex.axis =1)
text(0,0.5,"D", cex = 2, col = 'grey22')
dev.off()

## build x_ija----
#* R1 version ----
X_ija_all <- X_ija_10 <- X_ija_NULL  <- array(NA, dim = c(narea,narea,nages))

for(a in 1:2){ ## only two areas have movement
  for(g in 1:dim(X_ija_NULL)[3]){ ## loop ages
    diag(X_ija_NULL[,,g]) <- rep(1, length(  diag(X_ija_NULL[,,g])))
    X_ija_all[1,1,g] <- X_ija_10[1,1,g] <- 0.8
    X_ija_all[2,2,g] <- X_ija_10[2,2,g] <- 0.6
    X_ija_all[1,2,g] <- X_ija_10[1,2,g] <- 0.2
    X_ija_all[2,1,g] <- X_ija_10[2,1,g] <- 0.4
    if(g > 10){
      ## movement ceases after a10 (overwrite)
      X_ija_10[1,1,g] <- 1
      X_ija_10[2,2,g] <- 1
      X_ija_10[1,2,g] <- 0
      X_ija_10[2,1,g] <- 0
    }
  } ## end ages
} ## end areas



#* S1 versions----
# if(narea == 3){
#   X_ija_NULL <- array(0, dim = c(narea,narea,nages))
#   X_ija_MIX <- array(0, dim = c(narea,narea,nages))
#   X_ija <- array(NA, dim = c(narea,narea,nages))
# for(a in 1:2){ ## only two areas have movement
# 
#   for(g in 1:dim(X_ija)[3]){ ## loop ages
#     diag(X_ija_NULL[,,g]) <- rep(1, length(  diag(X_ija_NULL[,,g])))
#     if(g < 6 & a == 1){
#       X_ija[a,3,g] <-   X_ija_MIX[a,3,g]  <- 0.05 ## 20% movement from a to a3
#       X_ija[a,a,g] <-   X_ija_MIX[a,a,g] <- 0.95 ## retained
#       X_ija_MIX[3,1,g] <- 0.3 ## send 20% back from a3
#       X_ija_MIX[3,3,g] <- 0.7 
#     } else if(g < 6 & a == 2){
#       X_ija[a,3,g] <- X_ija_MIX[a,3,g]  <- 0.15
#       X_ija[a,a,g] <-  X_ija_MIX[a,a,g] <- 0.85
# 
#     } else{
#       X_ija[a,,g] <- 0 ## no movement at older ages
#       diag(X_ija[,,g]) <- 1 
#       diag(X_ija_MIX[,,g]) <- 1 
#       # cat( a, " ",diag(X_ija[,,g]) ,"\n")
#     } # end else
#   } ## end ages
# } ## end areas
# X_ija[is.na(X_ija)] <- 0
# X_ija[3,3,] <- X_ija_NULL[3,3,] <- 1 ## area 3 is self-seeding
# ## sanity check - all rows should sum to 1
# for(i in 1:dim(X_ija)[3]){
#   print(rowSums(X_ija[,,a]) == 1)
# }
# } else if (narea == 2){
# 
#   X_ija_EQUAL <- X_ija_NULL2 <- X_ija_MIX2 <-X_ija_MIX2b <- X_ija_UNI2 <- array(NA, dim = c(narea,narea,nages))
# 
#   for(a in 1:2){ ## only two areas have movement
#     for(g in 1:dim(X_ija_EQUAL)[3]){ ## loop ages
#       diag(X_ija_NULL2[,,g]) <- rep(1, length(  diag(X_ija_NULL2[,,g])))
#       if(g < 6){
#         X_ija_UNI2[1,1:2,g] <- 0.5
#         X_ija_UNI2[2,1,g] <- 0
#         X_ija_UNI2[2,2,g] <- 1
#         
#         X_ija_MIX2[2,1,g] <- 0.25 
#         X_ija_MIX2[2,2,g] <- 0.75 
#         X_ija_MIX2[1,1,g] <- 0.8 
#         X_ija_MIX2[1,2,g] <- 0.2
#         
#         X_ija_MIX2b[2,1,g] <- 0.5 
#         X_ija_MIX2b[2,2,g] <- 0.5 
#         X_ija_MIX2b[1,1,g] <- 0.8 
#         X_ija_MIX2b[1,2,g] <- 0.2
#         
#         X_ija_EQUAL[1,1,g]  <- X_ija_EQUAL[2,2,g] <- 0.5
#         X_ija_EQUAL[1,2,g]  <- X_ija_EQUAL[2,1,g] <- 0.5
#       } else{
#         X_ija_EQUAL[a,,g] <- X_ija_MIX2[a,,g] <- X_ija_MIX2b[a,,g] <- X_ija_UNI2[a,,g] <- 0 ## no movement at older ages
#         diag(X_ija_EQUAL[,,g]) <- 1 
#         diag(X_ija_MIX2[,,g]) <- 1 
#         diag(X_ija_MIX2b[,,g]) <- 1 
#         # cat( a, " ",diag(X_ija[,,g]) ,"\n")
#       } # end else
#     } ## end ages
#   } ## end areas
#   X_ija_NULL2[is.na(X_ija_NULL2)] <- 0
# } ## end only 2 areas

## plot xija----

p_all <-  data.frame(X_ija_all[,,1]) %>%
  mutate('FRM' = 1:narea) %>%
  reshape2::melt(id = 'FRM') %>%
  ggplot(., aes(x = FRM, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = 'grey22', high = 'grey66') +
  ggsidekick::theme_sleek() +
  theme(legend.position = 'none',
        axis.ticks = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) +
  scale_x_continuous(expand = c(0,0), breaks = 1:narea,
                     labels = paste("Area",1:narea)) +
  scale_y_discrete(expand = c(0,0), labels = paste("Area",1:narea))+
  geom_text(aes(label = value), colour = 'white', size = 10) +
  labs(x = "Area From", y = "Area To",
       title = 'Scenario 1 Movement (all ages)')

p_10_1 <-  data.frame(X_ija_10[,,1]) %>%
    mutate('FRM' = 1:narea) %>%
    reshape2::melt(id = 'FRM') %>%
    ggplot(., aes(x = FRM, y = variable, fill = value)) +
    geom_tile() +
    ggsidekick::theme_sleek() +
    theme(legend.position = 'none',
          axis.ticks = element_blank(),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10)) +
    scale_x_continuous(expand = c(0,0), breaks = 1:narea,
                       labels = paste("Area",1:narea)) +
    scale_y_discrete(expand = c(0,0), labels = paste("Area",1:narea))+
    geom_text(aes(label = value), colour = 'white', size = 10) +
    labs(x = "Area From", y = "Area To",
  title =  paste('Scenario 2 Movement (Ages 1-10)'))
p_10_11 <-  data.frame(X_ija_10[,,10]) %>%
  mutate('FRM' = 1:narea) %>%
  reshape2::melt(id = 'FRM') %>%
  ggplot(., aes(x = FRM, y = variable, fill = value)) +
  geom_tile() +
  ggsidekick::theme_sleek() +
  theme(legend.position = 'none',
        axis.ticks = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) +
  scale_x_continuous(expand = c(0,0), breaks = 1:narea,
                     labels = paste("Area",1:narea)) +
  scale_y_discrete(expand = c(0,0), labels = paste("Area",1:narea))+
  geom_text(aes(label = value), colour = 'white', size = 10) +
  labs(x = "Area From", y = "Area To",
       title =   "Scenario 2 Movement (Ages 11+)")
  # 
require(patchwork)


ggsave(p_all / (p_10_1  | p_10_11),
       file = here('figs','X_ija_2scen.png'),
       width = 8, height = 10, unit = 'in', dpi = 520)

## return unfished spawning biomass depending on method
getSB0 <- function(eq_method){
  SB0_i <- apply(doNage(eq_method = eq_method, Fv = rep(0,narea))[,10:12],2,sum)
  return(SB0_i)
}
