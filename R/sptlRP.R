## Revamp of FISH 559 HW3 for Spatial Refpts Paper
## M S Kapur 2020 Summer kapurm@uw.edu

rm(list = ls())
options(scipen = 0)
require(ggplot2, quietly = T)
require(reshape)
require(dplyr, quietly = T)
require(here)
require(ggsidekick)


## load data and introduce some demographic variation among areas
dat0 <- read.table(here("R","HOME3.txt"), header = T)
narea = 3
nages = 21
dat <- array(NA, dim = c(nages,ncol(dat0),narea)) ## placeholder

## first area is original
dat[,,1:3] <- as.matrix(dat0)
## second area, both sexes grow 15% larger, but with same fertility schedule
dat[,3:4,2] <- dat[,3:4,1]*1.5
## third area, productive at early age
dat[1:18,2,3] <- dat[4:21,2,1]
dat[18:20,2,3] <- dat[18,2,3]

par(mfrow = c(narea,3), 
    mar = rep(4,4))
## plot inputs by area
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
## movement matrix (simple) ----
## movement happens only below age 5, and is unidirectional from areas 1 and 2 to area 3
## movement happens only below age 5, and is unidirectional from areas 1 and 2 to area 3
X_ija <- array(NA, dim = c(narea,narea,nages))
for(a in 1:2){ ## only two areas have movement
  for(g in 1:dim(X_ija)[3]){ ## loop ages
    if(g < 6 & a == 1){
      X_ija[a,3,g] <- 0.2 ## 10% movement from a to a3
      X_ija[a,a,g] <- 0.8 ## 10% movement from a to a3
    } else if(g < 6 & a == 2){
      X_ija[a,3,g] <- 0.5 
      X_ija[a,a,g] <- 0.5 
    } else{
      X_ija[a,,g] <- 0 ## no movement at older ages
      diag(X_ija[,,g]) <- 1 
      cat( a, " ",diag(X_ija[,,g]) ,"\n")
      
      
    } # end else
  } ## end ages
} ## end areas
X_ija[is.na(X_ija)] <- 0
X_ija[3,3,] <- 1 ## area 3 is self-seeding
## sanity check - all rows should sum to 1
for(i in 1:dim(X_ija)[3]){
  print(rowSums(X_ija[,,a]) == 1)
}

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

doNage <- function(X = X_ija, ## input movement matrix
                   indat = dat, ## with bio info'
                   s = 1, ## F = 1, M = 2
                   Fv = rep(0,narea),
                   M = 0.15) {
  N_ai <- Z_ai <- B_ai <- SB_ai<- matrix(NA, nrow = nages, ncol = narea) ## placeholder
  
  for(a in 1:nages){
    if(a == 1) N_ai[a,] <- 0.5 ## inits
    for(i in 1:narea){
      Z_ai[a,i] <- M + indat[a,s+4,i]*Fv[i] ## female selex for now (cols 5:6)
      
      if(a > 1  & a < max(nages)) {
        pLeave = NCome = 0
        for(j in 1:narea){
          if(i != j){
            pLeave = pLeave + X_ija[i,j,a-1]
            NCome = NCome + X_ija[j,i,a-1]*N_ai[a-1,j]
            # if(is.na(NCome)) stop("NA NCOME at",a,i,j,"\n")
          } # end i != j
        } # end subareas j
        N_ai[a,i] <- ((1-pLeave)* N_ai[a-1,i] +NCome)*exp(-Z_ai[a-1,i])
        # if(is.na(N_ai[a,i])) stop("NA NAI at",a,i,"\n")
      } ## end age < maxage
      if(a == max(nages)) N_ai[a,i] <-  N_ai[a-1,i]*exp(-Z_ai[a-1,i])/(1- exp(-Z_ai[a,i]))
    } # end ages
    B_ai[a,i] <- N_ai[a,i]*indat[a,s+2,i] ## weight in 3 and 4 col
    if(s == 1){
      SB_ai[a,i]  <- NA
      SB_ai[a,i]  <- B_ai[a,i]*indat[a,1,i]
    } 
    B_i <- sum(B_ai[,i])
    SB_i <- sum(SB_ai[,i])
  } ## end subareas i
  return(cbind(N_ai,Z_ai,sum(B_i), sum(SB_i)))
}
## returns area-specific N@age and Z@age
doNage(s = 1)[,1:3] %>%
  data.frame() %>%
  mutate(Age = 1:nages) %>%
  reshape2::melt(id = 'Age') %>%
  mutate(Area = substr(variable,2,2)) %>%
  ggplot(., aes(x = Age, y = value, col = Area)) +
  geom_line(lwd = 1.1) + 
  scale_color_grey() +
  labs(x = 'Age', y = 'Numbers', color = 'Area') +
  theme_sleek()



## Return area-specific unfished spawning biomass




doYPR <-function( Fv= rep(0.2,narea), M = 0.15 ) {
  ypr_sa <- array(NA, dim = c(1,2,narea)) ## each sex by area
  ypr_a <- matrix(NA, nrow = 1, ncol = narea) ## each  area
  ypr  <- NA  ## total
  for(i in 1:narea){
    for(s in 1:2){ ## loop sexes
      wt <- dat[,s+2,i] ## cols 2 & 3
      nzmat <-  doNage(s = s, Fv = Fv) ## expects 3 FV values
      n_a <- nzmat[,i]
      z_a <- nzmat[,i+3]
      ## Baranov
      ypr_sa[1,s,i] <- sum(wt*(( dat[,s+4,i]*Fv[i])/z_a)*n_a*(1-exp(-z_a)))
    } ## end sexes
    ypr_a[i] <- sum(ypr_sa[1,,i])
  } ## end areas
  ypr <- sum(ypr_a)
  return(list(ypr_sa,ypr_a,ypr))
}


getAB <- function(SRR = 1, h = 0.5, R0 = 1, gam = 1){
  sbpr0 <- NULL
  for(i in 1:narea){
    ## always calculate at F = 0
    sbpr0[i] <-   sum(dat[,2,i] * doNage(Fv = rep(0, narea))) 
    alpha <- sbpr0[i]*((1-h)/(4*h))
    beta <- (5*h-1)/(4*h*R0) 
  }
  #list(dat$Sel.f., dat$Sel.m.)[[1]])[, 1])
  # if(SRR == 1){ 
  ## bev-hold
  
  # } else  if(SRR == 2){
  #   alpha <- exp(log(5*h)/0.8)/sbpr0
  #   beta <- log(5*h)/(0.8*sbpr0*R0)
  # } else   if(SRR == 3){
  #   alpha <- 1/sbpr0
  #   beta <- (5*h-1)/(1 - 0.2^gam) 
  # }
  return(c("alpha" = alpha, "beta" = beta))
}

doSRR <- function( SRR = 1, h = 0.5, 
                   Fv = rep(0,narea), gam = 1, R0 = 1, S0 = 0.6739975){
  sumSBPR <- R <- NULL
  
  for(i in 1:narea){
    
    ## get s-tilde
    sumSBPR[i] <- sum(dat[,2,i]*doNage(Fv = Fv))
    # sumSBPR <- sum(sbpr)
    # if(SRR == 1){ ## bevholt
    
    ab <- getAB(SRR = 1, h = h)
    R[i] <- (  sumSBPR[i] - ab[1] )/(ab[2] *   sumSBPR[i]) ## Equation 9
    rm(ab)
    # } else if(SRR == 2){ ## ricker
    #   ab <- getAB(SRR = 2, h = h)
    #   R <- log(ab[1]*sumSBPR)/(ab[2]*sumSBPR) ## Question 1A
    # } else if(SRR == 3){ ## Pella
    #   ab <- getAB(SRR = 3, h = h)
    #   R <- (S0/sumSBPR) * (1 - (1 - ab[1] * sumSBPR)/(ab[2] * ab[1] * sumSBPR))^(1/gam) ## Question 1B
  }
  return(list('rec' = as.numeric(R),'spawnbio' = sumSBPR))
}

doYield <- function(ypr, R){
  yield_a <- yield_tot <- NULL
  for(i in 1:narea){
    yield_a[i] <- ypr[i]*R[i]
  }
  yield_tot <- sum(yield_a)
  return(list(yield_a,yield_tot))
} 

## put it all together
masterFunc <-
  function(
    Fv = rep(0,narea),
    SRR = 1,
    h = 0.5,
    gam = 1,
    R0 = 1,
    S0 = 0.6739975) {
    
    ypr_a <- doYPR(Fv = Fv)[[2]] ## second element is per area
    rec_sb <-  doSRR( SRR = SRR, h = h, Fv = Fv,gam = gam, R0 = R0,S0 = S0)
    
    yield <- doYield(ypr_a,rec_sb$rec) ## second object is total
    
    df <- data.frame(  
      "SRR" = c('BevHolt','Ricker','Pella-T')[SRR],
      "Fmort" = Fv,
      "rec" = rec_sb$rec,
      "yield" = yield[[1]],
      "spawnbio" = rec_sb$spawnbio) #rec[2]*rec[1])    ## eq 7
    
    return(df)
  }


## brute plot (YPR and Yield vs F) ----
brute <- data.frame('FV_sys' = NA, "yield_1" = NA, "yield_2" = NA,"yield_3" = NA,
                    "ypr_1" = NA, "ypr_2" = NA,"ypr_3" = NA)
Fv_test <- seq(0,1,0.01)
for( i in 1:length(Fv_test)){
  temp <- masterFunc(SRR = 1, h = steep, Fv = rep(Fv_test[i],narea))
  temp2 <- doYPR(Fv = rep(Fv_test[i],narea))
  brute[i,'FV_sys'] = Fv_test[i]
  for(a in 1:narea) brute[i,a+1] = temp$yield[a]
  for(a in 1:narea) brute[i,a+4] = temp2[[2]][a]
  
}
## YPR
brute[,c(1,5:7)] %>%
  melt(id = 'FV_sys') %>%
  ggplot(., aes(x = FV_sys, y = value, color = variable)) +
  geom_line(lwd = 1.1) + 
  scale_color_grey(labels = paste("Area",1:3)) +
  labs(x = 'F in area', y = 'YPR', color = '') +
  # scale_y_continuous(limits = c(0,0.5)) +
  theme_sleek() + 
  theme(legend.position = c(0.8,0.9), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 20))
## Yield
brute[,c(1:4)] %>%
  melt(id = 'FV_sys') %>%
  ggplot(., aes(x = FV_sys, y = value, color = variable)) +
  geom_line(lwd = 1.1) + 
  scale_color_grey(labels = paste("Area",1:3)) +
  labs(x = 'F in area', y = 'Yield', color = '') +
  scale_y_continuous(limits = c(0,0.5)) +
  theme_sleek() + 
  theme(legend.position = c(0.8,0.9), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 20))

## put it all together
masterFunc <-
  function(
    Fv = rep(0.15,narea),
    SRR = 1,
    h = steep,
    gam = 1,
    R0 = 1,
    S0 = 0.6739975) {
    
    ypr_a <- doYPR(Fv = Fv)[[2]] ## second element is per area
    rec_sb <-  doSRR( SRR = SRR, h = steep, Fv = Fv,gam = gam, R0 = R0,S0 = S0)
    
    yield <- doYield(ypr_a,rec_sb$rec) ## second object is total
   
    ## results by area
    df <- data.frame(  'Area' = 1:3,
      "SRR" = c('BevHolt','Ricker','Pella-T')[SRR],
      "Fmort" = Fv,
      "rec" = rec_sb$rec,
      "yield" = yield[[1]],
      "spawnbio" = rec_sb$spawnbio) #rec[2]*rec[1])    ## eq 7
    
    return(df)
  }


## this needs to be spatial
## find where F is minimized and S ~ 0, bisection
# bisect <- function(Fmin = 0, Fmax = 1){
#   for(b in 1:1000){
#     Fv_testM <- (Fmin + Fmax)/2 ## update
#     sbio_temp <- masterFunc(SRR = s, Fv = Fv_testM)$spawnbio
#     if(round(sbio_temp,4) == 0 & (Fmax - Fmin) > 0.0002){ return(Fv_testM)
#     } else if(round(sbio_temp,4) > 0) { Fmin <- Fv_testM 
#     } else if(round(sbio_temp,4) < 0) { Fmax <- Fv_testM }
#   }
#   print('max iter')
# }

## uniroot sols
dfx.dxSYS <- function(Fv_test, h = steep){
  y1 <- masterFunc(SRR = 1, h = steep, Fv = rep(Fv_test-0.001,narea))$yield
  y2 <- masterFunc(SRR = 1, h = steep, Fv = rep(Fv_test+0.001,narea))$yield
  appx <- (sum(y2)-sum(y1))/(0.002) #0.002 is total X delta; we are using system yield
  return(appx)
}

## systemwide but with separate FV_test
dfx.dxSYS2 <- function(Fv_testa1, Fv_testa2, h = steep){
  y1 <- masterFunc(SRR = 1, h = steep, Fv = c(Fv_testa1-0.001,Fv_testa2- 0.001))$yield
  y2 <- masterFunc(SRR = 1, h = steep, Fv = c(Fv_testa1+0.001,Fv_testa2+ 0.001))$yield
  appx <- (sum(y2)-sum(y1))/(0.002) #0.002 is total X delta; we are using system yield
  return(appx)
}

dfx.dxAREA <- function(Fv_test, h = steep){
  y1 <- masterFunc(SRR = 1, h = steep, Fv = rep(Fv_test-0.001,narea))$yield[3]
  y2 <- masterFunc(SRR = 1, h = steep, Fv = rep(Fv_test+0.001,narea))$yield[3]
  appx <- (y2-y1)/(0.002) #0.002 is total delta
  return(appx)
}

## brute force - try various configs & save delta to search
# Fv_config <- expand.grid(seq(0.02,1,0.1),seq(0.02,1,0.1),seq(0.02,1,0.1))
# 
# # https://stackoverflow.com/questions/50978973/looping-uniroot-on-two-arguments-with-sapply
# 
# dfx.dxCONFIGA <- function(k, h = steep){
#   appx = NULL
#   # for(k in 1:nrow(Fv_config)){
#     y1 <- masterFunc(SRR = 1, h = steep, Fv = as.numeric(Fv_config[k,]-0.001))$yield
#     y2 <- masterFunc(SRR = 1, h = steep, Fv = as.numeric(Fv_config[k,]+0.001))$yield
#     appx[k] <- (sum(y2)-sum(y1))/(0.002) #0.002 is total delta; we are using system yield
#   # } # end configs
#   return(appx) ## want to minimize all 3
# }

minFunc <- function(F1,F2,F3){
  minus <- as.numeric(c(F1 - 0.001, F2 - 0.001, F3 - 0.001))
  plus <- as.numeric(c(F1 +0.001, F2 + 0.001, F3 + 0.001))
  y1 <- masterFunc(SRR = 1, h = steep, Fv = minus)$yield
  y2 <- masterFunc(SRR = 1, h = steep, Fv = plus)$yield
  appx <- (sum(y2)-sum(y1))/(0.002) ## system yield again
  return(appx)
}


## uniroot/find FMSY ----
df2 <- data.frame(
  expand.grid('Area' = 1:3,
  'Method' = c('Fmsy_System','Fmsy_Config')),
                 'FMSY' = NA,
                 'MSY' = NA,
                 'BMSY' = NA,
  'B0')

## single F, maximize system yield
df2$FMSY[df2$Method == 'Fmsy_System'] <- as.numeric(uniroot(f = dfx.dxSYS,  h = 0.86, interval = c(0.02,1))[1])
df2$MSY[df2$Method == 'Fmsy_System'] <-  masterFunc(SRR = 1, Fv = df2$FMSY[df2$Method == 'Fmsy_System'])$yield
df2$BMSY[df2$Method == 'Fmsy_System'] <- masterFunc(SRR = 1, Fv = df2$FMSY[df2$Method == 'Fmsy_System'])$spawnbio

## unique Fs ('config'), maximize system yield
df2$FMSY[df2$Method == 'Fmsy_Config'] <- coef(mle(minFunc, start = list(F1 = 0.025, F2 = 0.025, F3 = 0.025),
                                                  method = "L-BFGS-B",
                                                  lower = c(0.02, 0.02,0.02), upper = c(1,1,1)))
df2$MSY[df2$Method == 'Fmsy_Config'] <-  masterFunc(SRR = 1, Fv = df2$FMSY[df2$Method == 'Fmsy_Config'])$yield
df2$BMSY[df2$Method == 'Fmsy_Config'] <- masterFunc(SRR = 1, Fv = df2$FMSY[df2$Method == 'Fmsy_Config'])$spawnbio

## change catchability assumption (Langseth & Schueller 2016)
## this is the Sampson & Scott 2011 Method; see Maury et al...





## MSY by approach

##BMsy by approach
BMSY_sys <-
BMSY_area <- masterFunc(SRR = s, Fv = rep(FMSY_area,narea))$spawnbio
BMSY_config <- masterFunc(SRR = s, Fv = FMSY_config)$spawnbio


Fcrash <- bisect()
q3b[s,] <- c( c('BevHolt','Ricker','Pella-T, gamma = 1')[s], round(FMSY,4),
              round(MSY,4), round(Fcrash,4))

