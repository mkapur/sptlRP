## Revamp of FISH 559 HW3 for Spatial Refpts Paper
## M S Kapur 2020 Summer kapurm@uw.edu

rm(list = ls())
options(scipen = 0)
require(ggplot2, quietly = T)
require(reshape)
require(dplyr, quietly = T)
require(here)


## load data and introduce some demographic variation among areas
dat0 <- read.table(here("R","HOME3.txt"), header = T)
narea = 3
dat <- array(NA, dim = c(nrow(dat0),ncol(dat0),narea)) ## placeholder

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
for(a in 1:narea){
  dattemp <- as.data.frame(dat[,,a])
  names(dattemp) = names(dat0)
  ## F Fecundity
  with(dattemp, plot(Fecundity ~ Age, type = 'l', lwd = 2, ylim = c(0,1), ylab = 'Fecundity', col = 'seagreen4',  cex.axis =1.5, cex.lab= 1.5))
  ## M/F weights
  with(dattemp, plot(Wght.f. ~ Age, type = 'l', lwd = 2, ylim = c(0,3), ylab = 'Weight', col = 'seagreen4',  cex.axis =1.5, cex.lab= 1.5))
  with(dattemp, lines(Wght.m. ~ Age, lwd = 2, ylab = 'Weight', col = 'goldenrod',  cex.axis =1.5, cex.lab= 1.5))
  legend('bottomright', legend = c('Female','Male'), col = c('seagreen4','goldenrod'), lwd = 2)
  ## M/F selex
  with(dattemp, plot(Sel.f. ~ Age, type = 'l', lwd = 2,  ylim = c(0,1), ylab = 'Selectivity', col = 'seagreen4',  cex.axis =1.5, cex.lab= 1.5))
  with(dattemp, lines(Sel.m. ~ Age, lwd = 2, ylab = 'Selectivity', col = 'goldenrod',  cex.axis =1.5, cex.lab= 1.5))
  legend('bottomright', legend = c('Female','Male'), col = c('seagreen4','goldenrod'), lwd = 2)
}

## movement matrix (simple)
## movement happens only below age 5, and is unidirectional from areas 1 and 2 to area 3
X_ija <- array(NA, dim = c(narea,narea,nrow(dat0)))
for(a in 1:2){ ## only two areas have movement
  for(g in 1:dim(X_ija)[3]){
    if(g < 5){
      X_ija[a,3,g] <- 0.1 ## 10% movement from a to a3
      X_ija[a,a,g] <- 0.9 ## 10% movement from a to a3
    } else{
      X_ija[a,,g] <- 0 ## no movement at older ages
    } # end else
  } ## end ages
} ## end areas
X_ija[is.na(X_ija)] <- 0
X_ija[3,3,] <- 1 ## area 3 is self-seeding
## sanity check - all rows should sum to 1
for(i in 1:dim(X_ija)[3]){
  print(rowSums(X_ija[,,a]) == 1)
}


doNage <- function(nages = 21,
                   sel = dat$Sel.f.,
                   Fv = 0,
                   M = 0.15) {
  N_a <- Z_a <- matrix(NA, nrow = nages, ncol = 1)
  for(a in 1:nages){
    Z_a[a] <- M + sel[a]*Fv
    if(a == 1) N_a[a] <- 0.5
    if(a > 1  & a < max(nages)) N_a[a] <- N_a[a-1]*exp(-Z_a[a-1])
    if(a == max(nages)) N_a[a] <-  N_a[a-1]*exp(-Z_a[a-1])/(1- exp(-Z_a[a]))
  }
  cat(sum(N_a*dat$Wght.f.*dat$Fecundity),"\n")
  return(cbind(N_a,Z_a))
}

## Return area-specific B0,R0


doYPR <-
  function(nages = length(unique(dat$Age)),
           selex = list(dat$Sel.f., dat$Sel.m.),
           wts = list(dat$Wght.f., dat$Wght.m.),
           Fv = 0, M = 0.15 ) {
    ypr <- matrix(NA, nrow = nages, ncol = 2)
    for(i in 1:2){
      wt <- wts[[i]]
      nzmat <-  doNage(sel = selex[[i]], Fv = Fv)
      n_a <- nzmat[,1]
      z_a <- nzmat[,2]
      ypr[,i] <- wt*(( selex[[i]]*Fv)/z_a)*n_a*(1-exp(-z_a))
    }
    return(sum(ypr))
  }

getAB <- function(SRR = 1, h = 0.5, R0 = 1, area = 1,  gam = 1){
  fec = dat[,2,a]
  sel = dat[,3,a]#ist(,dat[,4,a])
  ## always calculate at F = 0
  sbpr0 <-
    sum(fec * doNage(Fv = 0, 
                     sel = sel)) #list(dat$Sel.f., dat$Sel.m.)[[1]])[, 1])
  if(SRR == 1){ 
    alpha <- sbpr0*((1-h)/(4*h))
    beta <- (5*h-1)/(4*h*R0) 
  } else  if(SRR == 2){
    alpha <- exp(log(5*h)/0.8)/sbpr0
    beta <- log(5*h)/(0.8*sbpr0*R0)
  } else   if(SRR == 3){
    alpha <- 1/sbpr0
    beta <- (5*h-1)/(1 - 0.2^gam) 
  }
  return(c("alpha" = alpha, "beta" = beta))
}

doSRR <- function(fec = dat$Fecundity, SRR = 1, h = 0.5, Fv = 0, gam = 1, R0 = 1, S0 = 0.6739975){
  ## get s-tilde
  sbpr <- fec*doNage(Fv = Fv, sel=  list(dat$Sel.f.,dat$Sel.m.)[[1]])[,1] 
  sumSBPR <- sum(sbpr)
  if(SRR == 1){ ## bevholt
    ab <- getAB(SRR = 1, h = h)
    R <- (sumSBPR - ab[1] )/(ab[2] * sumSBPR) ## Equation 9
  } else if(SRR == 2){ ## ricker
    ab <- getAB(SRR = 2, h = h)
    R <- log(ab[1]*sumSBPR)/(ab[2]*sumSBPR) ## Question 1A
  } else if(SRR == 3){ ## Pella
    ab <- getAB(SRR = 3, h = h)
    R <- (S0/sumSBPR) * (1 - (1 - ab[1] * sumSBPR)/(ab[2] * ab[1] * sumSBPR))^(1/gam) ## Question 1B
  }
  return(c('rec' = as.numeric(R),'spawnbio' = sumSBPR))
}

doYield <- function(ypr, R){
  yield <- ypr*R
  return(c("yield" = yield))
} 

## put it all together
masterFunc <-
  function(nages = 21,
           Fv = 0,
           selex = list(dat$Sel.f., dat$Sel.m.),
           wts = list(dat$Wght.f., dat$Wght.m.),
           SRR = 1,
           h = 0.5,
           gam = 1,
           R0 = 1,
           S0 = 0.6739975) {
    
    ypr <- doYPR(nages = nages, selex = selex, wts = wts, Fv = Fv)
    rec <-  doSRR( fec = dat$Fecundity,SRR = SRR, h = h, Fv = Fv,gam = gam, R0 = R0,S0 = S0)
    
    yield <- doYield(ypr,rec[1])
    
    df <- data.frame(  
      "SRR" = c('BevHolt','Ricker','Pella-T')[SRR],
      "Fmort" = Fv,
      "rec" = rec[1],
      "yield" = yield,
      "spawnbio" = rec[2]*rec[1])    ## eq 7
    
    return(df)
  }

q3b <- data.frame(SRR = NA, FMSY = NA, MSY = NA, BMSY= NA, Fcrash = NA)

## worked with J Sullivan on this function
dfx.dx <- function(Fv_test, h = 0.5){
  y1 <- masterFunc(SRR = s, h = h, Fv = Fv_test-0.001)$yield
  y2 <- masterFunc(SRR = s, h = h, Fv = Fv_test+0.001)$yield
  appx <- (y2-y1)/(0.002) #0.002 is total delta
  return(appx)
}

## find where F is minimized and S ~ 0, bisection
bisect <- function(Fmin = 0, Fmax = 1){
  for(b in 1:1000){
    Fv_testM <- (Fmin + Fmax)/2 ## update
    sbio_temp <- masterFunc(SRR = s, Fv = Fv_testM)$spawnbio
    if(round(sbio_temp,4) == 0 & (Fmax - Fmin) > 0.0002){ return(Fv_testM)
    } else if(round(sbio_temp,4) > 0) { Fmin <- Fv_testM 
    } else if(round(sbio_temp,4) < 0) { Fmax <- Fv_testM }
  }
  print('max iter')
}

for(s in 1:3){
  ## obtain FMSY
  FMSY <- as.numeric(uniroot(f = dfx.dx,  h = 0.5, interval = c(0.1,1))[1])
  ## plug for MSY
  MSY <- masterFunc(SRR = s, Fv = FMSY)$yield
  Fcrash <- bisect()
  q3b[s,] <- c( c('BevHolt','Ricker','Pella-T, gamma = 1')[s], round(FMSY,4), round(MSY,4), round(Fcrash,4))
}

