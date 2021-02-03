## functions used


logistic <- function(a,a50,a95){
  val <- 1/(1+exp(-log(19)*((a-a50)/(a95-a50))))
  return(val)
}

bh <- function(h, prop, r0, b0, bcurr){
  num <- prop*4*h*r0*bcurr/b0
  denom1 <- bcurr*(5*h-1)
  denom2 <- bcurr/b0*(1-h)
  rec = num/(denom1+denom2)
  return(rec)
}
getSB <- function(passR, passRprop, SBPR_F){
}
getYield <- function(passR, passRprop, YPR_F){
}
getExpR <- function(passR, passRprop, SBPR_F, SBPR_0){
  ## calculate SB_0 given passed pars
 SB_0 <- getSB(passR, passRprop, SBPR_0)
 ## calculate SB_F given passed pars and SBPR_F
 SB_F <- getSB(passR, passRprop, SBPR_F) 
 ## now use these in the bev-holt, be sure to use the global inputs
 Rexp <- bh(h = steep, prop = Rprop_input, r0 = R0_input, b0 = SB_0, bcuff = SB_F)
}

optimFunc <- function(passR, passRprop){
  ## do getExpR to get ExpR
  obsR <- Rbar*c(Rprop,1-Rprop)
  obj <- sum((stuff$obsR - stuff$expR)^2)
  return(obj)
}
## set up readable data frame with movement, selex, wtage, maturity inputs for each area
## each par will be of length 2, representing the two areas
## pStay indicates the residency rate for ages 10+; linear to this between age 1-11 
vals <- c('age','proportion_stay','weight','maturity','fishery_selectivity') ## things to enter into data frame

makeDat <- function(nage = 20, narea =2, wa,
                    fec_a50, fec_a95,
                    slx_a50,slx_a95,
                    pStay = c(0.9,0.6)){
  dat <- array(NA, dim = c(nage+1,length(vals),narea),dimnames = list(c(0:nage), c(vals), c(1:narea)))
  
  for(area in 1:narea){
    for(age in 0:(nage+1)){
      
      dat[age,"age",area] <- age
      
      dat[age,"proportion_stay",area] <- 1 ## recruits stay put
      if(age <10 & age > 1) dat[age,"proportion_stay",area] <- min(c(pStay[area], age*(pStay[area])/length(1:12)+0.25))
      if(age >= 10) dat[age,"proportion_stay",area] <- pStay[area]
      
      dat[age,"weight",area] <- wa[area] * age 
      
      dat[age,'maturity',area] <- logistic(a = age, a50 = fec_a50[area], a95 = fec_a95[area])
      
      dat[age,'fishery_selectivity',area] <- logistic(a = age, a50 = slx_a50[area], a95 = slx_a95[area])
      
    } ## end age
  } ## end area
  
  png(here('figs',paste0(Sys.Date(),'-inputDat.png')),
      width = 8, height = 8, units = 'in', res = 400)
  par(mfrow = c(2,2))
  for(v in 2:length(vals)){
    age <- 0:20
    plot(dat[,v,1] ~ age, type = 'p', pch = 19, xlab='age', ylab = vals[v],
         col = alpha('black',0.5),
         ylim = c(0,ifelse(vals[v]!='weight',1,100)))
    text(x = 19, y = ifelse(vals[v]!='weight',0.95,95), label = LETTERS[v-1], cex = 1.5)
    if(v == 2){
      legend('bottomright', legend = c('Area 1','Area 2'), cex = 1.2,
             pch = 19, col = alpha(c('black','blue'),0.5))
    }
    points(dat[,v,2] ~ age, type = 'p', pch = 19,col = alpha('blue',0.5),)
  }
  dev.off()
  cat("saved 2x2 input data figure in figs \n")
  return(dat)
}



## generate arrays with NAA, BPR, SBPR and YPR with natal record
## a given array slice (third dim) lets us track the fate of individuals spawned in that area.
## thus we must add rows across slices if we want totals in-area.
doPR <- function(dat,narea = 2, nage =20){
  NPR <- BPR <- SBPR <- YPR <- array(NA, dim = c(narea,nage,narea))
  
  
}



