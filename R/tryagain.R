popN <- function(par, dat, ret = c('opt','vals','sbpf0')[1]){
  
  ## load settings
  
  F1 <- exp(par[1])
  F2 <- exp(par[2])
  Nages <- 20
  Ages <- 0:(Nages-1)
  narea <- 2
  # LAA <- 50*(1-exp(-0.15*(Ages)))
  # WAA <- 0.63*LAA^1.81
  Sel <- Fec <- matrix(0,nrow=2,ncol=Nages)
  Sel[1,] <- dat$dat[Ages+1,"fishery_selectivity",1] #logistic(a = Ages, a50 = slx_a50, a95 = slx_a95)
  Sel[2,] <- dat$dat[Ages+1,"fishery_selectivity",2] #logistic(a = Ages, a50 = 9, a95 = 13)
  Fec[1,] <- Sel[1,]*dat$dat[Ages+1,'weight',1]
  Fec[2,] <- Sel[2,]*dat$dat[Ages+1,'weight',2]
  
  M = dat$M
  # M <- 0.2
  Steep <- dat$h
  
  ## run area-specific NAA
  Z<- matrix(0,nrow=2,ncol=Nages)
  for (Iage in 1:Nages) Z[1,Iage] <- M+Sel[1,Iage]*F1
  for (Iage in 1:Nages) Z[2,Iage] <- M+Sel[2,Iage]*F2
  
  NPR_SURV <- NPR <- BPR <- SBPR <- YPR <- array(NA, dim = c(narea,Nages,narea)) ## now 100 ages of record
  # NPR_SURV[,1,1] <- NPR[,1,1] <- c(dat$input_prop,1-dat$input_prop);  NPR_SURV[,1,2] <-  NPR[,1,2] <- c(dat$input_prop,1-dat$input_prop) ## single recruit to each area
  ## assign recruits to each area
  NPR[,1:2,1] <- c(dat$input_prop,0)
  NPR[,1:2,2] <- c(0,1-dat$input_prop)
  ## survive and move at once
  for(slice in 1:narea){
    for(age in 2:Nages){
      for(area in 1:narea){
        pLeave = NCome = 0
        for(jarea in 1:narea){
          if(area != jarea){
            pLeave = pLeave + (1-dat$dat[age,"proportion_stay",area]) ##  leaving for elsewhere
            NCome = NCome +(1-dat$dat[age,"proportion_stay",jarea])*
              NPR[jarea,age-1,slice]*exp(-Z[jarea, age-1]) ## mortality in other area
          } # end i != j
        }  # end subareas j
        # cat(pLeave,"\n")
        # cat(NCome,"\n")
        # cat( NPR[area,age,slice],"\n")
        NPR[area,age,slice] <- (1-pLeave)*NPR[area,age-1,slice]*exp(-Z[area, age-1]) + NCome
        cat( NPR[area,age,slice],"\n")
        
      }
    }
  }   
  for(slice in 1:narea){
    for(area in 1:narea){
        NPR[area,Nages,slice] <-    NPR[area,Nages,slice]/(1-exp(-Z[area,Nages]))
    }}
        # Ztemp <- -log(dat[age,'mortality',slice])+dat[age,"fishery_selectivity",area]*FF[area]
        
        # NPR[1,,1] <- N[1,]; NPR[2,,2] <- N[2,]
        # } ## end ages 2:Nages
        ## Calc Yield for each area-age - use baranov catch equation!
        ## bpr IS Wa x Nax
        ## make sure ztemp is not in exp space (so log mortality, which is exp(-M), really should be survivorship)
        
        # YPR[area,age,slice] <- (dat[age,"fishery_selectivity",area]*
        #                           FF[area]*
        #                           NPR[area,age,slice]* ## use total numbers available in area
        #                           # NPR_SURV[area,age,slice]*
        #                           dat[age,"weight",area]*
        #                           (1-exp(-Ztemp)))/(Ztemp)
        # # for(age in 1:Nages){
        # BPR[area,age,slice] <- NPR[area,age,slice]*dat[age,"weight",area]
        # SBPR[area,age,slice] <- BPR[area,age,slice]*dat[age,"maturity",area]
        
        # cat( YPR[area,age,slice],"\n")
      } ## end ages 1:Nages
    } ## end areas
  } ## end slices (array)
  
  
SSB <- Cat <- Recr <- c(0,0)
for(area in 1:2){
  for (Iage in 1:Nages) Cat[area] <- Cat[area] + WAA[Iage]*Sel[area,Iage]*c(F1,F2)[area]*NPR[area,Iage]/Z[area,Iage]*(1.0-exp(-Z[area,Iage]))
  for (Iage in 1:Nages) SSB[area] <- SSB[area] + Fec[Iage]*N[area,Iage]
  
  # Top <- 4*Steep[area]*SSB[area]/SBPF0[area] - (1-Steep[area])
  # Bot <- (5*Steep[area]-1)*SSB[area]/SBPF0[area]
  # Recr[area] <- Top/Bot
  
}
  
  
  
  # N<- matrix(0,nrow=2,ncol=Nages)
  # N[1,1] <-  Prop
  # for (Iage in 2:Nages) N[1,Iage] <- N[1,Iage-1]*exp(-Z[1,Iage-1])
  # N[1,Nages] <- N[1,Nages]/(1-exp(-Z[1,Nages]))
  # N[2,1] <- 1.0-Prop
  # for (Iage in 2:Nages) N[2,Iage] <- N[2,Iage-1]*exp(-Z[2,Iage-1])
  # N[2,Nages] <- N[2,Nages]/(1-exp(-Z[2,Nages]))
  
  
  
  ## switch between recruit assumption - optimise if needed
  req_global <- getEqR(assumption = 'GLOBAL', SBPF0, SBPR, SSB, Prop=dat$input_prop) 
  req_local <- getEqR(assumption = 'LOCAL', SBPF0, SBPR, SSB, Prop=dat$input_prop) 
  
  
  ## switch for what to return
  if(ret == 'sbpf0'){
    return(SSB) ## stop here, no need for recruit (prop already inside)
  } else if(ret == 'vals'){
    Cat <- Cat*Recr ## yeq, sys
    SSB <- SSB*Recr ## ssbeq, sys
    return(list('Prop','F1','F2','Cat1' = Cat[1], 'Cat2' = Cat[2],
                'Rec1' = Recr[1], 'Rec2' = Recr[2],
                'SSB1' = SSB[1], 'SSB2' = SSB[2],
                'SBPF01' = SBPF0[1],'SBPF02' = SBPF0[2],
                'tyield' = sum(Cat),
                'trec' = sum(Recr),
                'tSSB' = sum(SSB),
                'tSBPF0' = sum(SBPF0),
                'tDepl' = sum(SSB)/sum(SBPF0)))
  }else{
    Cat <- Cat*Recr ## yeq, sys
    SSB <- SSB*Recr ## ssbeq, sys
    if (Detail==T) cat(round(c(Prop,F1,F2,sum(Cat),sum(Recr), sum(SSB),sum(SBPF0),sum(SSB)/sum(SBPF0)),1),"\n")
    obj <- -sum(Cat)
    return(obj)
  }
}
getExpR <- function(SB_F, SB_0, meth ){
  ## not sure if this should pass sum for global vs local
  Rexp <- bh(h, prop = Rprop_input, r0 = R0_global, b0 = SB_0, bcurr = SB_F, method = meth)
  return(Rexp)
}

getSB <- function(passR, passRprop, SBPR_F){
  # SBPR_F_A1 <- sum(SBPR_F[1,]) #sum(SBPR_F[1,,1],SBPR_F[1,,2])
  # SBPR_F_A2 <- sum(SBPR_F[2,]) #sum(SBPR_F[2,,1],SBPR_F[2,,2])
  SB_A1 <- SBPR_F[1]*passR*passRprop
  SB_A2 <- SBPR_F[2]*passR*(1-passRprop)
  return(list('SB_A1'=SB_A1,"SB_A2"=SB_A2))
}
optimFunc <- function(par,SBPR_0,SBPR_F){
  # cat(par,"\n")
  passR <- par[1]; passRprop <- par[2]
  print(passR)
  print(passRprop)
  sb_0 <- getSB(1, Prop, SBPR_0)
  print(sb_0)
  sb_F <- getSB(passR ,passRprop, SBPR_F)
  Rexp <- getExpR(SB_F=sb_F, SB_0=sb_0, meth = 1) ## bh vals given pars, als use proposed method
  obsR <- passR*c(passRprop,1-passRprop) ## raw rec given rglobal and prop
  obj <- sum((obsR - Rexp)^2)
  # cat(c(passR,  sb_0[1], sb_0[2], 
  #       sb_F[1], sb_F[2], Rexp,  obsR,  obj),"\n")
  # obj <- ifelse(abs(obj) == Inf,'')
  return(obj)
}

getEqR <- function(assumption = 'GLOBAL', SBPF0, SBPR, SSB, Prop){
  if(assumption == 'GLOBAL'){
    # use summed qtties and return straight up req
    Top <- 4*Steep*SSB/SBPF0 - (1-Steep)
    Bot <- (5*Steep-1)*SSB/SBPF0
    Recr <- Top/Bot
  } else{
    ## optimize
 
    opt_temp <- optim(par = c(1,Prop),
                      SBPR_F = SBPR,
                      SBPR_0 = SBPF0,
                      lower = c(1E-4,1E-4),
                      upper = c(NA,0.9999),
                      method = "L-BFGS-B",
                      fn = optimFunc, hessian = FALSE,
                      control = list(
                        maxit = 1000,
                        ndeps = rep(1e-4,2)))
    
    Top <- 4*Steep[area]*SSB[area]/SBPF0[area] - (1-Steep[area])
    Bot <- (5*Steep[area]-1)*SSB[area]/SBPF0[area]
    Recr[area] <- Top/Bot
  }
  
}