Prop <- 0.7

logistic <- function(a,a50,a95){
  val <- 1/(1+exp(-log(19)*((a-a50)/(a95-a50))))
  return(val)
}

PopN_local <- function(par, getSB0 = FALSE){
  F1 <- exp(par[1])
  F2 <- exp(par[2])
  Nages <- 20
  Ages <- 0:(Nages-1)
  # LAA <- 100*(1.0-exp(-0.2*(Ages)))
  # WAA <- 0.00001*LAA^3
  LAA <- 50*(1-exp(-0.15*(Ages)))
  WAA <- 0.63*LAA^1.81
  
  # Sel <- rep(0,Nages)
  # Sel[6:Nages] <- 1
  Sel <- logistic(a = Ages, a50 = 9, a95 = 13)
  
  Fec <- Sel*WAA
  M <- 0.2
  Steep <- c(0.7,0.7)
  
  Z<- matrix(0,nrow=2,ncol=Nages)
  for (Iage in 1:Nages) Z[1,Iage] <- M+Sel[Iage]*F1
  for (Iage in 1:Nages) Z[2,Iage] <- M+Sel[Iage]*F2
  
  ## NEED MOVEMENT HERE
  N<- matrix(0,nrow=2,ncol=Nages)
  N[1,1] <-  Prop
  for (Iage in 2:Nages) N[1,Iage] <- N[1,Iage-1]*exp(-Z[1,Iage-1])
  N[1,Nages] <- N[1,Nages]/(1-exp(-Z[1,Nages]))
  N[2,1] <- 1.0-Prop
  for (Iage in 2:Nages) N[2,Iage] <- N[2,Iage-1]*exp(-Z[2,Iage-1])
  N[2,Nages] <- N[2,Nages]/(1-exp(-Z[2,Nages]))
  #if (Detail==T) print(N)
  #if (Detail==T) print(Z)

  SSB <- Cat <- Recr <- c(0,0)
  for(i in 1:2){
    for (Iage in 1:Nages) Cat[i] <- Cat[i] + WAA[Iage]*Sel[Iage]*c(F1,F2)[i]*N[i,Iage]/Z[i,Iage]*(1.0-exp(-Z[i,Iage]))
    for (Iage in 1:Nages) SSB[i] <- SSB[i] + Fec[Iage]*N[i,Iage]

    Top <- 4*Steep[i]*SSB[i]/SBPF0[i] - (1-Steep[i])
    Bot <- (5*Steep[i]-1)*SSB[i]/SBPF0[i]
    Recr[i] <- Top/Bot

  }

  if(getSB0 == T){
    return(SSB)
  } else{
    Cat <- Cat*Recr ## yeq, sys
    SSB <- SSB*Recr ## ssbeq, sys
    if (Detail==T) cat(round(c(Prop,F1,F2,sum(Cat),sum(Recr), sum(SSB),sum(SBPF0),sum(SSB)/sum(SBPF0)),1),"\n")
    
    obj <- -sum(Cat)
    return(obj)
  }

}

PopN_global <-function(par){
 F1 <- exp(par[1])
 F2 <- exp(par[2])
 Nages <- 20
 Ages <- 0:(Nages-1)
 # LAA <- 100*(1.0-exp(-0.2*(Ages)))
 # WAA <- 0.00001*LAA^3
 LAA <- 50*(1-exp(-0.15*(Ages)))
 WAA <- 0.63*LAA^1.81
 
 # Sel <- rep(0,Nages)
 # Sel[6:Nages] <- 1
 Sel <- logistic(a = Ages, a50 = 9, a95 = 13)
 
 Fec <- Sel*WAA
 M <- 0.2
 Steep <- 0.7
 
 Z<- matrix(0,nrow=2,ncol=Nages)
 for (Iage in 1:Nages) Z[1,Iage] <- M+Sel[Iage]*F1
 for (Iage in 1:Nages) Z[2,Iage] <- M+Sel[Iage]*F2
 
 ## NEED MOVEMENT HERE
 N<- matrix(0,nrow=2,ncol=Nages)
 N[1,1] <-  Prop
 for (Iage in 2:Nages) N[1,Iage] <- N[1,Iage-1]*exp(-Z[1,Iage-1])
 N[1,Nages] <- N[1,Nages]/(1-exp(-Z[1,Nages]))
 N[2,1] <- 1.0-Prop
 for (Iage in 2:Nages) N[2,Iage] <- N[2,Iage-1]*exp(-Z[2,Iage-1])
 N[2,Nages] <- N[2,Nages]/(1-exp(-Z[2,Nages]))
 #if (Detail==T) print(N)
 #if (Detail==T) print(Z)
 
 Cat <- 0
 for (Iage in 1:Nages) Cat <- Cat + WAA[Iage]*Sel[Iage]*F1*N[1,Iage]/Z[1,Iage]*(1.0-exp(-Z[1,Iage]))
 for (Iage in 1:Nages) Cat <- Cat + WAA[Iage]*Sel[Iage]*F2*N[2,Iage]/Z[2,Iage]*(1.0-exp(-Z[2,Iage]))
 
 SSB <- 0
 for (Iage in 1:Nages) SSB <- SSB + Fec[Iage]*N[1,Iage]
 for (Iage in 1:Nages) SSB <- SSB + Fec[Iage]*N[2,Iage]
 
 
 Top <- 4*Steep*SSB/SBPF0 - (1-Steep)
 Bot <- (5*Steep-1)*SSB/SBPF0
 Recr <- Top/Bot
 
 Cat <- Cat*Recr ## yeq, sys
 SSB <- SSB*Recr ## ssbeq, sys
 if (Detail==T) cat(round(c(Prop,F1,F2,Cat,Recr,SSB,SBPF0,SSB/SBPF0),1),"\n")
 
 obj <- -Cat
 return(obj)
}


Detail <- T

## PROP F1 F2 CAT RECR DEPL
SBPF0 <- 480.904 # 10.92109 GLOBAL
PopN_global(c(-10000,-100000)) ## F=0
for (II in 1:9) {
  Prop <- II*0.1
  Detail <- F
  ss <- optim(par=c(log(0.1),log(0.1)),fn=PopN_global)
  ss <- optim(par=ss$par,fn=PopN_global)
  Detail <- T
  PopN_global(ss$par)
}


for (II in 1:9) {
  Prop <- II*0.1
  ## UPDATE SB0 based on prop
  SBPF0 <- PopN_local(c(-10000,-10000), getSB0=T) ## F=0
  # cat(Prop,"\t",SBPF0,"\t", sum(SBPF0),"\n")
  Detail <- F
  ss <- optim(par=c(log(0.1),log(0.1)),
              fn=PopN_local, 
              lower = c(-10000,-10000),
              upper = c(log(10000),log(10000)),
              method = 'L-BFGS-B')
  ss <- optim(par=ss$par,
              fn=PopN_local,
              lower = c(-10000,-10000),
              upper = c(log(10000),log(10000)),
              method = 'L-BFGS-B')
  Detail <- T
  PopN_local(ss$par)
}

