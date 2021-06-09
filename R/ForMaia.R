Prop <- 0.7

PopN<-function(par)
{
 F1 <- exp(par[1])
 F2 <- exp(par[2])
 Nages <- 20
 Ages <- 0:(Nages-1)
 LAA <- 100*(1.0-exp(-0.2*(Ages)))
 WAA <- 0.00001*LAA^3
 Sel <- rep(0,Nages)
 Sel[6:Nages] <- 1
 Fec <- Sel*WAA
 M <-0.2
 Steep <- 0.7
 
 Z<- matrix(0,nrow=2,ncol=Nages)
 for (Iage in 1:Nages) Z[1,Iage] <- M+Sel[Iage]*F1
 for (Iage in 1:Nages) Z[2,Iage] <- M+Sel[Iage]*F2
 
 N<- matrix(0,nrow=2,ncol=Nages)
 N[1,1] <- 1 # Prop
 for (Iage in 2:Nages) N[1,Iage] <- N[1,Iage-1]*exp(-Z[1,Iage-1])
 N[1,Nages] <- N[1,Nages]/(1-exp(-Z[1,Nages]))
 N[2,1] <- 1 # 1.0-Prop
 for (Iage in 2:Nages) N[2,Iage] <- N[2,Iage-1]*exp(-Z[2,Iage-1])
 N[2,Nages] <- N[2,Nages]/(1-exp(-Z[2,Nages]))
 #if (Detail==T) print(N)
 #if (Detail==T) print(Z)
 
 Cat <- 0
 for (Iage in 1:Nages) Cat <- Cat + Prop*WAA[Iage]*Sel[Iage]*F1*N[1,Iage]/Z[1,Iage]*(1.0-exp(-Z[1,Iage]))
 for (Iage in 1:Nages) Cat <- Cat + (1-Prop)*WAA[Iage]*Sel[Iage]*F2*N[2,Iage]/Z[2,Iage]*(1.0-exp(-Z[2,Iage]))

 SSB <- 0
 for (Iage in 1:Nages) SSB <- SSB + Prop*Fec[Iage]*N[1,Iage]
 for (Iage in 1:Nages) SSB <- SSB + (1-Prop)*Fec[Iage]*N[2,Iage]
 
 
 ## local vs global here?
 Top <- 4*Steep*SSB/SBPF0 - (1-Steep)
 Bot <- (5*Steep-1)*SSB/SBPF0
 Recr <- Top/Bot

 Cat <- Cat*Recr ## yeq, sys
 SSB <- SSB*Recr ## ssbeq, sys
 if (Detail==T) cat(Prop,F1,F2,Cat,Recr,SSB/SBPF0,"\n")
 
 obj <- -Cat
 return(obj)

}

SBPF0 <- 10.92109
Detail <- T
PopN(c(-10000,-100000))
for (II in 1:9)
 {
  Prop <- II*0.1
  Detail <- F
  ss <- optim(par=c(log(0.1),log(0.1)),fn=PopN)
  ss <- optim(par=ss$par,fn=PopN)
  Detail <- T
  PopN(ss$par)
}
