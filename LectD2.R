library(stats4)
# setwd("D:\\courses\\2020 AMARE-MED\\Lectures\\")



ProjectModel<- function(Narea,Prow,k,M,Catch,Yr,Phi,R0,Steep,Nyr,plot=F)
{
 Biomass <- matrix(0,nrow=Narea,ncol=Nyr+1)
 Recruit <- rep(0,Nyr+1)
  
  # Find the unexploited equilibrium biomass (easy for this case)
 for (Iarea in 1:Narea)  
  Biomass[Iarea,1] <- Phi[Iarea]*R0/(1-(1+Prow[Iarea])*exp(-M[Iarea])+Prow[Iarea]*exp(-2*M[Iarea]))
 B0 <- sum(Biomass[,1])
 Recruit[1] <- R0
 
 # Set up the second year (special)
 Iyr <- 1
 for (Iarea in 1:Narea)  
  { 
   Term1 <- (1+Prow[Iarea])*exp(-M[Iarea])*(Biomass[Iarea,Iyr]-Catch[Iyr,Iarea])
   Term2 <- Prow[Iarea]*exp(-2*M[Iarea])*(Biomass[Iarea,Iyr]-Catch[Iyr,Iarea])/Biomass[Iarea,Iyr]*Biomass[Iarea,1]
   Recr <- R0
   Biomass[Iarea,Iyr+1] <- Term1 - Term2 + Phi[Iarea]*Recr
  } 
 Recruit[2] <- 4*Steep*R0*sum(Biomass[,2]/B0)/( (1-Steep) + (5*Steep-1)*sum(Biomass[,2]/B0))
 
 # Project forward
 for (Iyr in 2:Nyr)
  {
   for (Iarea in 1:Narea)  
    { 
     Term1 <- (1+Prow[Iarea])*exp(-M[Iarea])*(Biomass[Iarea,Iyr]-Catch[Iyr,Iarea])
     Term2 <- Prow[Iarea]*exp(-2*M[Iarea])*(Biomass[Iarea,Iyr]-Catch[Iyr,Iarea])/Biomass[Iarea,Iyr]*(Biomass[Iarea,Iyr-1]-Catch[Iyr-1,Iarea])
     if (Iyr+1-k < 1)
      Recr <- R0
     else
      Recr <- Recruit[Iyr+1-k]
     Biomass[Iarea,Iyr+1] <- Term1-Term2 + Phi[Iarea]*Recr
     if (Biomass[Iarea,Iyr+1] < 0.1) Biomass[Iarea,Iyr+1] <- 0.1
   } 
   Recruit[Iyr+1] <- 4*Steep*R0*sum(Biomass[,Iyr+1]/B0)/( (1-Steep) + (5*Steep-1)*sum(Biomass[,Iyr+1]/B0))
  } 
 
 if (plot==T)
  {  
   par(mfrow=c(2,2))   
   ymax <- max(apply(Biomass,2,sum))*1.05
   plot(1:(Nyr+1),apply(Biomass,2,sum),lty=1,type="l",ylim=c(0,ymax),xlab="Year",ylab="Biomass")
   ymax <- max(Biomass)*1.05
   plot(1:(Nyr+1),Biomass[1,],lty=1,type="l",ylim=c(0,ymax),xlab="Year",ylab="Biomass")
   lines(1:(Nyr+1),Biomass[2,],lty=2)
   lines(1:(Nyr+1),Biomass[3,],lty=3)
 }
 
 return(Biomass)
  
}

# ===============================================================================================

NegLogLike <- function(R0,Steep,Phi1,Phi2,Nyr,Narea,Prow,k,M,Catch,TheData,SurveyCV,plot)
{
 Phi3 <- 1-Phi1-Phi2
 Phi <- c(Phi1,Phi2,Phi3)
 if (Phi3 < 0) return(10e20)
   
 Biomass <- ProjectModel(Narea,Prow,k,M,Catch,Yr,Phi,R0,Steep,Nyr,plot=plot)
 Index <- TheData[,2]  
 BioPred <- t(Biomass[,Index])

 # Simple likelihood 
 NegLog <- 0
 for (Idata in 1:length(Index))
  for (Iarea in 1:Narea) 
   {
    Residual <- log(TheData[Idata,Iarea+2])-log(BioPred[Idata,Iarea]) 
    NegLog <- NegLog + Residual*Residual/(2*SurveyCV*SurveyCV)
    if (plot==T) points(Index[Idata],TheData[Idata,Iarea+2],pch=Iarea+14)
   }  
 #cat(R0,Steep,Phi1,Phi2,Phi3,NegLog,"\n") 
 return(NegLog)
}  

# ===============================================================================================

#Some basic parameters
Narea <- 3
Catch <- read.csv("LectD2.csv")
Yrs <- Catch[,1]
Nyr <- length(Catch[,1])
Catch <- Catch[,1+c(1:Narea)]
Prow <- c(0.843,0.843,0.843)
M <- c(0.1,0.1,0.1)

# True population trajectory
TBiomass <- ProjectModel(Narea=Narea,Prow=Prow,k=5,M=M,Catch=Catch,Phi=c(0.2,0.5,0.3),R0=1700,Steep=0.7,Nyr=Nyr)

# geneate data 
set.seed(78210)
SurveyCV <- 0.2
Index <- seq(from=62,to=99,by=3)
TheData <- matrix(0,nrow=length(Index),ncol=Narea+2)
TheData[,1] <- Yrs[Index]
TheData[,2] <- Index
for (Nindex in 1:length(Index))
 for (Iarea in 1:Narea)
  TheData[Nindex,Iarea+2] <- TBiomass[Iarea,Index[Nindex]]*exp(rnorm(1,0,SurveyCV)-SurveyCV^2/2)

# test the likelihood (true values)
SS <- NegLogLike(R0=1700,Steep=0.7,Phi1=0.2,Phi2=0.5,Nyr,Narea,Prow,k=5,M,Catch,TheData,SurveyCV,plot=T)
print(SS)
# test the likelihood (initial values)
SS <- NegLogLike(R0=2500,Steep=0.6,Phi1=0.3,Phi2=0.3,Nyr,Narea,Prow,k=5,M,Catch,TheData,SurveyCV,plot=T)
print(SS)

# now fit the model and plot the results
fit <- mle(NegLogLike,start=list(R0=2000,Steep=0.6,Phi1=0.3,Phi2=0.3),fixed=list(
           Nyr=Nyr,Narea=Narea,Prow=Prow,k=5,M=M,Catch=Catch,TheData=TheData,SurveyCV=SurveyCV,plot=F))
print(summary(fit))
est <- as.numeric(coef(fit)[1:4])
SS <- NegLogLike(R0=est[1],Steep=est[2],Phi1=est[3],Phi2=est[4],Nyr,Narea,Prow,k=5,M,Catch,TheData,SurveyCV,plot=T)

  







  