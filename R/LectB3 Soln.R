require(here)
Nsize <- 3; Narea <- 2;Ndim <- Nsize*Narea

# Read in the marices 
# DataFile <- here("_background","LectB3_new.txt")
source(here("R","EquilSpawnRecrFxn.R"))
DataFile <- here("_background","LectB3.txt")

S <- matrix(scan(DataFile,skip=1,n=Ndim*Ndim,quiet=T),nrow=Ndim,ncol=Ndim,byrow=T)
A <- matrix(scan(DataFile,skip=Ndim+2,n=Ndim*Ndim,quiet=T),nrow=Ndim,ncol=Ndim,byrow=T) 
# X <- matrix(scan(DataFile,skip=2*Ndim+3,n=Ndim*Ndim,quiet=T),nrow=Ndim,ncol=Ndim,byrow=T)
X <- matrix(0, nrow=Ndim,ncol=Ndim)
## fill lower diag with 1s
for(i in 1:Ndim){
  if(i %% Nsize != 0) {X[i+1,i] <- 1}#;  cat(i,"\t",i+1,"\n")}
}
for(i in seq(Nsize, Ndim,Nsize) ) X[i,i] <- 1 ## accumulate at plus group ag
# Identity matrix - will be used often
I <- matrix(0,ncol=Ndim,nrow=Ndim); diag(I) = 1
R_init <-  rep(1,Narea) #matrix(1,0,0,1,0,0)

FindEqn <- function(FF=0, 
                    R0_i = R_init,
                    R_use = R_use,
                    finding_sb0 = FALSE)
 {
  Outs <- NULL
  
  ## create R based on R_init
  R <- rep(0,Ndim)
  R[1] <- R_use[1] ; R[1+Nsize]  <- R_use[2]
  
  H <- matrix(0,ncol=Ndim,nrow=Ndim); diag(H) = 1
  # Specify the H matrix, which is literally the proportion remaining post-fishing
  # H[Nsize,Nsize] <- 1.0 - 0.2/0.8*FF ## 1- selex * f slide 7
  H[Nsize,Nsize] <- 1.0 - FF ## 1- selex * f slide 7
  H[Nsize+Nsize,Nsize+Nsize] <- 1.0 - FF ## next stage bin is just 1-FF
  # Multiply the matrices
  Mat1 <- S ## natural survival e^-M
  Mat2 <- (X %*% (A %*% (S %*% (H %*% S)))) ## for use in slide 12
  # Matrix inversion multiplied by a recruitment vector with a 1 in the first row
  ## this assumes recruitment is 1 in all areas.
  ## if unfished used R0
  if(finding_sb0 == TRUE){
    Neqn <- solve(I-Mat2) %*% R ## slide 12
    spawntemp_byage <- Neqn * maturity_vector ## gives you expected # spawners
    spawntemp <- c(sum(spawntemp_byage[1:3]),sum(spawntemp_byage[4:6]))
    Catch <- sum((I-H) %*% (Mat1 %*% Neqn)) ## Mat1 * Neqn is who dies; note that H is harvest REMAINDER so 1-h is capture
    
  }
  
  ## calculate expected recruits given SRR; SPR is the spawners div by
  ## the original r_use input
  # R_equil <-  Equil_Spawn_Recr_Fxn(steepness = 0.5,
  #                                  SSB_virgin = Spawn0[1],
  #                                  Recr_virgin = R0_i[1],
  #                                  SPR_temp = spawntemp[1]/(R_use[1]+0.005*R0_i[1]))$R_equil

  
  # Compute the yield
  
  
 else if(finding_sb0 == FALSE){
    
    Neqn <- solve(I-Mat2) %*% R ## slide 12
    spawntemp_byage <- Neqn * maturity_vector ## gives you expected # spawners
    spawntemp <- c(sum(spawntemp_byage[1:3]),sum(spawntemp_byage[4:6]))
    Catch <- sum((I-H) %*% (Mat1 %*% Neqn)) ## Mat1 * Neqn is who dies; note that H is harvest REMAINDER so 1-h is capture
    
    #print(Neqn) ## equilibrium numbers at stage in area
    ## calc equil recruitment (unclear if by area or not...)
    R_equil_temp_a1 <- Equil_Spawn_Recr_Fxn(steepness = 0.5, 
                                            SSB_virgin = Spawn0[1], 
                                            Recr_virgin = R0_i[1],
                                            SPR_temp = spawntemp[1]/(R_use[1]+0.005*R0_i[1]))$R_equil
    
    
    R_equil_temp_a2 <- Equil_Spawn_Recr_Fxn(steepness = 0.5, 
                                            SSB_virgin = Spawn0[2], 
                                            Recr_virgin = R0_i[2],
                                            SPR_temp = spawntemp[2]/(R_use[2]+0.005*R0_i[2]))$R_equil
    
    R_equil_temp_global <- Equil_Spawn_Recr_Fxn(steepness = 0.5, 
                                                SSB_virgin = sum(Spawn0), 
                                                Recr_virgin = sum(R0_i),
                                                SPR_temp = sum(spawntemp)/ sum(R_use))$R_equil
    # R_new <-  0.88*spawntemp #rep(1.4,2) ### for now
    # R_new <- c(rep(R_equil_temp_a1,3),rep(R_equil_temp_a2,3))
    R_new <- c(R_equil_temp_a1,R_equil_temp_a2)
    # R_new <- rep(R_equil_temp_global/2,2)
    Outs$R_new = R_new
    Outs$R_global = R_equil_temp_global*c(0.25,0.75)
    # cat(R_new,"\n")
  }
  # Code to chck the equilibrium is correct
  #Test <- S %*% Neqn
  #Test <- H %*% Test
  #Test <- S %*% Test
  #Test <- A %*% Test
  #Test <- X %*% Test
  #Test[1] <- Test[1] + 1
  #print(Test-Neqn)

  
  ## this is equivalent
  # YPR1 <- ((I-H) %*% (Mat1 %*% Neqn))[3]/R_use[1]
  # YPR2 <- ((I-H) %*% (Mat1 %*% Neqn))[6]/R_use[2]
  # Yield <- sum(YPR1*R_equil_temp_a1,YPR2*R_equil_temp_a2)
  
  # Return
  Outs$FF <- FF
  Outs$Neqn <- Neqn
  Outs$Catch <- Catch
  # Outs$Yield <- Yield
  
  return(Outs)
}



# =================================================================================
maturity_vector = c(0,0.5,1,0,0.5,1)


# Spawn0 <- sum(FindEqn(FF = 0, R0_i = R_init)$Neqn[c(3,6)]) ## unfished spawning biomass
Spawn0_byage <- FindEqn(FF = 0, R0_i = R_init, R_use = R_init, finding_sb0 = TRUE)$Neqn*maturity_vector 
Spawn0 <- c(sum(Spawn0_byage[1:3]),sum(Spawn0_byage[4:6]))## unfished spawning biomass by area

# Part 1 (yield vs F)
FFs <- seq(from=0,to=1,by=0.01)
Yields <- SSYields <-  OGYields <- rep(0,length(FFs))
Spawn <- SSSpawn <- OGSpawn <- rep(0,length(FFs))
# Rused <- matrix(NA, length(FFs), Narea)
Rused <- array(NA, dim = c(length(FFs), Narea, 101))

for (II in 1:length(FFs)){
  OGPrj <- FindEqn(FFs[II], R0_i = R_init, R_use = R_init) ## equilbrium numbers given F
  OGYields[II] <- OGPrj$Catch ## yield at F
  OGSpawn[II] <- sum(OGPrj$Neqn* maturity_vector) ## expected N spawners at F
  for(k in 1:2){
    if(k == 1){
      R_use_prop = R_use_SS = R_init ## start all off at the same values
    } else if(k > 1){
      R_use_prop <- ModelPrj$R_new+R_init #ModelPrj$R_new ## use last iteration
      # cat(k,R_use_prop,"\n")
      R_use_SS <- SSPrj$R_global #ModelPrj$R_new ## use last iteration
    }
    ModelPrj <- FindEqn(FFs[II], R0_i = R_init, R_use = R_use_prop) ## equilbrium numbers given F
    Yields[II] <- ModelPrj$Catch ## yield at F
    Spawn[II] <- sum(ModelPrj$Neqn * maturity_vector) ## expected N spawners at F
    
    # Rused[II,,k] <- R_use
    # Yields2[II] <- ModelPrj$Yield
    
    SSPrj <- FindEqn(FFs[II], R0_i = R_init, R_use = R_use_SS) ## equilbrium numbers given F
    SSYields[II] <- SSPrj$Catch ## yield at F
    SSSpawn[II] <- sum(ModelPrj$Neqn * maturity_vector)  ## expected N spawners at F
    
    cat("FF ", FFs[II]," k ", k, " Ruse prop ",  R_use_prop, " Ruse ss ",R_use_SS, "\n")
    # cat(FFs[II],k, ModelPrj$Neqn,"\t",   SSPrj$Neqn ,"\n")
    # cat("SPAWN" , FFs[II],k,     Spawn[II],      SSSpawn[II] , "\n")
    # cat("SBPR" ,FFs[II],k,     Spawn[II]/R_use_prop, SSSpawn[II]/R_use_SS, "\n")
    # cat(FFs[II],k, Spawn[II]/R_use_prop,   SSSpawn[II]/R_use_SS, "\n")
    # cat(FFs[II],k, SSPrj$R_global, ModelPrj$R_new, "\n")
  } ## end iters
} ## end Fs
# par(mfrow=c(2,1),oma=c(1,1,1,1))
plot(FFs,Yields,xlab="Fishing effort",ylab="Yield",type="l",lty=1,
     col  = 'red', ylim = c(0,2))
lines(FFs,OGYields,xlab="Fishing effort",ylab="Yield",type="l",lty=1)
lines(FFs,SSYields,xlab="Fishing effort",ylab="Yield",type="l",lty=1, col = 'blue')
legend('topright', legend = c('No SRR','SRR by Area (proposed method)', 'Global SRR'),
       col = c('black','red', 'blue'), lty = 1)

plot(FFs,Spawn,xlab="Fishing effort",ylab="Spawners",type="l",lty=1, col  = 'red', ylim = c(0,10))
lines(FFs,OGSpawn,xlab="Fishing effort",ylab="Spawners",type="l",lty=1)
lines(FFs,SSSpawn,xlab="Fishing effort",ylab="Spawners",type="l",lty=1, col = 'blue')
legend('topright', legend = c('No SRR','SRR by Area (proposed method)', 'Global SRR'),
       col = c('black','red', 'blue'), lty = 1)

plot(FFs,Rused[,1,2],xlab="Fishing effort",ylab="R after 2 Iter",
     type="l",lty=1, col  ='black', ylim = c(0,2))
lines(FFs,Rused[,2,2],col = 'blue')
# lines(FFs,Rused[,1,50],col = 'black')
# leglines(FFs,Rused[,2,50],col = 'blue')

end('topright', legend = c('No SRR','SRR by Area'), col = c('black','red'), lty = 1)
# =================================================================================
##  neq[3,6] are the  mature stages in each area
## this equation finds the SB at F and subtracts SB35%, thus enabling us to 
## minimze this equation and find F which returns SB35 on a global basis
func <- function(FF,Spawn0)
 {
  func <-  sum(FindEqn(FF)$Neqn*maturity_vector) - 0.35*sum(Spawn0)
  return(func)
 }  

funcII <- function(FF, Spawn0, niter = 2){
  
  for(k in 1:niter){
    if(k == 1){
      R_use = R_init
    } else if(k > 1){
      R_use <- ModelPrj$R_new ## use last iteration
    }
    ModelPrj <- FindEqn(FF, R = R_use) ## equilbrium numbers given F
  } ## end iters, ModelPrj is now final R_use
  func <-  sum(ModelPrj$Neqn[c(3,6)]) - 0.35*Spawn0
  return(func)
  
}


# Part 2 find F such that mature numbers are 35% of that in equilirbium

soln <- uniroot(func,lower=0,upper=1,Spawn0=Spawn0)$root ## optimization
soln2 <- uniroot(funcII,lower=0,upper=1,Spawn0=Spawn0)$root ## optimization

cat("Optimal effort = ",soln,"; check =",sum(FindEqn(soln)$Neqn[c(3,6)])/Spawn0,"\n")
cat("Optimal effort = ",soln2,"; check =",sum(FindEqn(soln2)$Neqn[c(3,6)])/Spawn0,"\n")






