lb3func <- function(FF){
  
  
  Nsize <- 3; Narea <- 2;Ndim <- Nsize*Narea
  
  # Read in the marices 
  DataFile <- here("_background","LectB3.txt")
  S <- matrix(scan(DataFile,skip=1,n=Ndim*Ndim,quiet=T),nrow=Ndim,ncol=Ndim,byrow=T)
  A <- matrix(scan(DataFile,skip=Ndim+2,n=Ndim*Ndim,quiet=T),nrow=Ndim,ncol=Ndim,byrow=T) ## growth? M in slides
  X <- matrix(scan(DataFile,skip=2*Ndim+3,n=Ndim*Ndim,quiet=T),nrow=Ndim,ncol=Ndim,byrow=T)
  
  # Identity matrix - will be used often
  I <- matrix(0,ncol=Ndim,nrow=Ndim); diag(I) = 1
  R_init <-  rep(1,Narea) ## fixed R0 in each earea
  
  FindEqn <- function(FF=0, R0_i = R_init,finding_sb0 = FALSE)
  {
    Outs <- NULL
    
    ## create R based on R_init
    R <- matrix(0,ncol=Ndim,nrow=Ndim)
    R[1,1:3] <- R0_i[1] ; R[1,4:6]  <- R0_i[2]
    H <- matrix(0,ncol=Ndim,nrow=Ndim); diag(H) = 1
    
    if(length(FF) == 1){
      # Specify the H matrix, which is literally the proportion remaining post-fishing
      H[Nsize,Nsize] <- 1.0 - FF ## 1- selex * f slide 7
      H[Nsize+Nsize,Nsize+Nsize] <- 1.0 - FF ## next stage bin is just 1-FF
    } else{
      H[Nsize,Nsize] <- 1.0 - FF[1] ## 1- selex * f slide 7, for spawners only
      H[Nsize+Nsize,Nsize+Nsize] <- 1.0 - FF[2] ## 
    }
    
    # Multiply the matrices
    Mat1 <- S ## natural survival e^-M
    Mat2 <- (X %*% (A %*% (S %*% (H %*% S)))) ## for use in slide 12
    # Matrix inversion multiplied by a recruitment vector with a 1 in the first row
    ## this assumes recruitment is 1 in all areas.
    Neqn <- solve(I-Mat2)[,1]*R[1,] ## slide 12
    
    spawntemp <- Neqn[c(3,6)] ## spawners
    
    if(finding_sb0 == FALSE){
      #print(Neqn) ## equilibrium numbers at stage in area
      ## calc equil recruitment (unclear if by area or not...)
      R_equil_temp_a1 <- Equil_Spawn_Recr_Fxn(steepness = 0.5, 
                                              SSB_virgin = Spawn0[1], 
                                              Recr_virgin = R0_i[1],
                                              SPR_temp = spawntemp[1]/R0_i[1])$R_equil
      R_equil_temp_a2 <- Equil_Spawn_Recr_Fxn(steepness = 0.5, 
                                              SSB_virgin = Spawn0[2], 
                                              Recr_virgin = R0_i[2],
                                              SPR_temp = spawntemp[2]/R0_i[2])$R_equil
      
      R_equil_temp_global <- Equil_Spawn_Recr_Fxn(steepness = 0.5, 
                                                  SSB_virgin = sum(Spawn0), 
                                                  Recr_virgin = sum(R0_i[2]),
                                                  SPR_temp = sum(spawntemp)/R0_i[2])$R_equil
      R_new <- 0.2*spawntemp ## for now
      # R_new <- c(rep(R_equil_temp_a1,3),rep(R_equil_temp_a2,3))
      # R_new <- c(R_equil_temp_a1,R_equil_temp_a2)
      # R_new <- rep(R_equil_temp_global/2,2)
      Outs$R_new = R_new
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
    
    # Compute the yield
    Catch <- sum((I-H) %*% (Mat1 %*% Neqn)) ## Mat1 * Neqn is who dies; note that H is harvest REMAINDER so 1-h is capture
    
    # Return
    Outs$FF <- FF
    Outs$Neqn <- Neqn
    Outs$Catch <- Catch
    
    
    return(Outs)
  }
  
  
  
  # =================================================================================
  
  
  
  # Spawn0 <- sum(FindEqn(FF = 0, R0_i = R_init)$Neqn[c(3,6)]) ## unfished spawning biomass
  Spawn0 <- FindEqn(FF = c(0,0), R0_i = R_init)$Neqn[c(3,6)] ## unfished spawning biomass by area
  
  # Part 1 (yield vs F)
  FFs <- seq(from=0,to=1,by=0.01)
  Yields <- OGYields <- rep(0,length(FFs))
  Spawn <- OGSpawn <- rep(0,length(FFs))
  # Rused <- matrix(NA, length(FFs), Narea)
  Rused <- array(NA, dim = c(length(FFs), Narea, 101))
  
  for (II in 1:length(FFs)){
    OGPrj <- FindEqn(FFs[II], R = R_init) ## equilbrium numbers given F
    OGYields[II] <- OGPrj$Catch ## yield at F
    OGSpawn[II] <- sum(OGPrj$Neqn[c(3,6)]) ## expected N spawners at F
    
    SSPrj <- FindEqn(FFs[II], R = R_use) ## equilbrium numbers given F
    # Yields[II] <- SSPrj$Catch ## yield at F
    # Spawn[II] <- sum(SSPrj$Neqn[c(3,6)]) ## expected N spawners at F
    
    for(k in 1:2){
      if(k == 1){
        R_use = R_init
      } else if(k > 1){
        R_use <- ModelPrj$R_new ## use last iteration
      }
      
      ModelPrj <- FindEqn(FFs[II], R = R_use) ## equilbrium numbers given F
      Yields[II] <- ModelPrj$Catch ## yield at F
      Spawn[II] <- sum(ModelPrj$Neqn[c(3,6)]) ## expected N spawners at F
      Rused[II,,k] <- R_use
      
      cat(FFs[II], R_use,  Yields[II],"\n")
    } ## end iters
  } ## end Fs
  
  
  
}