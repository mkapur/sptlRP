
doNage <- function(X = X_ija, ## input movement matrix
                   indat = dat, ## with bio info'
                   s = 1, ## F = 1, M = 2
                   Fv = rep(0,narea),
                   M = 0.15,
                   SB0 = SB0_i,
                   eq_method = c('STD','TIME','STB')[1]) {
  if(eq_method == 'STD'){
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
            } # end i != j
          } # end subareas j
          N_ai[a,i] <- ((1-pLeave)* N_ai[a-1,i] +NCome)*exp(-Z_ai[a-1,i])
        } ## end age < maxage
        if(a == max(nages)) N_ai[a,i] <-  N_ai[a-1,i]*exp(-Z_ai[a-1,i])/(1- exp(-Z_ai[a,i]))
        B_ai[a,i] <- N_ai[a,i]*indat[a,s+2,i] ## weight in 3 and 4 col
        if(s == 1){
          # SB_ai[a,i]  <- NA
          SB_ai[a,i]  <- B_ai[a,i]*indat[a,2,i]
        } 
        B_i <- sum(B_ai[,i])
        SB_i <- sum(SB_ai[,i])
      } # end subareas i 
    } ## end ages
  } ## end eq_method == STD
  else if (eq_method == 'TIME'){
    #   ## run for nage*2 years with randomly sampled recdevs...
    N_aiy <- Z_aiy <- B_aiy <- SB_aiy  <- array(NA, dim = c(nages, narea, 50)) ## run for 50 yrs
    R_iy <- matrix(NA, nrow = 50, ncol = narea)
    
    for(y in 1:(dim(N_aiy)[3]-1)){
      if(y == 1){
        for(a in 1:nages){
            if(a ==1){
              N_aiy[1,,1] <- 0.5 ## inits
              Z_aiy[1,,1] <- M + indat[1,s+4,]*Fv ## female selex for now (cols 5:6)
            }
          for(i in 1:narea){
            if(a > 1  & a < max(nages)) {
              Z_aiy[a,i,y] <- M + indat[a,s+4,i]*Fv[i] ## female selex for now (cols 5:6)
              if(is.na(Z_aiy[a,i,y])) stop(a,i,'error in ZAIY')
              pLeave = NCome = 0
              for(j in 1:narea){
                if(i != j){
                  pLeave = pLeave + X_ija[i,j,a-1]
                  NCome = NCome + X_ija[j,i,a-1]*N_aiy[a-1,j,y]
                } # end i != j
              } # end subareas j
              N_aiy[a,i,1] <- ((1-pLeave)* N_aiy[a-1,i,y] +NCome)*exp(-Z_aiy[a-1,i,y])
              if(is.na(N_aiy[a,i,y])) stop('error in NAIY')
            } ## end age < maxage
            if(a == max(nages)){
              Z_aiy[a,i,y] <- M + indat[a,s+4,i]*Fv[i] 
              N_aiy[a,i,y] <-  N_aiy[a-1,i,y]*exp(-Z_aiy[a-1,i,y])/(1- exp(-Z_aiy[a-1,i,y]))
            } ## end plus group
            B_aiy[a,i,1] <- N_aiy[a,i,y]*indat[a,s+2,i] ## weight in 3 and 4 col
            # if(s == 1){
              # SB_aiy[a,i,y]  <- NA
              SB_aiy[a,i,1]  <- B_aiy[a,i,y]*indat[a,2,i]
            # } ## end spawning biomass 
          } ## end subareas i  
        } # end ages
        for(i in 1:narea){
          B_i <- sum(B_aiy[,i,1])
          SB_i <- sum(SB_aiy[,i,1]) ## single value for area
          R_iy[1,i] <- doSRR(SB_aiy = SB_i,
                             SB0i = SB0_i[which(row.names(SB0_i) == "TIME"),i])
          N_aiy[1,i,2] <- R_iy[y,i]
          
        } ## end final loop of subareas to get recruits
      } ## end y == 1
      else if(y > 1){
        
        for(a in 1:nages){
          for(i in 1:narea){
            Z_aiy[1:nages,i,y] <- M + indat[1:nages,s+4,i]*Fv[i] ## female selex for now (cols 5:6)
            
            if(a > 1  & a < max(nages)) {
              pLeave = NCome = 0
              for(j in 1:narea){
                if(i != j){
                  pLeave = pLeave + X_ija[i,j,a-1]
                  NCome = NCome + X_ija[j,i,a-1]*N_aiy[a-1,j,y-1]
                } # end i != j
              } # end subareas j
              N_aiy[a,i,y] <- ((1-pLeave)* N_aiy[a-1,i,y-1] +NCome)*exp(-Z_aiy[a-1,i,y-1])
              if(is.na(N_aiy[a,i,y])) stop(a,i,y,'error in NAIY')
            } ## end age < maxage
            if(a == max(nages)) N_aiy[a,i,y] <-  N_aiy[a-1,i,y-1]*exp(-Z_aiy[a-1,i,y-1])/(1- exp(-Z_aiy[a,i,y-1]))
            B_aiy[a,i,y] <- N_aiy[a,i,y]*indat[a,s+2,i] ## weight in 3 and 4 col
            # if(s == 1){
              SB_aiy[a,i,y]  <- B_aiy[a,i,y]*indat[a,2,i]
            # }
          } ## end ages  
        } # end subareas i 
        for(i in 1:narea){
          
          B_i <- sum(B_aiy[,i,y])
          SB_i <- sum(SB_aiy[,i,y])
          R_iy[y,i] <- doSRR(SB_aiy = SB_i,
                             SB0i = SB0_i[which(row.names(SB0_i) == 'TIME'),i])
          
          if (y < 50)     N_aiy[1,i,y+1] <- R_iy[y,i]
        } # end subareas i 
        
        
      } ## end y > 1
      
    } ## end years
    ## take last year
    N_ai <- N_aiy[,,21]; Z_ai<- Z_aiy[,,21]; B_ai<- B_aiy[,,21]; SB_ai<- SB_aiy[,,21]; 
  }## end eq_method == TIME
  else if (eq_method == 'STB'){
    R0 = 1
    ## use eigenvector 
    N_ai <- Z_ai <- B_ai <- SB_ai<- matrix(NA, nrow = nages, ncol = narea) ## placeholder
    for(a in 1:nages){
      # omega_ij <- eigen(X_ija[,,a])$values/sum(eigen(X_ija[,,a])$values)
      omega_ij <- rev(eigen(X_ija[,,a])$values) ## i think these are ordered 3:1? 
      
      for(i in 1:narea){
        N_ai[1,i] <- 0.5
        
        if(a >1 &  a < max(nages)) {
          Z_ai[a,i] <- a*(M+ indat[a,s+4,i]*Fv[i]) ## female selex for now (cols 5:6)
          
          N_ai[a,i] <- 0.5*omega_ij[i]*R0*exp(-Z_ai[a,i])
        } ## end age < maxage
        if(a == max(nages)){
          Z_ai[a,i] <- (M+ indat[a,s+4,i]*Fv[i]) ## female selex for now (cols 5:6)
          
          N_ai[a,i] <-  omega_ij[i]*N_ai[a-1,i]*exp(-Z_ai[a,i])/(1- exp(-Z_ai[a,i]))
        }
        B_ai[a,i] <- N_ai[a,i]*indat[a,s+2,i] ## weight in 3 and 4 col
        if(s == 1){
          # SB_ai[a,i]  <- NA
          SB_ai[a,i]  <- B_ai[a,i]*indat[a,2,i]
        } 
        B_i <- sum(B_ai[,i])
        SB_i <- sum(SB_ai[,i])
      } # end subareas i 
    } ## end ages
  }## end eq_method == TIME
  return(cbind(N_ai,Z_ai,B_ai,SB_ai))
}
