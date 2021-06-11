
makeDat <- function(nage = 100, 
                    narea =2, 
                    h = steep,
                    input_prop,
                    wa = NULL,  
                    mort = M,
                    fec_a50, fec_a95,
                    slx_a50,slx_a95,
                    pStay = c(0.9,0.6)){
  dat <- array(NA, dim = c(nage+1,length(vals),narea),dimnames = list(c(0:nage), c(vals), c(1:narea)))
  
  for(area in 1:narea){
    len = NULL
    dat[,'fishery_selectivity',area] <- logistic(0:nage, a50 = slx_a50[area], a95 = slx_a95[area])
    for(age in 1:(nage+1)){
      
      dat[age,"age",area] <- age-1
      dat[1,"proportion_stay",area] <- 1 ## recruits stay put
      
      ## do descending pstay from 1 (full sedentary) to Xija (whatever the terminal sedentary prop is)
      if(age <10 & age > 0) dat[age,"proportion_stay",area] <- 1+age*(1-pStay[area])/-9
      # if(age <10 & age > 1) dat[age,"proportion_stay",area] <- min(c(pStay[area], age*(pStay[area])/length(1:12)+0.25))
      if(age >= 10) dat[age,"proportion_stay",area] <- pStay[area]
      if(all(pStay == 1)) dat[age,"proportion_stay",area] <- 1 ## no movement exception
      
      ## make weight @ age correct
      if(is.null(wa)){ ## default WAA
        len[age] <- 50*(1-exp(-0.15*(age-1)))
        dat[age,"weight",area] <- 0.63*len[age]^1.81
      } else{
        len[age] <- 50*(1-exp(-0.15*(age-1)))
        dat[age,"weight",2] <- 0.63*len[age]^1.81
        ## make area 1 bigger
        len[age] <- 50*(1-exp(-0.3*(age- -1.77)))
        dat[age,"weight",1] <- 0.8*len[age]^2
      } 
      
      dat[age,'maturity',area] <- logistic(a = age, a50 = fec_a50[area], a95 = fec_a95[area])
      # if(!is.na(slx_a50[1])){
      #   dat[age,'fishery_selectivity',area] <- logistic(a = age, a50 = slx_a50[area], a95 = slx_a95[area])
      # } else{
      #   dat[age,'fishery_selectivity',area] <- 1 ## full slx
      # }
      
      dat[1,"fishery_selectivity",area] <- 0 ## don't fish recruits
      dat[age,'mortality',area] <- mort
      
    } ## end age
  } ## end area
  return(list('dat' = dat,"h" = h,"input_prop" = input_prop, 'm' = mort))
}


logistic <- function(a,a50,a95){
  val <- 1/(1+exp(-log(19)*((a-a50)/(a95-a50))))
  return(val)
}

PopN_local <- function(par,
                       getSBPF0 = FALSE, 
                       returnVals= FALSE,
                       slx_a50 = 9, 
                       slx_a95 = 13,
                       h = c(0.7,0.7),
                       pstay_a1 = dat[,'proportion_stay',1],
                       pstay_a2 = dat[,'proportion_stay',2]){
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
  Sel <- matrix(0,nrow=2,ncol=Nages)
  Sel[1,] <- logistic(a = Ages, a50 = slx_a50, a95 = slx_a95)
  Sel[2,] <- logistic(a = Ages, a50 = 9, a95 = 13)
  # cat('NASEL',sum(is.na(Sel)),"\n")
  Fec <- Sel*WAA
  M <- 0.2
  Steep <- h
  
  Z<- matrix(0,nrow=2,ncol=Nages)
  for (Iage in 1:Nages) Z[1,Iage] <- M+Sel[1,Iage]*F1
  for (Iage in 1:Nages) Z[2,Iage] <- M+Sel[2,Iage]*F2
  # cat('NAZ',sum(is.na(Z)),"\n")
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
  # cat('NAN',sum(is.na(N)),"\n")
  
  SSB <- Cat <- Recr <- c(0,0)
  for(i in 1:2){
    for (Iage in 1:Nages) Cat[i] <- Cat[i] + WAA[Iage]*Sel[Iage]*c(F1,F2)[i]*N[i,Iage]/Z[i,Iage]*(1.0-exp(-Z[i,Iage]))
    for (Iage in 1:Nages) SSB[i] <- SSB[i] + Fec[Iage]*N[i,Iage]
    
    Top <- 4*Steep[i]*SSB[i]/SBPF0[i] - (1-Steep[i])
    Bot <- (5*Steep[i]-1)*SSB[i]/SBPF0[i]
    Recr[i] <- Top/Bot
    
  }
  # cat('Recr',sum(is.na(Recr)),"\n")
  # cat('Cat',sum(is.na(Cat)),"\n")
  # cat('SSB',sum(is.na(SSB)),"\n")
  
  
  if(getSBPF0 == T){
    return(SSB) ## stop here, no need for recruit (prop already inside)
  } else if(returnVals == T){
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

PopN_global <-function(par, 
                       returnVals= F, 
                       slx_a50 = 9, 
                       slx_a95=13,
                       h = 0.7,
                       pstay_a1 = dat[,'proportion_stay',1],
                       pstay_a2 = dat[,'proportion_stay',2]){
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
  Sel <- matrix(0,nrow=2,ncol=Nages)
  Sel[1,] <- logistic(a = Ages, a50 = slx_a50, a95 = slx_a95)
  Sel[2,] <- logistic(a = Ages, a50 = 9, a95 = 13)
  
  Fec <- Sel*WAA
  M <- 0.2
  Steep <- mean(h) ## covers if >2
  
  Z<- matrix(0,nrow=2,ncol=Nages)
  for (Iage in 1:Nages) Z[1,Iage] <- M+Sel[1,Iage]*F1
  for (Iage in 1:Nages) Z[2,Iage] <- M+Sel[2,Iage]*F2
  
  
  
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
  
  if(returnVals == T){
    return(list('Prop','F1','F2',
                # 'Cat1' = Cat[1], 'Cat2' = Cat[2],
                # 'Rec1' = Recr[1], 'Rec2' = Recr[2],
                # 'SSB1' = SSB[1], 'SSB2' = SSB[2],
                # 'SBPF01' = SBPF0[1],'SBPF02' = SBPF0[2],
                'tyield' = sum(Cat),
                'trec' = sum(Recr),
                'tSSB' = sum(SSB),
                'tSBPF0' = sum(SBPF0),
                'tDepl' = sum(SSB)/sum(SBPF0)))
  }else{
    if (Detail==T) cat(round(c(Prop,F1,F2,Cat,Recr,SSB,SBPF0,SSB/SBPF0),1),"\n")
    
    obj <- -Cat
    return(obj)
  }
}
