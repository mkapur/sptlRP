## load settings
Nages <- 100#20 
narea <- 2
Ages <- 0:(Nages-1)


vals <- c('age','proportion_stay','weight','maturity',
          'fishery_selectivity','mortality') ## things to enter into data frame


logistic <- function(a,a50,a95){
  val <- 1/(1+exp(-log(19)*((a-a50)/(a95-a50))))
  return(val)
}

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
        len[age] <- max(0,50*(1-exp(-0.15*(age-2))))
        # if(age ==1) cat(len[age],"\n")
        dat[age,"weight",1] <- 0.63*len[age]^1.81
        # if(age ==1) cat(dat[age,"weight",1],"\n")
      } 
      
      dat[age,'maturity',area] <- logistic(a = age, a50 = fec_a50[area], a95 = fec_a95[area])
      dat[1,"fishery_selectivity",area] <- 0 ## don't fish recruits
      dat[age,'mortality',area] <- mort
      
    } ## end age
  } ## end area
  return(list('dat' = dat,"h" = h,"input_prop" = input_prop, 'M' = mort))
}


doNAA <- function(F1,F2, usedat, Sel){
  M <- usedat$M
  h <- usedat$h
  
  ## run area-specific NAA
  Z<- matrix(0,nrow=2,ncol=Nages)
  for (Iage in 1:Nages) Z[1,Iage] <- M+Sel[1,Iage]*F1
  for (Iage in 1:Nages) Z[2,Iage] <- M+Sel[2,Iage]*F2

  N <- array(NA, dim = c(narea,Nages,narea)) 
  ## assign single recruit to each area
  N[,1:2,1] <- c(1,0) #c(dat$input_prop,0)
  N[,1:2,2] <- c(0,1) #c(0,1-dat$input_prop)
  ## survive and move at once
  for(slice in 1:narea){
    for(age in 2:Nages){
      for(area in 1:narea){
        pLeave = NCome = 0
        for(jarea in 1:narea){
          if(area != jarea){
            pLeave = pLeave + (1-usedat$dat[age,"proportion_stay",area]) ##  leaving for elsewhere
            NCome = NCome +(1-usedat$dat[age,"proportion_stay",jarea])*
              N[jarea,age-1,slice]*exp(-Z[jarea, age-1]) ## mortality in other area
          } # end i != j
        }  # end subareas j
        N[area,age,slice] <- (1-pLeave)*N[area,age-1,slice]*exp(-Z[area, age-1]) + NCome
      } ## end sink-area loop
    } ## end ages
  } ## end source-area loop
  for(slice in 1:narea){
    for(area in 1:narea){
      N[area,Nages,slice] <-    N[area,Nages,slice]/(1-exp(-Z[area,Nages]))
    }}
  # cat(N[1,2,1],"\n")
  # plot(N)
  return(list('N' = N,'Z' = Z))
}



runSim <- function(par, 
                   dat, 
                   ret = c('opt','vals')[1], 
                   assume = 'GLOBAL'){

  F1 = exp(par[1])
  F2 = exp(par[2])
  
  WAA <-  matrix(c(dat$dat[Ages+1,'weight',1], dat$dat[Ages+1,'weight',2]),nrow=2,ncol=Nages, byrow = T)
  Sel <- matrix(c(dat$dat[Ages+1,'fishery_selectivity',1], dat$dat[Ages+1,'fishery_selectivity',2]),nrow=2,ncol=Nages, byrow = T)
  Fec <- matrix(c(Sel[1,]*WAA[1,], Sel[2,]*WAA[2,]),nrow=2,ncol=Nages, byrow = T)
    
  N_F0 <- doNAA(F1=0,F2=0, usedat =dat, Sel)$N
  N_Z_F <- doNAA(F1, F2, usedat = dat, Sel)
  
  # par(mfrow = c(1,3))
  # plot(N_F0[1,,1], col = 'blue', main = 'spawned in a1', ylim = c(0,1))
  # points(N_F0[2,,1])
  # plot(N_F0[1,,2], col = 'blue', main = 'spawned in a2', ylim = c(0,1))
  # points(N_F0[2,,2])
  # plot(rowSums(N_F0[1,,]), col = 'blue', main = 'totals', ylim = c(0,1))
  # points(rowSums(N_F0[2,,]))
  # legend('topright', legend = c('present in a1','present in a2'),pch = 1, col = c('blue','black'))

  ## derived quants on a per-area basis
  ## these are per one recruit (no prop here)
  SBPR <- SBPF0 <- Cat <- c(0,0)
  Cat <- matrix(0,nrow=2,ncol=2)
  for(area in 1:2){
    for (Iage in 1:Nages) SBPF0[area] <- SBPF0[area] + Fec[area,Iage]*sum(N_F0[area,Iage,1:2])
    for (Iage in 1:Nages) Cat[area,1] <- Cat[area,1] + WAA[area,Iage]*Sel[area,Iage]*c(F1,F2)[area]*sum(N_Z_F$N[area,Iage,1])/
        N_Z_F$Z[area,Iage]*(1.0-exp(-N_Z_F$Z[area,Iage]))
    for (Iage in 1:Nages) Cat[area,2] <- Cat[area,2] + WAA[area,Iage]*Sel[area,Iage]*c(F1,F2)[area]*sum(N_Z_F$N[area,Iage,2])/
        N_Z_F$Z[area,Iage]*(1.0-exp(-N_Z_F$Z[area,Iage]))
    
    for (Iage in 1:Nages) SBPR[area] <- SBPR[area] + Fec[area,Iage]*sum(N_Z_F$N[area,Iage,1:2]) ## does NOT have prop
  }
  
  
  # dat$h = c(0.6,0.8)
  req_global <- getEqR(assumption = 'GLOBAL', 
                       Fec = Fec,
                       N_F0 = N_F0,
                       N_Z_F = N_Z_F$N,
                       Prop=dat$input_prop, 
                       Steep = mean(dat$h)) 
  
  req_local <- getEqR(assumption = 'LOCAL', 
                      Fec = Fec,
                      N_F0 = N_F0,
                      N_Z_F = N_Z_F$N,
                      Prop=dat$input_prop,
                      Steep = dat$h)


  Recr_global = c(req_global*dat$input_prop,req_global*(1-dat$input_prop))
  # Recr_local <- c(req_local$par[1]*req_local$par[2],req_local$par[1]*(1-req_local$par[2]))
  # rprop_est = req_local$par[2]
  Recr_local <- c(req_local$par[1],req_local$par[2])
  Recr <- c(sum(Recr_global),sum(Recr_local))
  rprop_est = req_local$par[1]/Recr[2]
  # print(Recr)
  # print(Recr_global)
  # print(Recr_local)
  # print(rprop_est)
 
  global_catch <-  Cat[1,1]*Recr_global[1]+Cat[2,1]*Recr_global[1]+Cat[1,2]*Recr_global[2]+Cat[2,2]*Recr_global[2]
  local_catch <-   Cat[1,1]*Recr_local[1]+Cat[2,1]*Recr_local[1]+Cat[1,2]*Recr_local[2]+Cat[2,2]*Recr_local[2]
  #print(global_catch)
  #print(local_catch)
  # if(round(global_catch,2) != round(local_catch,2)) print(c(F1,F2))

  #AAA
  
  ## versions that consider propr
  # global_catch <- c(Cat[1]*Recr[1]*dat$input_prop,Cat[2]*Recr[1]*(1-dat$input_prop))
  # local_catch <- c(Cat[1]*Recr[2]*rprop_est,Cat[2]*Recr[2]*(1-rprop_est))
  # cat(global_catch,"\n")
  # cat(local_catch,"\n")
  ## switch for what to return 

  # global_sb0_a1 <- SBPF0[1]*Recr[1]*dat$input_prop
  # global_sb0_a2 <- SBPF0[2]*Recr[1]*(1-dat$input_prop)
  # global_tssb0 <-  sum(global_sb0_a1,global_sb0_a2)
  # sb_0 <- as.numeric(getSB(GLOBAL_R0, dat$input_prop, SBPR_F =SBPF0 ))
  global_sb0_a1 <- SBPF0[1]*GLOBAL_R0*dat$input_prop
  global_sb0_a2 <- SBPF0[2]*GLOBAL_R0*(1-dat$input_prop)
  global_tssb0 <-  sum(global_sb0_a1,global_sb0_a2)# sum(sb_0[1],sb_0[2])
  
  global_ssb_a1 <- SBPR[1]*Recr[1]*dat$input_prop
  global_ssb_a2 <- SBPR[2]*Recr[1]*(1-dat$input_prop)
  global_tssb <- sum(global_ssb_a1,global_ssb_a2)
  # ssb <- as.numeric(getSB(Recr[1]*dat$input_prop, dat$input_prop, SBPR_F =SBPR ))
  # global_ssb_a1 <- ssb[1] #SSB[1]*Recr[1]*dat$input_prop
  # global_ssb_a2 <- ssb[2] #SSB[2]*Recr[1]*(1-dat$input_prop)
  # global_tssb <- sum(ssb[1],ssb[2])
  
  global_depl_a1 <- global_ssb_a1/global_sb0_a1
  global_depl_a2 <- global_ssb_a2/global_sb0_a2
  global_tdepl <- global_tssb/ global_tssb0
  
  # local_sb0_a1 <- SBPF0[1]*Recr[2]*rprop_est
  # local_sb0_a2 <- SBPF0[2]*Recr[2]*(1-rprop_est)
  # local_tssb0 <-  sum(local_sb0_a1,local_sb0_a2)
  # sb_0 <- as.numeric(getSB(GLOBAL_R0, rprop_est, SBPR_F =SBPF0 ))
  local_sb0_a1 <- SBPF0[1]*GLOBAL_R0*dat$input_prop
  local_sb0_a2 <- SBPF0[2]*GLOBAL_R0*(1-dat$input_prop)
  local_tssb0 <-  sum(local_sb0_a1,local_sb0_a2)# sum(sb_0[1],sb_0[2])
  
  local_ssb_a1 <- SBPR[1]*Recr[2]*rprop_est
  local_ssb_a2 <- SBPR[2]*Recr[2]*(1-rprop_est)
  local_tssb <- sum(local_ssb_a1,local_ssb_a2)
  # ssb <- as.numeric(getSB(1, rprop_est, SBPR_F = SBPR ))
  # local_ssb_a1 <- ssb[1]
  # local_ssb_a2 <- ssb[2]
  # local_tssb <-  sum(ssb[1], ssb[2])
  
  local_depl_a1 <- local_ssb_a1/local_sb0_a1
  local_depl_a2 <- local_ssb_a2/local_sb0_a2
  local_tdepl <- local_tssb/ local_tssb0
  
  # if(F2 == 0) cat( SBPF0[2],"\n")
  # if(F2 == 0) cat( SSB[2],"\n")
  # if(F2 == 0) cat( local_catch[2],"\n")
  # if(F2 == 0) cat( global_catch[2],"\n")
  # if(F2 == exp(2)) cat( SBPF0[2],"\n")
  # if(F2 == exp(2)) cat( SSB[2],"\n")
  # if(F2 == exp(2)) cat( local_catch[2],"\n")
  # if(F2 == exp(2)) cat( global_catch[2],"\n")
  if(ret == 'vals'){
    return(c("tyield_global" = sum(global_catch),
             "tyield_local" = sum(local_catch),
             "local_tssb0" = local_tssb0,
             "global_tssb0" =  global_tssb0, 
             
             "local_tssb" = local_tssb, 
             "global_tssb" = global_tssb,
             
             "local_a1ssb" = local_ssb_a1, 
             "global_a1ssb" = global_ssb_a1,
             
             "local_a2ssb" = local_ssb_a2, 
             "global_a2ssb" = global_ssb_a2,
             
             "req_local"= req_local$par[1], ## just return req not prop
             "req_local_prop" = rprop_est,
             "req_global" =req_global))
    
  }else{
    if(assume == "GLOBAL"){obj <- -sum(global_catch)}
    else{obj <- -sum(local_catch)}
    return(obj)
  }
}


getSB <- function(passR, passRprop, SBPR_F){
  # SBPR_F_A1 <- sum(SBPR_F[1,,1],SBPR_F[1,,2])
  # SBPR_F_A2 <- sum(SBPR_F[2,,1],SBPR_F[2,,2])
  SB_A1 <- SBPR_F[1]*passR*passRprop
  SB_A2 <- SBPR_F[2]*passR*(1-passRprop)
  return(list('SB_A1'=SB_A1,"SB_A2"=SB_A2))
}


getEqR <- function(assumption = 'GLOBAL', Fec, N_F0, N_Z_F, Prop, Steep){
  if(assumption == 'GLOBAL'){
    # use summed qtties and return straight up req
    # Top <- 4*GLOBAL_R0*Steep*SSB/SBPF0 - (1-Steep)
    # Bot <- (5*Steep-1)*SSB/SBPF0
    # Recr <- Top/Bot
    # return(max(1e-4,Recr))
    # cat(Steep,"\n")
    
    SBPR <- SBPF0 <- Cat <- c(0,0)
    # Nages <- 20
    Nages = 100
    for(area in 1:2){
      for (Iage in 1:Nages) SBPF0[area] <- SBPF0[area] + Fec[area,Iage]*(Prop*N_F0[area,Iage,1]+(1-Prop)*N_F0[area,Iage,2])
      for (Iage in 1:Nages) SBPR[area] <- SBPR[area] + Fec[area,Iage]*(Prop*N_Z_F[area,Iage,1]+(1-Prop)*N_Z_F[area,Iage,2])
    }
    SBPF0 <- sum(SBPF0)
    SSB  <- sum(SBPR)

    alpha <- SBPF0*(1-Steep)/(4*Steep)
    beta <- (5*Steep-1)/(4*Steep*GLOBAL_R0)
    req <- max(0.001, (SSB - alpha)/(beta*SSB)) ## a la SS
    #cat("LocalR",req,"\n")
  } else{
    
    ## raw calc
    # Recr <- c(0,0)
    # SBPR <- SBPF0 <- Cat <- c(0,0)
    # # Nages <- 20
    # Nages = 100
    # for(area in 1:2){
    #   for (Iage in 1:Nages) SBPF0[area] <- SBPF0[area] + Fec[area,Iage]*(Prop*N_F0[area,Iage,1]+(1-Prop)*N_F0[area,Iage,2])
    #   for (Iage in 1:Nages) SBPR[area] <- SBPR[area] + Fec[area,Iage]*(Prop*N_Z_F[area,Iage,1]+(1-Prop)*N_Z_F[area,Iage,2])
    # }
    # for(i in 1:2){
    #   Top <- 4*Steep[i]*SBPR[i]/SBPF0[i] - (1-Steep[i])
    #   Bot <- (5*Steep[i]-1)*SBPR[i]/SBPF0[i]
    #   Recr[i] <- max(1e-4,Top/Bot)
    # }

    # print(Recr)
    # return(list('par' = c(sum(Recr), Recr[1]/Recr[2])))
    ## optimize
    # dat$h = c(0.6,0.8)
    opt_temp <- optim(par = c(0.5,0.5),
                      Fec = Fec,
                      N_F0 = N_F0,
                      N_Z_F = N_Z_F,
                      # lower = c(1E-4,1E-4),
                      # upper = c(NA,NA),
                      # method = "L-BFGS-B",
                      fn = optimFunc, hessian = FALSE,
                      control = list(
                        maxit = 1000,
                        ndeps = rep(1e-4,2)))
    # cat(opt_temp$par,"\n")
    return(opt_temp)
  }
  
}


optimFunc <- function(par,Fec,N_F0,N_Z_F){
  # cat(par,"\n")
  # cat(dat$h,"\n")
  # passR <- par[1]; passRprop <- par[2]
  passR1 <- par[1]; passR2 <- par[2]
  passR = sum(passR1,passR2)
  passRprop = passR1/sum(passR)
  #print(passR)
  #print(passRprop)
  sb_F <- sb_0 <- Cat <- c(0,0)
  PropF0 = dat$input_prop
  RecF0 = GLOBAL_R0
  Nages <- 100 #20
  for(area in 1:2){
    for (Iage in 1:Nages) sb_0[area] <- sb_0[area] + RecF0*Fec[area,Iage]*(PropF0*N_F0[area,Iage,1]+(1-PropF0)*N_F0[area,Iage,2])
    for (Iage in 1:Nages) sb_F[area] <- sb_F[area] + passR*Fec[area,Iage]*(passRprop*N_Z_F[area,Iage,1]+(1-passRprop)*N_Z_F[area,Iage,2])
    # for (Iage in 1:Nages) sb_F[area] <- sb_F[area] + Fec[area,Iage]*(passR1*N_Z_F[area,Iage,1])+passR2*N_Z_F[area,Iage,2]
  }
  # cat(sb_F,"\n")
  # cat(sb_0,"\n")
  Rexp = c(0,0)
  for(i in 1:narea){
    if(i == 1){
      # num <- GLOBAL_R0*passRprop*4*dat$h[i]*sb_F[[i]]/sb_0[[i]]
      num <- GLOBAL_R0*PropF0*4*dat$h[i]*sb_F[[i]]/sb_0[[i]]
      # cat(i,"\t",num,"\n")
    } else{
      # num <- GLOBAL_R0*(1-passRprop)*4*dat$h[i]*sb_F[[i]]/sb_0[[i]]
      num <- GLOBAL_R0*(1-PropF0)*4*dat$h[i]*sb_F[[i]]/sb_0[[i]]
      # cat(i,"\t",num,"\n")
    }
    denom1 <- sb_F[[i]]/sb_0[[i]]*(5*dat$h[i]-1)
    denom2 <- (1-dat$h[i])
    # cat(i,"\t",denom1+denom2,"\n")
    Rexp[i] = num/(denom1+denom2)
  } ## end area loop
  
  obsR <- passR*c(passRprop,1-passRprop)
  # cat(obsR,"\n")
  # cat(Rexp,'\n')
  obj <- sum((obsR - Rexp)^2)
  #cat(passR,passRprop,obj,"\n")
  return(obj)
}
