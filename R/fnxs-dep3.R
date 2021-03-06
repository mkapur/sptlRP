## load settings
Nages <- 20 
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
        len[age] <- 50*(1-exp(-0.3*(age- -1.77)))
        dat[age,"weight",1] <- 0.8*len[age]^1.81
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

  N <- array(NA, dim = c(narea,Nages,narea)) ## this is not NPR, since we are starting with proportions
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

  ## derived quants on a per-area basis
  ## these are per one recruit (no prop here)
  SBPR <- SBPF0 <- Cat <- c(0,0)
  for(area in 1:2){
    for (Iage in 1:Nages) SBPF0[area] <- SBPF0[area] + Fec[area,Iage]*sum(N_F0[area,Iage,1:2])
    for (Iage in 1:Nages) Cat[area] <- Cat[area] + WAA[area,Iage]*Sel[area,Iage]*c(F1,F2)[area]*sum(N_Z_F$N[area,Iage,1:2])/
        N_Z_F$Z[area,Iage]*(1.0-exp(-N_Z_F$Z[area,Iage]))
    
    for (Iage in 1:Nages) SBPR[area] <- SBPR[area] + Fec[area,Iage]*sum(N_Z_F$N[area,Iage,1:2]) ## does NOT have prop
  }
  # cat(sum(SBPF0),"\n")
  # cat(sum(SBPR),"\n")
  # cat(SBPF0,"\n")
  req_global <- getEqR(assumption = 'GLOBAL', 
                       SBPF0 = sum(SBPF0),
                       SSB =  sum(SBPR),
                       Prop=dat$input_prop, 
                       Steep = mean(dat$h)) 
  req_local <- getEqR(assumption = 'LOCAL', 
                      SBPF0=SBPF0, 
                      SSB=SBPR, 
                      Prop=dat$input_prop,
                      Steep = dat$h)

  # cat(req_global,"\n")
  # cat(req_local$par[1]*GLOBAL_R0,"\n")
  # cat(req_local$par[2]*GLOBAL_R0,"\n")
  
  Recr = c(req_global, req_local$par[1])
  rprop_est = req_local$par[2]
  ## equilibrium yields - should these not consider prop?
  global_catch <-  Cat*Recr[1]
  local_catch <-   Cat*Recr[2]
  
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


getEqR <- function(assumption = 'GLOBAL', SBPF0, SSB, Prop, Steep){
  if(assumption == 'GLOBAL'){
    # use summed qtties and return straight up req
    # Top <- 4*GLOBAL_R0*Steep*SSB/SBPF0 - (1-Steep)
    # Bot <- (5*Steep-1)*SSB/SBPF0
    # Recr <- Top/Bot
    # return(max(1e-4,Recr))
    
    alpha = SBPF0*(1-Steep)/(4*Steep)
    beta = (5*Steep-1)/(4*Steep*GLOBAL_R0)
    req <- max(0.001, (SSB - alpha)/(beta*SSB)) ## a la SS
  } else{
    
    ## raw calc
    # Recr <- c(0,0)
    # for(i in 1:2){
    #   Top <- 4*Steep[i]*SSB[i]/SBPF0[i] - (1-Steep[i])
    #   Bot <- (5*Steep[i]-1)*SSB[i]/SBPF0[i]
    #   Recr[i] <- max(1e-4,Top/Bot)
    # }
    # 
    # return(Recr)
    ## optimize
    opt_temp <- optim(par = c(1,Prop),
                      SBPR_F = SSB,
                      SBPR_0 = SBPF0,
                      lower = c(1E-4,1E-4),
                      upper = c(NA,1-1E-5),
                      method = "L-BFGS-B",
                      fn = optimFunc, hessian = FALSE,
                      control = list(
                        maxit = 1000,
                        ndeps = rep(1e-4,2)))
    
    return(opt_temp)
  }
  
}


optimFunc <- function(par,SBPR_0,SBPR_F){
  # cat(par,"\n")
  passR <- par[1]; passRprop <- par[2]
  # print(passR)
  # print(passRprop)
  sb_0 <- getSB(GLOBAL_R0, dat$input_prop, SBPR_0) 
  sb_F <- getSB(passR,passRprop, SBPR_F)
  Rexp = c(0,0)
  for(i in 1:narea){
    if(i == 1){
      num <- GLOBAL_R0*passRprop*4*dat$h[[i]]*sb_F[[i]]/sb_0[[i]]
    } else{
      num <- GLOBAL_R0*(1-passRprop)*4*dat$h[[i]]*sb_F[[i]]/sb_0[[i]]
    }
    denom1 <- sb_F[[i]]/sb_0[[i]]*(5*dat$h[[i]]-1)
    denom2 <- (1-dat$h[[i]])
    Rexp[i] = num/(denom1+denom2)
  } ## end area loop
  
  obsR <- passR*c(passRprop,1-passRprop)
  # cat(obsR,"\n")
  # cat(Rexp,'\n')
  obj <- sum((obsR - Rexp)^2)
  return(obj)
}
