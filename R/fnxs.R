## functions used

makeDat <- function(nage = 100, 
                    narea =2, 
                    wa = NULL,  
                    mort = exp(-0.2),
                    fec_a50, fec_a95,
                    slx_a50,slx_a95,
                    pStay = c(0.9,0.6)){
  dat <- array(NA, dim = c(nage+1,length(vals),narea),dimnames = list(c(0:nage), c(vals), c(1:narea)))
  
  for(area in 1:narea){
    len = NULL
    for(age in 1:(nage+1)){
      
      dat[age,"age",area] <- age-1
      dat[1,"proportion_stay",area] <- 1 ## recruits stay put
      
      ## do descending pstay from 1 (full sedentary) to Xija (whatever the terminal sedentary prop is)
      if(age <10 & age > 0) dat[age,"proportion_stay",area] <- 1+age*(1-pStay[area])/-9
      # if(age <10 & age > 1) dat[age,"proportion_stay",area] <- min(c(pStay[area], age*(pStay[area])/length(1:12)+0.25))
      if(age >= 10) dat[age,"proportion_stay",area] <- pStay[area]
      if(all(pStay == 1)) dat[age,"proportion_stay",area] <- 1 ## no movement exception
      
      ## make weight lenght age correct
      # 
      len[age] <- 50*(1-exp(-0.2*(age-1)))
      dat[age,"weight",area] <- 0.63*len[age]^1.81
      if(!is.null(wa)) dat[age,"weight",1] <- 0.8*len[age]^2
      
      # dat[age,"weight",area] <- wa[area] * age 
      
      dat[age,'maturity',area] <- logistic(a = age, a50 = fec_a50[area], a95 = fec_a95[area])
      if(!is.na(slx_a50[1])){
        dat[age,'fishery_selectivity',area] <- logistic(a = age, a50 = slx_a50[area], a95 = slx_a95[area])
      } else{
        dat[age,'fishery_selectivity',area] <- 1 ## full slx
      }
    
      dat[1,"fishery_selectivity",area] <- 0 ## don't fish recruits
      dat[age,'mortality',area] <- mort
      
    } ## end age
  } ## end area
  # 
  # png(here('figs',paste0(Sys.Date(),"-",SCENARIO,'-inputDat.png')),
  #     width = 8, height = 8, units = 'in', res = 400)
  # par(mfrow = c(2,2), mar = c(4,4,1,1))
  # for(v in 2:5){
  #   age <- 0:100
  #   plot(dat[,v,1] ~ age, type = 'p', pch = 19, xlab='age', ylab = vals[v],
  #        col = alpha('black',0.5),
  #        ylim = c(0,ifelse(vals[v]!='weight',1.1,1100)))
  # 
  #   text(x = 10, y = ifelse(vals[v]!='weight',1.05,1050), label = LETTERS[v-1], cex = 1.5)
  #   if(v == 2){
  #     legend('bottomright', legend = c('Area 1','Area 2'), cex = 1.2,
  #            pch = 19, col = alpha(c('black','blue'),0.5))
  #   }
  #   points(dat[,v,2] ~ age, type = 'p', pch = 19,col = alpha('blue',0.5),)
  # }
  # dev.off()
  # cat("saved 2x2 input data figure in figs \n")
  return(dat)
}



doPR <- function(dat, narea = 2, nage = 100, FF = c(0,0)){
  NPR_SURV <- NPR <- BPR <- SBPR <- YPR <- array(NA, dim = c(narea,nage,narea)) ## now 100 ages of record
  NPR_SURV[,1,1] <- NPR[,1,1] <- c(1,0);  NPR_SURV[,1,2] <-  NPR[,1,2] <- c(0,1) ## single recruit to each area
  
  for(slice in 1:narea){
    ## Calc Survivors for each area-age within slice
    for(age in 2:nage){
      for(area in 1:narea){
        NPR_SURV[area,age,slice] <- NPR_SURV[area,age-1,slice]*dat[age,'mortality',slice] ## Ntilde
      } ## end survivors-in-area
    } ## end ages 2:nage
    
    for(area in 1:narea){
      for(age in 1:nage){
        pLeave = NCome = 0
        for(jarea in 1:narea){
          ## now calculate movement with F
          if(area != jarea){
            pLeave = pLeave + (1-dat[age,"proportion_stay",area]) ##  leaving for elsewhere
            NCome = NCome +(1-dat[age,"proportion_stay",jarea])*NPR_SURV[jarea,age,slice]*exp(-FF[jarea]) ##  leaving other area post F there
          } # end i != j
        }
        # end subareas j
        NPR[area,age,slice] <- (1-pLeave)*NPR_SURV[area,age,slice]*exp(-FF[area]) + NCome
        Ztemp <- -log(dat[age,'mortality',slice])+dat[age,"fishery_selectivity",area]*FF[area]
        
        
        # } ## end ages 2:nage
        ## Calc Yield for each area-age - use baranov catch equation!
        ## bpr IS Wa x Nax
        ## make sure ztemp is not in exp space (so log mortality, which is exp(-M), really should be survivorship)
        
        YPR[area,age,slice] <- (dat[age,"fishery_selectivity",area]*
                                  FF[area]*
                                  NPR[area,age,slice]* ## use total numbers available in area
                                  # NPR_SURV[area,age,slice]*
                                  dat[age,"weight",area]*
                                  (1-exp(-Ztemp)))/(Ztemp)
        # for(age in 1:nage){
        BPR[area,age,slice] <- NPR[area,age,slice]*dat[age,"weight",area]
        SBPR[area,age,slice] <- BPR[area,age,slice]*dat[age,"maturity",area]
        
        # cat( YPR[area,age,slice],"\n")
      } ## end ages 1:nage
    } ## end areas
  } ## end slices (array)
  # cat(FF,Ztemp,sum(YPR[,,,ny]), sum(NPR[,,,ny]), "\n")
  return(list("NPR"=NPR[,1:25,],
              "BPR"=BPR[,1:25,],
              "SBPR"=SBPR[,1:25,],
              "YPR"=YPR[,1:25,]))
} ## end func

eq3d <- function(SBPR_F, steep, SBPR_0, r0 = R0_global){
  ## these are equivalent
  alpha = SBPR_0*(1-steep)/(4*steep)
  beta = (5*steep-1)/(4*steep*r0)
  req <- max(0.001, (SBPR_F - alpha)/(beta*SBPR_F)) ## a la SS
  
  # alpha = 4.0 * steep*R0_global / (5*steep-1)
  # beta = ( SBPR_0*(1-steep)) / (5*steep-1)
  # B_equil = alpha *  SBPR_F - beta
  # B_equil = max(B_equil,0.0001)
  # req =(4*steep*R0_global*B_equil) / ( SBPR_0*(1-steep)+(5*steep-1)*B_equil)
  return(req)
  
}
# R0_global*(4*mean(h)*sum(tmp$SBPR) - (1-mean(h))*sum(tmp0$SBPR) )/((5*mean(h)-1)*sum(tmp$SBPR) )

# eq3d(SBPR_F = sum(tmp$SBPR), steep = mean(h), SBPR_0 = sum(tmp0$SBPR))


## takes F, performs optimization and returns derived quants at Rbar hat rprop hat
optim_loop <- function(FFs,i){
  if(is.na(i)){ ## for uniroot
    tmp <- doPR(dat,FF = FFs)
  } else{
    ## for testing
    tmp <- doPR(dat,FF = as.numeric(c(FFs[i,]))) 
  }
  # cat(is.na(tmp),"\n")
  tmp0 <-  doPR(dat,FF = c(0,0) )
  ## optimize this (find rbar and prop)
  opt_temp <- optim(par = c(4,0.6),
                    SBPR_F = tmp$SBPR,
                    SBPR_0 = tmp0$SBPR,
                    lower = c(1E-4,1E-4),
                    upper = c(NA,0.9999),
                    method = "L-BFGS-B",
                    fn = optimFunc, hessian = FALSE,
                    control = list(
                      maxit = 1000,
                      ndeps = rep(1e-4,2)))
  
  return(list("opt_temp"=opt_temp,"tmp0"=tmp0,"tmp"=tmp))
}

## run analysis across discrete surface given input LH and FF vetors
makeOut <- function(dat,FFs){
  ## Generate a surface of expected yields, given new method
  out <- array(NA, dim = c(nrow(FFs),15,2), 
               dimnames = list(c(1:nrow(FFs)),c("FF_Area1","FF_Area2",
                                                "estRbar","estRprop",
                                                "Yield_A1","Yield_A2",
                                                "SB_A1","SB_A2",
                                                "SB0_A1","SB0_A2",
                                                "expR_A1","expR_A2",
                                                "obsR_A1","obsR_A2","tyield"),
                               c('new','old'))) ## each slice is old or new
  
  for(i in 1:nrow(FFs)){
    # cat(i,"\n")
    out[i,'FF_Area1',] <- FFs[i,1];   out[i,'FF_Area2',] <- FFs[i,2]
    ## this is the new method; old method uses global inputs
    opt0 <- optim_loop(FFs,i) 
    opt_temp <- opt0$opt_temp; tmp0 <- opt0$tmp0; tmp <- opt0$tmp
    ## new method, use optimized Rbar(F) and phi_i----
    ## these are the values which return recruitment most similar to what
    ## we'd get using the BH given our spatial dynamic.
    if(any(opt_temp$par < 0)) next() ## bail if these are negative (happens with non Pope methods)
    out[i,'estRbar',1] <- opt_temp$par[1];  out[i,'estRprop',1] <- opt_temp$par[2];
    
    ## derived quants at optimized value
    yields <- as.numeric(getYield(passR = out[i,'estRbar',1], passRprop =   out[i,'estRprop',1], YPR_F = tmp$YPR))
    out[i,'Yield_A1',1] <- yields[1];  out[i,'Yield_A2',1] <- yields[2];
    
    sbs <- getSB(passR = out[i,'estRbar',1], passRprop = out[i,'estRprop',1], SBPR_F = tmp$SBPR)
    out[i,'SB_A1',1] <- as.numeric(sbs[1]);  out[i,'SB_A2',1] <- as.numeric(sbs[2]);
    
    # sb0 <- getSB(passR = R0_global, passRprop = out[i,'estRprop',1], SBPR_F = tmp0$SBPR)
    # sb0 <- getSB(passR = out[i,'estRbar',1], passRprop = out[i,'estRprop',1], SBPR_F = tmp0$SBPR)
    sb0 <- getSB(passR = R0_global, passRprop = Rprop_input, SBPR_F = tmp0$SBPR)
    
    out[i,'SB0_A1',1] <- as.numeric(sb0[1]);  out[i,'SB0_A2',1] <-as.numeric(sb0[2]);
    
    ## return expected (BH) with optimized pars
    ## basically this modifies the BH by the proportion identified
    
    ## and ensuring that we are doing so on a per-area basis (already uses prop_input)
    rexp <- as.numeric(getExpR( SB_F =sbs, SB_0 =sb0, meth= 1))
    out[i,'expR_A1',1] <- rexp[1];  out[i,'expR_A2',1] <- rexp[2];
    
    ## keep in mind we already "knew" these as opt was created, we are just printing them
    ## return the deterministic recruitment given the pars (simply multiply global by prop)
    obsr <- as.numeric(out[i,'estRbar',1]*c(out[i,'estRprop',1],1-out[i,'estRprop',1]))
    out[i,'obsR_A1',1] <- obsr[1];  out[i,'obsR_A2',1] <- obsr[2];
    rm(opt0)
    
    ## derived quants at global value ("current method")----
    ## old method, use straight inputs
    out[i,'estRprop',2] <- Rprop_input

    out[i,'estRbar',2] <- eq3d(SBPR_0 = sum(tmp0$SBPR), steep = mean(h),
                               r0 = R0_global, SBPR_F = sum(tmp$SBPR))
    
    # (sum(tmp$SBPR)-(sum(tmp0$SBPR)*(1-h))/(sum(tmp$SBPR)*(4*mean(h))/(5*mean(h)-1)/(4*mean(h)*R0_global)))
    # cat("Req w R0global", req,"\n")
    # cat("Req x SBPReq x prop,1-prop ", req*sum(tmp$SBPR)*c( out[i,'estRprop',2],1-out[i,'estRprop',2]),"\n")
    
    yields <- as.numeric(getYield(passR = out[i,'estRbar',2], passRprop =   out[i,'estRprop',2], YPR_F = tmp$YPR))
    out[i,'Yield_A1',2] <- yields[1];  out[i,'Yield_A2',2] <- yields[2];
    
    
    sbs <- getSB(passR = out[i,'estRbar',2], passRprop = out[i,'estRprop',2], SBPR_F = tmp$SBPR)
    # cat(unlist(sbs),"\n")
    out[i,'SB_A1',2] <- as.numeric(sbs[1]);  out[i,'SB_A2',2] <- as.numeric(sbs[2]);
    
    sb0 <- getSB(passR = R0_global, passRprop = Rprop_input, SBPR_F = tmp0$SBPR)
    # sb0 <- getSB(passR = R0_global, passRprop = out[i,'estRprop',2], SBPR_F = tmp0$SBPR)
    out[i,'SB0_A1',2] <- as.numeric(sb0[1]);  out[i,'SB0_A2',2] <- as.numeric(sb0[2]);
    # cat(unlist(sb0),"\n")
    
    
    ## Run bev-hold using global R0 and input proportion (these don't affect sbpr)
    rexp <- as.numeric(getExpR(SB_F = data.frame(sbs), SB_0 = data.frame(sb0), meth = 2)) ## one value, global rec
    out[i,'expR_A1',2] <- rexp*out[i,'estRprop',2];  out[i,'expR_A2',2] <- rexp*(1-out[i,'estRprop',2]);
    # cat("rexp x prop", rexp*c( out[i,'estRprop',2],1-out[i,'estRprop',2]),"\n")
    # cat("Req x prop", req*c( out[i,'estRprop',2],1-out[i,'estRprop',2]),"\n")
    
    obsr <- as.numeric(out[i,'estRbar',2]*c( out[i,'estRprop',2],1-out[i,'estRprop',2]))
    out[i,'obsR_A1',2] <- obsr[1];  out[i,'obsR_A2',2] <- obsr[2];
    # cat("obsr", obsr,"\n")
    
    out[i,'tyield',1] <- out[i,'Yield_A1',1]+ out[i,'Yield_A2',1]
    out[i,'tyield',2] <- out[i,'Yield_A1',2]+ out[i,'Yield_A2',2]
    
    # out[i,'ralstonR_A1',1] <- out[i,'expR_A1',1] /out[i,'SB_A1',1]*(out[i,'SB_A1',1]+out[i,'SB_A2',1])/2
    # out[i,'ralstonR_A2',1] <- out[i,'expR_A2',1] /out[i,'SB_A2',1]*(out[i,'SB_A1',1]+out[i,'SB_A2',1])/2
    
    # out[i,'ralstonR_A1',2] <- out[i,'expR_A1',2] /out[i,'SB_A1',2]*(out[i,'SB_A1',2]+out[i,'SB_A2',2])/2
    # out[i,'ralstonR_A2',2] <- out[i,'expR_A2',2] /out[i,'SB_A2',2]*(out[i,'SB_A1',2]+out[i,'SB_A2',2])/2
    
  } ## end FFs loop
  return(out)
}
makeOut2 <- function(propmsy){
  
  ## now take what's determined to be FMSY  and return ssb, yield etc
  ## this is telling us where the best yield actually occurs, as a function of both
  out2 <- array(NA, dim = c(nrow(propmsy),17,2), 
                dimnames = list(c(rep(NA, nrow(propmsy))),c("Fprop", "FMSY",
                                                            "FF_Area1","FF_Area2",
                                                            "estRbar","estRprop",
                                                            "Yield_A1","Yield_A2",
                                                            "SB_A1","SB_A2",
                                                            "SB0_A1","SB0_A2",
                                                            "expR_A1","expR_A2",
                                                            "obsR_A1","obsR_A2", 'tyield'),
                                c('new','old'))) ## each slice is old or new
  for(i in 1:nrow(propmsy)){
    out2[i,'Fprop',1:2] <- propmsy[i,'Fprop']
    out2[i,'FMSY',1] <- propmsy[i,'FMSY_new']
    out2[i,'FMSY',2] <- propmsy[i,'FMSY_global']
    
    FFs_new <- c(propmsy[i,'Fprop']*propmsy[i,'FMSY_new'],(1-propmsy[i,'Fprop'])*propmsy[i,'FMSY_new'] )
    FFs_global <- c(propmsy[i,'Fprop']*propmsy[i,'FMSY_global'],(1-propmsy[i,'Fprop'])*propmsy[i,'FMSY_global'] )
    
    out2[i,'FF_Area1',1] <- FFs_new[1]; out2[i,'FF_Area2',1] <- FFs_new[2]
    out2[i,'FF_Area1',2] <- FFs_global[1]; out2[i,'FF_Area2',2] <- FFs_global[2]
    
    opt0 <- optim_loop(FFs_new,i = NA) ## already specified
    opt_temp <- opt0$opt_temp; tmp0 <- opt0$tmp0; tmp <- opt0$tmp
    
    out2[i,'estRbar',1] <- opt_temp$par[1];  out2[i,'estRprop',1] <- opt_temp$par[2];
    
    # alpha = sum(tmp0$SBPR)*(1-mean(h))/(4*mean(h))
    # beta = (5*mean(h)-1)/(4*mean(h)*R0_global)
    # req <- max(0.001, (sum(tmp$SBPR) - alpha)/(beta*sum(tmp$SBPR))) ## a la SS
    out2[i,'estRbar',2] <- eq3d(SBPR_0 = sum(tmp0$SBPR), steep = mean(h),
                                r0 = R0_global, SBPR_F = sum(tmp$SBPR))
    
    # out2[i,'estRbar',2] <- R0_global; 
    out2[i,'estRprop',2] <- Rprop_input
    
    ## derived quants at optimized value
    yields <- as.numeric(getYield(passR = out2[i,'estRbar',1], passRprop =   out2[i,'estRprop',1], YPR_F = tmp$YPR))
    out2[i,'Yield_A1',1] <- yields[1];  out2[i,'Yield_A2',1] <- yields[2];
    
    sbs <-getSB(passR = out2[i,'estRbar',1], passRprop = out2[i,'estRprop',1], SBPR_F = tmp$SBPR)
    out2[i,'SB_A1',1] <-  as.numeric(sbs[1]);  out2[i,'SB_A2',1] <-  as.numeric(sbs[2]);
    
    sb0 <- getSB(passR = R0_global, passRprop = Rprop_input, SBPR_F = tmp0$SBPR)
    # sb0 <- getSB(passR = R0_global, passRprop = out2[i,'estRprop',1], SBPR_F = tmp0$SBPR)
    # sb0 <- getSB(passR = out2[i,'estRbar',1], passRprop = out2[i,'estRprop',1], SBPR_F = tmp0$SBPR)
    out2[i,'SB0_A1',1] <-  as.numeric(sb0[1]);  out2[i,'SB0_A2',1] <-  as.numeric(sb0[2]);
    
    rexp <- as.numeric(getExpR( SB_F =sbs, SB_0 =sb0, meth = 1))
    out2[i,'expR_A1',1] <- rexp[1];  out2[i,'expR_A2',1] <- rexp[2];
    
    obsr <- as.numeric(out2[i,'estRbar',1]*c(out2[i,'estRprop',1],1-out2[i,'estRprop',1]))
    out2[i,'obsR_A1',1] <- obsr[1];  out2[i,'obsR_A2',1] <- obsr[2];
    rm(opt0)
    
    ## derived quants at global value ("current method")
    yields <- as.numeric(getYield(passR = out2[i,'estRbar',2], passRprop =   out2[i,'estRprop',2], YPR_F = tmp$YPR))
    out2[i,'Yield_A1',2] <- yields[1];  out2[i,'Yield_A2',2] <- yields[2];
    
    sbs <-getSB(passR = out2[i,'estRbar',2], passRprop = out2[i,'estRprop',2], SBPR_F = tmp$SBPR)
    out2[i,'SB_A1',2] <-  as.numeric(sbs[1]);  out2[i,'SB_A2',2] <-  as.numeric(sbs[2]);
    
    sb0 <- getSB(passR = R0_global, passRprop = out2[i,'estRprop',2], SBPR_F = tmp0$SBPR)
    out2[i,'SB0_A1',2] <-  as.numeric(sb0[1]);  out2[i,'SB0_A2',2] <-  as.numeric(sb0[2]);
    
    rexp <- as.numeric(getExpR(SB_F = data.frame(sbs), SB_0 =data.frame(sb0), meth = 2)) ## one value, global rec
    out2[i,'expR_A1',2] <- rexp*out2[i,'estRprop',2];  out2[i,'expR_A2',2] <- rexp*(1-out2[i,'estRprop',2]);
    
    # rexp <- as.numeric(getExpR(passR = out2[i,'estRbar',2], passRprop =out2[i,'estRprop',2],
    #                            SB_F = data.frame(sbs), SB_0 =data.frame(sb0)))
    # out2[i,'expR_A1',2] <- rexp[1];  out2[i,'expR_A2',2] <- rexp[2];
    
    obsr <- as.numeric(out2[i,'estRbar',2]*c( out2[i,'estRprop',2],1-out2[i,'estRprop',2]))
    out2[i,'obsR_A1',2] <- obsr[1];  out2[i,'obsR_A2',2] <- obsr[2];
    
    out2[i,'tyield',1] <- out2[i,'Yield_A1',1]+ out2[i,'Yield_A2',1]
    out2[i,'tyield',2] <- out2[i,'Yield_A1',2]+ out2[i,'Yield_A2',2]
    rm(tmp)
  }
  return(out2)
}


## calls from global environment to optimize
getMSY <- function(){
 
  # https://stackoverflow.com/questions/57173162/function-for-uniroot-that-has-two-parameters-that-need-to-be-run-across-a-vector
  ## the example above actually has 3 pars and he optimizes over 2 known vectors
  ## the mapply will return the best F value given proportion
  ## inside dfx.dxSYS_new we run optim and use passed R, Rprop
  ## need a second version of this which uses global R, rprop
  FpropVec <- seq(0.05,0.99,0.01) ##proportions of F in Area 1
  fbest_new <-
    mapply(
      function(Fv_prop)
        uniroot(f = dfx.dxSYS_new, 
                interval = c(0,1e3), ## higher if continuous, 1 instF
                Fv_prop = Fv_prop)[1],
      FpropVec)
  cat('performed 2d optimization (new method) \n')
  fbest_global  <-
    mapply(
      function(Fv_prop)
        uniroot(f = dfx.dxSYS_global, 
                interval =c(0.01,100), 
                Fv_prop = Fv_prop)[1],
      FpropVec)
  cat('performed global optimization (old method) \n')
  
  propmsy <- data.frame('Fprop' = FpropVec,
                        'FMSY_new' = matrix(unlist(fbest_new)),
                        'FMSY_global' = matrix(unlist(fbest_global)))  
  return(propmsy)
}


# https://stackoverflow.com/questions/32600722/uniroot-in-r-when-there-are-two-unknowns
## we have two unknowns, similar to R setup, which are F input and Fprop
## First we take integration of our function wrt x (call this g(a))
dfx.dxSYS_global <- function(Fv_test, Fv_prop){
  ## Fv_prop indicates proportion of Fv_test applied in A1
  opt0 <- optim_loop(FFs=c((Fv_test-0.001)*(Fv_prop), (Fv_test-0.001)*(1-Fv_prop)), i = NA)
  opt_temp <- opt0$opt_temp; tmp0 <- opt0$tmp0; tmp <- opt0$tmp
  req_global <- eq3d(SBPR_0 = sum(tmp0$SBPR), 
                     steep = mean(h),
                     r0 = R0_global, 
                     SBPR_F = sum(tmp$SBPR))
  
  # yields <- getYield(passR = R0_global, passRprop =  Rprop_input, YPR_F = tmp$YPR)
  yields <- getYield(passR = req_global, passRprop =  Rprop_input, YPR_F = tmp$YPR)
  y1 <- yields$Yield_A1+yields$Yield_A2
  # cat(y1,'\n')
  opt0 <- optim_loop(FFs=c((Fv_test+0.001)*(Fv_prop), (Fv_test+0.001)*(1-Fv_prop)), i = NA)
  opt_temp <- opt0$opt_temp; tmp0 <- opt0$tmp0; tmp <- opt0$tmp
  req_global <- eq3d(
    SBPR_0 = sum(tmp0$SBPR),
    steep = mean(h),
    r0 = R0_global,
    SBPR_F = sum(tmp$SBPR)
  )
  # yields <- getYield(passR = R0_global, passRprop =  Rprop_input, YPR_F = tmp$YPR)
  yields <- getYield(passR = req_global, passRprop =  Rprop_input, YPR_F = tmp$YPR)
  y2 <- yields$Yield_A1+yields$Yield_A2
  # cat(y2,'\n')
  # appx <- (round(y2,1)-round(y1,1))/0.002 #0.002 is total X delta; we are using system yield
  appx <- (y2-y1)/(0.002) #0.002 is total X delta; we are using system yield
  # cat(Fv_test,Fv_prop,appx,'\n')
  return(appx)
}

dfx.dxSYS_new <- function(Fv_test, Fv_prop){

  ## Fv_prop indicates proportion of Fv_test applied in A1
  opt0 <- optim_loop(FFs=c((Fv_test-0.001)*(Fv_prop), (Fv_test-0.001)*(1-Fv_prop)), i = NA)
  opt_temp <- opt0$opt_temp; tmp0 <- opt0$tmp0; tmp <- opt0$tmp
  yields <- getYield(passR = opt_temp$par[1], passRprop =  opt_temp$par[2], YPR_F = tmp$YPR)
  y1 <- yields$Yield_A1+yields$Yield_A2
  # cat(y1,'\n')
  opt0 <- optim_loop(FFs=c((Fv_test+0.001)*(Fv_prop), (Fv_test+0.001)*(1-Fv_prop)), i = NA)
  opt_temp <- opt0$opt_temp; tmp0 <- opt0$tmp0; tmp <- opt0$tmp
  yields <- getYield(passR =opt_temp$par[1], passRprop =   opt_temp$par[2], YPR_F = tmp$YPR)
  y2 <- yields$Yield_A1+yields$Yield_A2
  # cat(y2,'\n')
  # appx <- (round(y2,1)-round(y1,1))/0.002 #0.002 is total X delta; we are using system yield
  
  appx <- (y2-y1)/0.002 #0.002 is total X delta; we are using system yield
  cat("Fv_test",Fv_test,"Fv_prop",Fv_prop,appx,'\n')
  return(appx)
}

logistic <- function(a,a50,a95){
  val <- 1/(1+exp(-log(19)*((a-a50)/(a95-a50))))
  return(val)
}

bh <- function(h, prop, r0, b0, bcurr, narea = 2, method){
  rec = NULL
  ## global method - use summed biomass
  ## for returning purposes just use global prop
  if(method == 2){
    num <- 4*mean(h)*r0*sum(bcurr)/sum(b0)
    denom1 <- sum(bcurr)/sum(b0)*(5*mean(h)-1)
    denom2 <- (1-mean(h))
    rec = num/(denom1+denom2)
  }else{
    for(i in 1:narea){
      if(i == 1){
        num <- prop*4*h[[i]]*r0*bcurr[[i]]/b0[[i]]
      } else{
        num <- (1-prop)*4*h[[i]]*r0*bcurr[[i]]/b0[[i]]
      }
      denom1 <- bcurr[[i]]/b0[[i]]*(5*h[[i]]-1)
      denom2 <- (1-h[[i]])
      rec[i] = num/(denom1+denom2)
    } ## end area loop
  } #end proposed method
  # cat(rec,"\n")
  return(rec)
}
getSB <- function(passR, passRprop, SBPR_F){
  SBPR_F_A1 <- sum(SBPR_F[1,,1],SBPR_F[1,,2])
  SBPR_F_A2 <- sum(SBPR_F[2,,1],SBPR_F[2,,2])
  SB_A1 <- SBPR_F_A1*passR*passRprop
  SB_A2 <- SBPR_F_A2*passR*(1-passRprop)
  return(list('SB_A1'=SB_A1,"SB_A2"=SB_A2))
}
getYield <- function(passR, passRprop, YPR_F){
  ## yield in Area 1 is in first row of each array slice
  YPR_F_A1 <- sum(YPR_F[1,,1],YPR_F[1,,2])
  YPR_F_A2 <- sum(YPR_F[2,,1],YPR_F[2,,2])
  Yield_A1 <- YPR_F_A1*passR*passRprop
  Yield_A2 <-  YPR_F_A2*passR*(1-passRprop)
  return(list('Yield_A1'=Yield_A1,"Yield_A2"=Yield_A2))
}
getExpR <- function(SB_F, SB_0, meth ){
  ## not sure if this should pass sum for global vs local
  Rexp <- bh(h, prop = Rprop_input, r0 = R0_global, b0 = SB_0, bcurr = SB_F, method = meth)
  return(Rexp)
}

optimFunc <- function(par,SBPR_0,SBPR_F){
  # cat(par,"\n")
  passR <- par[1]; passRprop <- par[2]
  ## get sbprf and sbpr0 given pars
  ## the sbprF changes with F 
  # sb_0 <- getSB(passR ,passRprop, SBPR_0)
  # sb_0 <- getSB(R0_global ,passRprop, SBPR_0)
  sb_0 <- getSB(R0_global, Rprop_input, SBPR_0)
  sb_F <- getSB(passR ,passRprop, SBPR_F)
  Rexp <- getExpR(SB_F=sb_F, SB_0=sb_0, meth = 1) ## bh vals given pars, als use proposed method
  obsR <- passR*c(passRprop,1-passRprop) ## raw rec given rglobal and prop
  obj <- sum((obsR - Rexp)^2)
  # obj <- ifelse(abs(obj) == Inf,'')
  return(obj)
}
## set up readable data frame with movement, selex, wtage, maturity inputs for each area
## each par will be of length 2, representing the two areas
## pStay indicates the residency rate for ages 10+; linear to this between age 1-11 
vals <- c('age','proportion_stay','weight','maturity',
          'fishery_selectivity','mortality') ## things to enter into data frame




# 
# plot(NPR)
# sum(YPR[,,])

