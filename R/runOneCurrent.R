run_one_current <- function(Fv_i = rep(0,narea), 
                            rec_level_idx = 1, 
                            movemat = X_ija ){
  
  ## Get NAA using movement. Input movemat_NULL to turn movement OFF (smooth curve)
  ## applying system-wide F
  
  R0 <- R0_list[[rec_level_idx]]
  rlevelUse <- R0
  ## define virgin biomass
  SB0 <- doNage( Fv = rep(0,narea), 
                 X = movemat,
                 refR = rlevelUse)$SB_total
  
  ## get values at present Fv
  curr <- doNage( Fv = Fv_i, 
                  X = movemat,
                  refR = rlevelUse)
  
  # calc SPB/R and Yield/R
  SB_R <- curr$SB_total/sum(rlevelUse)
  Yield_R <- curr$Yield_total/sum(rlevelUse)
  
  #call Equil_Spawn_Recr_Fxn to get B_equil and R_equil from SPB/R and SR parms
  currEq <- Equil_Spawn_Recr_Fxn(steepness = steep[1], SSB_virgin = SB0, 
                                 Recr_virgin = sum(R0), SPR_temp = SB_R)## L17247 ON TPL
  
  if(currEq$R_equil > sum(R0)) currEq$R_equil <- sum(R0) ## could alternatively use flattop BH
  Outs = NULL
  
  Outs$Yield <- Yield_R * currEq$R_equil
  Outs$B <- SB_R* currEq$R_equil ## the same as currEq$B_equil
  Outs$R_ESUMB <- currEq$R_equil ## expected recruits given sum biomass in area
  Outs$SBEQTOTAL2 <- currEq$B_equil ## expected recruits given sum biomass in area
  
  return(Outs)
  
}
