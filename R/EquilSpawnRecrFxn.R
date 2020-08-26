Equil_Spawn_Recr_Fxn <- function(steepness, SSB_virgin, Recr_virgin, SPR_temp){
  # SS_Label_44.1.3  Beverton-Holt
    alpha = 4.0 * steepness*Recr_virgin / (5*steepness-1)
    beta = (SSB_virgin*(1-steepness)) / (5*steepness-1)
    B_equil = alpha * SPR_temp - beta
    B_equil = max(B_equil,0.0001)
    R_equil=(4*steepness*Recr_virgin*B_equil) / (SSB_virgin*(1-steepness)+(5*steepness-1)*B_equil)
    return(list('R_equil' = R_equil, "B_equil" = B_equil))
}
