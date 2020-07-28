abSRR <- function(alpha, beta, SPR_temp){
  ## should give you R0 when input B0
  # return(list("R_equil" = alpha*biomass/(beta+biomass))) 
  # return(list("R_equil" = biomass/(alpha + beta*biomass))) ## textbook version
  return(list("R_equil" = max(0,(SPR_temp - alpha)/(beta*SPR_temp))))
  
}
