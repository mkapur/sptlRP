abSRR <- function(alpha, beta, biomass){
  ## should give you R0 when input B0
  # return(list("R_equil" = alpha*biomass/(beta+biomass))) 
  return(list("R_equil" = biomass/(alpha + beta*biomass))) 
  
}
