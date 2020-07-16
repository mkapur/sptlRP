rm(list = ls())

## This code will execute the trials and sources functions
## to run the proposed/current approaches

## for running example
require(reshape)
require(dplyr, quietly = T)
require(here)
require(stats4)

## for plotting
require(patchwork)
require(ggsidekick)
require(ggplot2, quietly = T)

## settings ----
narea <- 3
nages <- 21
steep <- rep(0.7,3)
recr_dist <- list(c(1,1,1),
                  c(0.5,0.3,0.2))[[1]] ## global recruits to areas

R0 <- c(420,330,250) ## each area has its own R0
rec_level <- R0 ## I suggest it should be the area-specific R0.
# nominal_dist <- R0/sum(R0)


## load functions & initialize OM
lapply(list.files(here("R"), full.names = TRUE), source)

## Storage
Ftest <- seq(0,1,0.05)
current <- data.frame(Fv = NA, Yield = NA, B = NA)
rick <- data.frame() ## storage for SRR question
maxiter = 101
proposed <- data.frame(Fv = NA, Yield = NA, B = NA)
proposed_i <- array(NA, dim = c(length(Ftest),3,narea), dimnames = list(NULL,c('Fv','Yield',"B"))) ## now for each area
B_eq_i <- R_eq_i <- B_eq_i_INIT <- R_eq_i_INIT <- SB_Ri <- Yield_Ri <- matrix(NA, nrow =length(Ftest), ncol = narea)
radj <- array(NA, dim = c(maxiter,length(Ftest),3)) ## keeping track of convergence

## SB_0i
SB0_i <- doNage( Fv = rep(0,narea), 
                X = X_ija,
                rdist = recr_dist, ## these are set to 1
                refR = rec_level)$SB_i

for(v in 1:length(Ftest)){
  current[v,'Fv'] <- Ftest[v]
  curr <- run_one_current(Fv_i = rep(Ftest[v],narea), rec_level)
  current[v,'Yield'] <- curr$Yield
  current[v,'B'] <- curr$B ## the same as currEq$B_equil
  rick[v,'R_ESUMB'] <- curr$R_ESUMB # expected recruits given sum biomass in area
  rick[v,'SBeqtotal2'] <- curr$SBEQTOTAL2 ## expected recruits given sum biomass in area
  
  for(k in 1:maxiter){
    
    run_one_proposed(Fv = rep(Ftest[v],narea), rec_level
    radj[k,v,i] <- Outs$radj_kvi
  } ## end k
  ## save totals from final iteration
  proposed[v,'Fv'] <- Ftest[v]
  proposed[v,'Yield'] <-   sum(proposed_i[v,'Yield',])
  proposed[v,'B'] <-  sum(proposed_i[v,'B',])
  
  
}


## TRIAL 1
