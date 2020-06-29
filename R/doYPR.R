doYPR <-function( Fv= rep(0.2,narea), M = 0.15, ...) {
  ypr_sa <- array(NA, dim = c(1,2,narea)) ## each sex by area
  ypr_a <- matrix(NA, nrow = 1, ncol = narea) ## each  area
  ypr  <- NA  ## total
  for(i in 1:narea){
    for(s in 1:2){ ## loop sexes
      wt <- dat[,s+2,i] ## cols 2 & 3
      nzmat <-  doNage(s = s, Fv = Fv, eq_method = eq_method) ## expects 3 FV values
      n_a <- nzmat[,i]
      z_a <- nzmat[,i+3]
      ## Baranov
      ypr_sa[1,s,i] <- sum(wt*(( dat[,s+4,i]*Fv[i])/z_a)*n_a*(1-exp(-z_a)))
    } ## end sexes
    ypr_a[i] <- sum(ypr_sa[1,,i])
  } ## end areas
  ypr <- sum(ypr_a)
  return(list(ypr_sa,ypr_a,ypr))
}

doYield <- function(ypr, R){
  yield_a <- yield_tot <- NULL
  for(i in 1:narea){
    yield_a[i] <- ypr[i]*R[i]
  }
  yield_tot <- sum(yield_a)
  return(list(yield_a,yield_tot))
} 
