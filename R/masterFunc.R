
## put it all together
masterFunc <-
  function(
    Fv = rep(0.15,narea),
    SRR = 1,
    h = steep,
    gam = 1,
    R0 = 1,
    S0 = 0.6739975) {
    
    ypr_a <- doYPR(Fv = Fv)[[2]] ## second element is per area
    rec_sb <-  getEqRec( SRR = SRR, h = steep, Fv = Fv,gam = gam, R0 = R0,S0 = S0)
    
    yield <- doYield(ypr_a,rec_sb$rec) ## second object is total
    
    ## results by area
    df <- data.frame(  'Area' = 1:3,
                       "SRR" = c('BevHolt','Ricker','Pella-T')[SRR],
                       "Fmort" = Fv,
                       "rec" = rec_sb$rec,
                       "yield" = yield[[1]],
                       "spawnbio" = rec_sb$spawnbio) #rec[2]*rec[1])    ## eq 7
    
    return(df)
  }