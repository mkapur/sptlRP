
## put it all together
masterFunc <-
  function(
    Fv = rep(0.15,narea),
    eq_method = c('STD','TIME','STB')[1],
    SRR = 1,
    h = steep,
    gam = 1,
    R0 = 1,
    S0 = 0.6739975) {
    
    ypr_a <- doYPR(Fv = Fv, eq_method = eq_method)[[2]] ## second element is per area
    rec_sb <-  getEqRec( SRR = SRR, h = steep, Fv = Fv,gam = gam, R0 = R0,S0 = S0, eq_method = eq_method)
    
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
