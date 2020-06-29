
## Single F for all three areas
dfx.dxSYS <- function(Fv_test, h = steep, eq_method ){
  y1 <- masterFunc(SRR = 1, h = steep, Fv = rep(Fv_test-0.001,narea), eq_method = eq_method)$yield
  y2 <- masterFunc(SRR = 1, h = steep, Fv = rep(Fv_test+0.001,narea), eq_method = eq_method)$yield
  appx <- (sum(y2)-sum(y1))/(0.002) #0.002 is total X delta; we are using system yield
  return(appx)
}

## systemwide but with separate FV_test
# dfx.dxSYS2 <- function(Fv_testa1, Fv_testa2, h = steep){
#   y1 <- masterFunc(SRR = 1, h = steep, Fv = c(Fv_testa1-0.001,Fv_testa2- 0.001))$yield
#   y2 <- masterFunc(SRR = 1, h = steep, Fv = c(Fv_testa1+0.001,Fv_testa2+ 0.001))$yield
#   appx <- (sum(y2)-sum(y1))/(0.002) #0.002 is total X delta; we are using system yield
#   return(appx)
# }
# 
# dfx.dxAREA <- function(Fv_test, h = steep){
#   y1 <- masterFunc(SRR = 1, h = steep, Fv = rep(Fv_test-0.001,narea))$yield[3]
#   y2 <- masterFunc(SRR = 1, h = steep, Fv = rep(Fv_test+0.001,narea))$yield[3]
#   appx <- (y2-y1)/(0.002) #0.002 is total delta
#   return(appx)
# }


minFunc <- function(F1,F2,F3,e){
  minus <- as.numeric(c(F1 - 0.001, F2 - 0.001, F3 - 0.001))
  plus <- as.numeric(c(F1 +0.001, F2 + 0.001, F3 + 0.001))
  y1 <- masterFunc(SRR = 1, h = steep, Fv = minus, eq_method = c('STD','TIME','STB')[e])$yield
  y2 <- masterFunc(SRR = 1, h = steep, Fv = plus, eq_method = c('STD','TIME','STB')[e])$yield
  appx <- (sum(y2)-sum(y1))/(0.002) ## system yield again
  return(appx)
}
