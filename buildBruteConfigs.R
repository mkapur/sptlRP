maxF <- 0.5
incF <- c(0.01,0.05,0.1)[2]



## bruteYield (systemwide)
Fv_test <- seq(0,maxF,incF)
for(e in c("STD",'TIME','STB')){
  
  brute <- data.frame('FV_sys' = NA, "yield_1" = NA, "yield_2" = NA,"yield_3" = NA,
                      "ypr_1" = NA, "ypr_2" = NA,"ypr_3" = NA)
  
  
  for( v in 1:length(Fv_test)){
    temp <- masterFunc(SRR = 1, h = steep, Fv = rep(Fv_test[v],narea), eq_method = e )
    temp2 <- doYPR(Fv = rep(Fv_test[v],narea))
    brute[v,'FV_sys'] = Fv_test[v]
    for(a in 1:narea) brute[v,a+1] = temp$yield[a]
    for(a in 1:narea) brute[v,a+4] = temp2[[2]][a]
    brute$sysyield[v] <- sum(brute[a,2:4])
    
    
  } ## end rows
  save(brute, file = here('rdata',paste('brute_',e,'.rdata')))
} ## end eq method

## takes about 4 mins with 5k subset
brute_config_STD <- data.frame(expand.grid(Area = 1:3,
                                           FA1 = seq(0,maxFincF),
                                           FA2 =  seq(0,maxFincF), 
                                           FA3 =  seq(0,maxFincF)),
                               "yield_1" = NA, "yield_2" = NA,"yield_3" = NA, sysyield = NA)
# brute_config_STD <- sample_n(brute_config_STD, 5000)
# brute_config_STD <- brute_config_STD[1:5000,] ## xy need to be increasing
for(i in 1:nrow(brute_config_STD)){
  temp <- masterFunc(SRR = 1, 
                     h = steep, 
                     eq_method = 'STD',
                     Fv = with(brute_config_STD[i,], c(FA1,FA2,FA3)))
  # temp2 <- doYPR(Fv = rep(Fv_test[i],narea))
  for(a in 1:narea) brute_config_STD[i,a+4] = temp$yield[a]
  # for(a in 1:narea) brute[i,a+4] = temp2[[2]][a]
  if(i %% 100 == 0) cat(i,"\n")
}
brute_config_STD$IDX <- 1:nrow(brute_config_STD)
brute_config_STD$sysyield = rowSums(brute_config_STD[,5:7])

save(brute_config_STD, file = here("rdata","brute_config_STD.Rdata"))

brute_config_TIME <- data.frame(expand.grid(Area = 1:3,
                                           FA1 = seq(0,maxFincF),
                                           FA2 =  seq(0,maxFincF), 
                                           FA3 =  seq(0,maxFincF)),
                               "yield_1" = NA, "yield_2" = NA,"yield_3" = NA, sysyield = NA)
# brute_config_TIME <- sample_n(brute_config_TIME, 5000)
# brute_config_TIME <- brute_config_TIME[1:5000,] ## xy need to be increasing
for(i in 1:nrow(brute_config_TIME)){
  temp <- masterFunc(SRR = 1, 
                     h = steep, 
                     eq_method = 'TIME',
                     Fv = with(brute_config_TIME[i,], c(FA1,FA2,FA3)))
  # temp2 <- doYPR(Fv = rep(Fv_test[i],narea))
  for(a in 1:narea) brute_config_TIME[i,a+4] = temp$yield[a]
  # for(a in 1:narea) brute[i,a+4] = temp2[[2]][a]
  if(i %% 100 == 0) cat(i,"\n")
}
brute_config_TIME$IDX <- 1:nrow(brute_config_TIME)
brute_config_TIME$sysyield = rowSums(brute_config_TIME[,5:7])
save(brute_config_TIME, file = here("rdata","brute_config_TIME.Rdata"))


brute_config_STB <- data.frame(expand.grid(Area = 1:3,
                                           FA1 = seq(0,maxFincF),
                                           FA2 =  seq(0,maxFincF), 
                                           FA3 =  seq(0,maxFincF)),
                               "yield_1" = NA, "yield_2" = NA,"yield_3" = NA, sysyield = NA)
# brute_config_STB <- sample_n(brute_config_STB, 5000)
# brute_config_STB <- brute_config_STB[1:5000,] ## xy need to be increasing
for(i in 1:nrow(brute_config_STB)){
  temp <- masterFunc(SRR = 1, 
                     h = steep, 
                     eq_method = 'STB',
                     Fv = with(brute_config_STB[i,], c(FA1,FA2,FA3)))
  # temp2 <- doYPR(Fv = rep(Fv_test[i],narea))
  for(a in 1:narea) brute_config_STB[i,a+4] = temp$yield[a]
  # for(a in 1:narea) brute[i,a+4] = temp2[[2]][a]
  if(i %% 100 == 0) cat(i,"\n")
}
brute_config_STB$IDX <- 1:nrow(brute_config_STB)
brute_config_STB$sysyield = rowSums(brute_config_STB[,5:7])
save(brute_config_STB, file = here("rdata","brute_config_STB.Rdata"))
