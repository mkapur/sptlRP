## Revamp of FISH 559 HW3 for Spatial Refpts Paper
## M S Kapur 2020 Summer kapurm@uw.edu

rm(list = ls())
options(scipen = 0)
require(ggplot2, quietly = T)
require(reshape)
require(dplyr, quietly = T)
require(here)
require(ggsidekick)

## settings
narea = 3
nages = 21
steep = 0.5

## load functions & initialize OM
lapply(list.files(here("R"), full.names = TRUE), source)




SB0_i <-  data.frame()
for(m in c(1:3)){
  SB0_im <- getSB0(eq_method = c('STD','STB','STB')[m])
  # SB0_i[m,1] <- 
  SB0_i[m,1:3] <- SB0_im
}
row.names(SB0_i) <- c('STD','TIME','STB')


## movement matrix (simple) ----

## function to return population level F given various Fv, p_i
## Eq 7 in CJFAS pub, but not subtracting M
langsZ <- function(M, N_ai, Fv,p_i){
  num1 <-  p_i[1]*N_ai[a-1,1]*exp(-Fv[1]-M)
  num2 <-  p_i[2]*N_ai[a-1,2]*exp(-Fv[2]-M)
  num3 <-  p_i[3]*N_ai[a-1,3]*exp(-Fv[3]-M)
  denom <- sum(p_i*N_ai[a-1,])
  return(-log(sum(num1,num2,num3)/denom))
}












## this needs to be spatial
## find where F is minimized and S ~ 0, bisection
# bisect <- function(Fmin = 0, Fmax = 1){
#   for(b in 1:1000){
#     Fv_testM <- (Fmin + Fmax)/2 ## update
#     sbio_temp <- masterFunc(SRR = s, Fv = Fv_testM)$spawnbio
#     if(round(sbio_temp,4) == 0 & (Fmax - Fmin) > 0.0002){ return(Fv_testM)
#     } else if(round(sbio_temp,4) > 0) { Fmin <- Fv_testM 
#     } else if(round(sbio_temp,4) < 0) { Fmax <- Fv_testM }
#   }
#   print('max iter')
# }

## brute force - try various configs & save delta to search
# Fv_config <- expand.grid(seq(0.02,1,0.1),seq(0.02,1,0.1),seq(0.02,1,0.1))
# 
# # https://stackoverflow.com/questions/50978973/looping-uniroot-on-two-arguments-with-sapply
# 
# dfx.dxCONFIGA <- function(k, h = steep){
#   appx = NULL
#   # for(k in 1:nrow(Fv_config)){
#     y1 <- masterFunc(SRR = 1, h = steep, Fv = as.numeric(Fv_config[k,]-0.001))$yield
#     y2 <- masterFunc(SRR = 1, h = steep, Fv = as.numeric(Fv_config[k,]+0.001))$yield
#     appx[k] <- (sum(y2)-sum(y1))/(0.002) #0.002 is total delta; we are using system yield
#   # } # end configs
#   return(appx) ## want to minimize all 3
# }



## uniroot/find FMSY ----
df2 <- data.frame(
  expand.grid('Area' = 1:3,
              'Eq_Method' = c('STD','TIME','STB'),
  'F_Method' = c('Fmsy_System','Fmsy_Config')),
                 'FMSY' = NA,
                 'MSY' = NA,
                 'BMSY' = NA,
  'B0' = NA)

## single F, maximize system yield
df2$FMSY[df2$F_Method == 'Fmsy_System' & df2$Eq_Method == 'STD'] <- as.numeric(uniroot(f = dfx.dxSYS,  h = steep, interval = c(0.02,1))[1])
df2$MSY[df2$F_Method == 'Fmsy_System' & df2$Eq_Method == 'STD'] <-  masterFunc(SRR = 1, Fv = df2$FMSY[df2$F_Method == 'Fmsy_System'])$yield
df2$BMSY[df2$F_Method == 'Fmsy_System' & df2$Eq_Method == 'STD'] <- masterFunc(SRR = 1, Fv = df2$FMSY[df2$F_Method == 'Fmsy_System'])$spawnbio

## unique Fs ('config'), maximize system yield
df2$FMSY[df2$F_Method == 'Fmsy_Config'] <- coef(mle(minFunc, start = list(F1 = 0.025, F2 = 0.025, F3 = 0.025),
                                                  method = "L-BFGS-B",
                                                  lower = c(0.02, 0.02,0.02), upper = c(1,1,1)))
df2$MSY[df2$F_Method == 'Fmsy_Config'] <-  masterFunc(SRR = 1, Fv = df2$FMSY[df2$F_Method == 'Fmsy_Config'])$yield
df2$BMSY[df2$F_Method == 'Fmsy_Config'] <- masterFunc(SRR = 1, Fv = df2$FMSY[df2$F_Method == 'Fmsy_Config'])$spawnbio
unfishedB <- apply(doNage()[,7:9],2,sum)
df2$B0[df2$Area == 1 ] <- unfishedB[1];df2$B0[df2$Area == 2 ] <- unfishedB[2];df2$B0[df2$Area == 3 ] <- unfishedB[3]

## change catchability assumption (Langseth & Schueller 2016)
## this is the Sampson & Scott 2011 Method; see Maury et al...





## MSY by approach

##BMsy by approach
BMSY_sys <-
BMSY_area <- masterFunc(SRR = s, Fv = rep(FMSY_area,narea))$spawnbio
BMSY_config <- masterFunc(SRR = s, Fv = FMSY_config)$spawnbio


Fcrash <- bisect()
q3b[s,] <- c( c('BevHolt','Ricker','Pella-T, gamma = 1')[s], round(FMSY,4),
              round(MSY,4), round(Fcrash,4))

