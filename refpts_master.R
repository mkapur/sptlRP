## Revamp of FISH 559 HW3 for Spatial Refpts Paper
## M S Kapur 2020 Summer kapurm@uw.edu

rm(list = ls())
options(scipen = 0)
require(ggplot2, quietly = T)
require(reshape)
require(dplyr, quietly = T)
require(here)
require(ggsidekick)
require(stats4)
## settings
narea = 3
nages = 21
steep = 0.5

## load functions & initialize OM
lapply(list.files(here("R"), full.names = TRUE), source)

## preload SB0
SB0_i <-  data.frame()
for(m in c(1:3)){
  SB0_im <- getSB0(eq_method = c('STD','STB','STB')[m])
  # SB0_i[m,1] <- 
  SB0_i[m,1:3] <- SB0_im
}
row.names(SB0_i) <- c('STD','TIME','STB')















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



## Optimization based upon equilibrium method (6 scenarios)
df2 <- data.frame(
  expand.grid('Area' = 1:3,
              'Eq_Method' = c('STD','TIME','STB'),
              'F_Method' = c('Fmsy_System','Fmsy_Config')),
  'FMSY' = NA,
  'F_POP' = NA,
  'MSY' = NA,
  'BMSY' = NA,
  'B0' = NA)


for(e in 1:length(c('STD','TIME','STB'))){
  cat(c('STD','TIME','STB')[e],"\n")

    ## find optimal F vector with various methods
  ## FMSY_SYSTEM
  FVTEMP <-    as.numeric(uniroot(f = dfx.dxSYS,  h = steep, eq_method = c('STD','TIME','STB')[e],
                                  interval = c(0.02,1))[1])
  cat("-SYSTEM","\n")
  df2$FMSY[df2$F_Method == 'Fmsy_System' & df2$Eq_Method == c('STD','TIME','STB')[e]] <- rep(FVTEMP,narea)
  df2$MSY[df2$F_Method == 'Fmsy_System' & df2$Eq_Method ==  c('STD','TIME','STB')[e]] <-  
    masterFunc(SRR = 1, Fv = rep(FVTEMP,narea), eq_method = c('STD','TIME','STB')[e])$yield
  df2$BMSY[df2$F_Method == 'Fmsy_System' & df2$Eq_Method == c('STD','TIME','STB')[e]] <- 
    masterFunc(SRR = 1, Fv = rep(FVTEMP,narea), eq_method = c('STD','TIME','STB')[e])$spawnbio
  
  ## get population-level F
  N_ai_temp = doNage(Fv = rep(FVTEMP,narea), eq_method = c('STD','TIME','STB')[e])[,1:3]
  df2$F_POP[df2$F_Method == 'Fmsy_System' & df2$Eq_Method == c('STD','TIME','STB')[e]] <- 
    langsF(M = 0.15, N_ai = N_ai_temp, Fv = rep(FVTEMP,narea), p_i = c(0.25,0.25,0.5))
  rm(FVTEMP)
  ## FMSY_CONFIG
  FVTEMP <-  coef(mle(minFunc, 
                      start = list(F1 = 0.025, F2 = 0.025, F3 = 0.025),
                      method = "L-BFGS-B",
                      fixed = list(e = e), ## subsetting eq method
                      lower = c(0.02, 0.02,0.02), upper = c(1,1,1)))[1:3]
  cat("--CONFIG","\n")
  df2$FMSY[df2$F_Method == 'Fmsy_Config' & df2$Eq_Method == c('STD','TIME','STB')[e]] <- FVTEMP
  df2$MSY[df2$F_Method == 'Fmsy_Config'& df2$Eq_Method == c('STD','TIME','STB')[e]] <-  
    masterFunc(SRR = 1, Fv = FVTEMP, eq_method = c('STD','TIME','STB')[e])$yield
  df2$BMSY[df2$F_Method == 'Fmsy_Config'& df2$Eq_Method == c('STD','TIME','STB')[e]] <- 
    masterFunc(SRR = 1, Fv = FVTEMP, eq_method = c('STD','TIME','STB')[e])$spawnbio
  ## get population-level F
  N_ai_temp = doNage(Fv = FVTEMP, eq_method = c('STD','TIME','STB')[e])[,1:3]
  df2$F_POP[df2$F_Method == 'Fmsy_Config' & df2$Eq_Method == c('STD','TIME','STB')[e]] <- 
    langsF(M = 0.15, N_ai = N_ai_temp, Fv = FVTEMP, p_i = c(0.25,0.25,0.5))
  
  unfishedB <- apply(doNage(eq_method = c('STD','TIME','STB')[e])[,7:9],2,sum)
  
  df2$B0[df2$Area == 1 & df2$Eq_Method == c('STD','TIME','STB')[e]] <- unfishedB[1]
  df2$B0[df2$Area == 2 & df2$Eq_Method == c('STD','TIME','STB')[e]] <- unfishedB[2]
  df2$B0[df2$Area == 3 & df2$Eq_Method == c('STD','TIME','STB')[e]] <- unfishedB[3]
  
  
}

df2 %>%
  ggplot(., aes(x = FMSY, y = F_POP, fill = factor(Eq_Method))) +
  geom_point() +
  theme_sleek() + scale_fill_grey() +
  labs(x= expression("F"[MSY]),y = expression("F"[Pop]), fill = 'Eq_Method') +
  facet_wrap(~F_Method)

df2 %>%
  ggplot(., aes(x = Eq_Method, y = FMSY, fill = factor(Area))) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_sleek() + scale_fill_grey() +
  labs(x= 'Equilibrium Method',y = expression("F"[MSY]), fill = 'Area') +
  facet_wrap(~F_Method)
ggsave(last_plot(), 
       file = here('figs','FMSY_scenarios.png'),
       width = 6, height = 4, unit = 'in', dpi = 520)


df2 %>%
  ggplot(., aes(x = Eq_Method, y = B0, fill = factor(Area))) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_sleek() +   theme(legend.position = c(0.9,0.8)) + scale_fill_grey() +
  labs(x= 'Equilibrium Method',y = expression('Unfished Biomass '~B[0]), fill = 'Area')
ggsave(last_plot(), 
       file = here('figs','B0_Method.png'),
       width = 6, height = 4, unit = 'in', dpi = 520)
## single F, maximize system yield
# df2$FMSY[df2$F_Method == 'Fmsy_System' & df2$Eq_Method == 'STD'] <- as.numeric(uniroot(f = dfx.dxSYS,  h = steep, interval = c(0.02,1))[1])
# df2$MSY[df2$F_Method == 'Fmsy_System' & df2$Eq_Method == 'STD'] <-  masterFunc(SRR = 1, Fv = df2$FMSY[df2$F_Method == 'Fmsy_System'])$yield
# df2$BMSY[df2$F_Method == 'Fmsy_System' & df2$Eq_Method == 'STD'] <- masterFunc(SRR = 1, Fv = df2$FMSY[df2$F_Method == 'Fmsy_System'])$spawnbio
# 
# ## unique Fs ('config'), maximize system yield
# df2$FMSY[df2$F_Method == 'Fmsy_Config'] <- coef(mle(minFunc, start = list(F1 = 0.025, F2 = 0.025, F3 = 0.025),
#                                                   method = "L-BFGS-B",
#                                                   lower = c(0.02, 0.02,0.02), upper = c(1,1,1)))




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

