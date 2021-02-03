## Rzation of excel file made with AEP on 19 Jan 2021
## kapurm@Uw.edu
require(dplyr)
require(ggplot2)
require(patchwork)
require(here)

## start values & settings
R0_input <- 4
h <- 0.75 ## will become parameter
prop_RA1 <- 0.65 ## proportion recruits assigned to A1

passedRbar <- 3.5 ## par
passedRprop <- 0.6 ## par

narea <- 2
nage <- 3

mort <- exp(-0.2) ## this is why AEP used 0.8
FF <-  ## f mort in each area, applied to age 2 only

source('C:/Users/mkapur/Dropbox/UW/sptlRP/R/newFuncs.R', echo=F)

NA12 <- getNA12(nage=nage,narea=narea,XIJ=XIJ,mort=mort,FF=c(0,0)) ## returns orange box from excel (deterministic

## get sbf, returns obsr and sb
## pars are bar, prop
optim(par = c(4,0.6),
      NA_12 = NA12,
      prop_RA1=prop_RA1,
      R0_input=R0_input,
      lower = c(0,0),
      upper = c(1,NA),
      method = "L-BFGS-B",
      fn = optimFunc, hessian = FALSE)


FF_test <- expand.grid(FF1 = seq(0,10,0.05),FF2 = seq(0,1,0.05) )
out <- data.frame()
for(i in 1:nrow(FF_test)){
  
  FF_temp <- as.numeric(c(FF_test[i,]))
  out[i,'FF_Area1'] <- FF_temp[1];   out[i,'FF_Area2'] <- FF_temp[2]; 
  NA12 <- getNA12(nage=nage,
                  narea=narea,
                  XIJ=XIJ,
                  mort=mort,
                  FF=FF_temp) ## returns orange box from excel (deterministic)
 
  ## find pars only need once
  if(i == 1){
  opt_temp <- optim(par = c(4,0.6),
        NA_12 = NA12,
        prop_RA1=prop_RA1,
        R0_input=R0_input,
        lower = c(0,0),
        upper = c(NA,1),
        method = "L-BFGS-B",
        fn = optimFunc, hessian = FALSE)
  }
  ## return exp, obs with optimized values
  opt_vals <- getStuff(par = opt_temp$par,  
           NA12, ## unchanged
           prop_RA1,
           R0_input)
  out[i,'estRbar'] <- opt_temp$par[1];  out[i,'estRprop'] <- opt_temp$par[2];
  
  out[i,'expR_A1'] <- opt_vals$expR[1];  out[i,'expR_A2'] <- opt_vals$expR[2];
  out[i,'obsR_A1'] <- opt_vals$obsR[1];  out[i,'obsR_A2'] <- opt_vals$obsR[2];
  out[i,'SB_A1'] <- opt_vals$SB[1];  out[i,'SB_A2'] <- opt_vals$SB[2];
  out[i,'SB0_A1'] <- opt_vals$SB_0[1];  out[i,'SB0_A2'] <- opt_vals$SB_0[2];
  out[i,'Yield_A1'] <- opt_vals$yield[1];  out[i,'Yield_A2'] <- opt_vals$yield[2]; ## build this
  rm(NA12)
}


out <- subset(out, SB_A1 >0 & SB_A2 >0)
## 2d F vs SSB and Yield plots----
FSB <- out %>%
  select(FF_Area1,FF_Area2,SB_A1,SB_A2) %>%
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,4,5), SB= value) %>%
  select(-variable,-value) %>%
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = SB)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() + 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c() +
  facet_wrap(~Area) +
  labs(x = 'F in Area 1 (age 2)', y = 'F in Area 2 (age 2)', fill = 'SB in area') 

FYIELD <- out %>%
  select(FF_Area1,FF_Area2,Yield_A1,Yield_A2) %>%
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,7,8), yield = value) %>%
  select(-variable,-value) %>% 
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = yield)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() + 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(option = 'magma') +
  facet_wrap(~Area) +
  labs(x = 'F in Area 1 (age 2)', y = 'F in Area 2 (age 2)', fill = 'Yield in area') 

FSB/FYIELD 
ggsave(last_plot(),
       height = 10, width = 8, dpi = 520,
       file = here('figs',paste0(Sys.Date(),"-FvsSBandYield_byArea.png")))

##  F vs Composite SSB and Yield plots ----
FSB <- out %>%
  mutate(SB_Total = SB_A1+SB_A2) %>%
  select(FF_Area1,FF_Area2, SB_Total) %>%
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,4,5), SB= value) %>%
  select(-variable,-value) %>%
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = SB)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c() +
  labs(x = 'F in Area 1 (age 2)', 
       y = 'F in Area 2 (age 2)', 
       fill = 'Total SB') 

FYIELD <- out %>%
  mutate(Yield_Total = Yield_A1+Yield_A2) %>%
  select(FF_Area1,FF_Area2, Yield_Total) %>%  
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,7,8), yield = value) %>%
  select(-variable,-value) %>% 
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = yield)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() + 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(option = 'magma') +
  labs(x = 'F in Area 1 (age 2)', 
       y = 'F in Area 2 (age 2)',
       fill = 'Total Yield') 



FSB|FYIELD
ggsave(last_plot(),
       height = 10, width = 8, dpi = 520,
       file = here('figs',paste0(Sys.Date(),"-FvsSBandYield.png")))
##  Composite F vs Composite SSB and Yield plots ----
FSB <- out %>%
  mutate(SB_Total = SB_A1+SB_A2,
         F_total = FF_Area1+FF_Area2 ) %>%
  select(F_total, SB_Total) %>%
  reshape2::melt(id = c("F_total")) %>%
  mutate(Area = substr(variable,4,5), SB= value) %>%
  select(-variable,-value) %>%
  ggplot(., aes(x = FF_Area1, y = FF_Area2, fill = SB)) +
  geom_tile() +
  coord_equal() +
  ggsidekick::theme_sleek() + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c() +
  labs(x = 'F in Area 1 (age 2)', 
       y = 'F in Area 2 (age 2)', 
       fill = 'Total SB') 

FYIELD <-  out %>%
  select(FF_Area1,FF_Area2, Yield_A1) %>%  
  reshape2::melt(id = c("FF_Area1","FF_Area2")) %>%
  mutate(Area = substr(variable,7,8), yield = value) %>%
  select(-variable,-value,-Area) %>% 
  ggplot(., aes(x = FF_Area1, y = yield, color = FF_Area2, group = FF_Area2)) +
  geom_line(lwd = 1) +
  # geom_point(size = 1) +
  ggsidekick::theme_sleek() + 
  scale_fill_viridis_c(option = 'magma') +
  labs(x = 'F in Area 1', 
       y = 'Yield in Area 1') 

FSB|FYIELD
ggsave(last_plot(),
       height = 10, width = 8, dpi = 520,
       file = here('figs',paste0(Sys.Date(),"-FvsSBandYield.png")))
  
