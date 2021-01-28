## Rzation of excel file made with AEP on 19 Jan 2021
## kapurm@Uw.edu


## start values
R0_input = 4
h <- 0.75 ## will become parameter
prop_RA1 <- 0.65 ## will become parameter; proportion recruits assigned to A1

narea <- 2
nage <- 3

mort <- exp(-0.2) ## this is why AEP used 0.8

## movement; not age-specific for now
XIJ <- matrix(NA, nrow = narea, ncol = narea)
XIJ[1,1] <- 0.9 ## from one, to one
XIJ[1,2] <- 1-XIJ[1,1]
XIJ[2,1] <- 0.1 ## from one, to one
XIJ[2,2] <- 1-XIJ[2,1] 


## a matrix which tracks the proportion of original numbers which reside in each area
## this includes the calculation of how many area 1 age-2s are spawned in area 1 vs area 2.
## we assume that 
na1 <- na2 <- matrix(NA, ncol = nage, nrow = narea)
na1[,1] <- c(1,0) ## 1 recruit in a1 from a1, 0 in a1 from a2
na2[,1] <- c(0,1) ## 1 recruit in a2 from a2, 0 in a2 from a1

for(age in 2:nage){
  for(area in 1:narea){
    na1[area,age] <- mort*na1[area,age-1]*XIJ[narea,1]
    na2[area,age] <- mort*na1[area,age-1]*XIJ[narea,2]
    
  }

}