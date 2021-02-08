

## sanity checking functions ----
png(here('figs','nominal_srr.png'))
plot(cbind(1,bh(h=steep, prop = Rprop_input, r0 = R0_global, b0 = 15, bcurr = 1)),
     ylim = c(0,3),xlim = c(0,100), xlab = 'SBcurrent', ylab = 'BH Rec')
for(i in 1:100)points(cbind(i,bh(h=steep, prop = Rprop_input, r0 = R0_global, b0 = 15, bcurr = i)))
dev.off()


par(mfrow = c(2,2))
tmp <- doPR(dat, FF = c(0,0)) ## defaults, no fishing
all(round(colSums(tmp$NPR[,,1])) == round(colSums(tmp$NPR[,,2]))) ## net exchange should be conserved
plot(tmp$NPR[1,,1], main = 'A1 SRC');points(tmp$NPR[2,,1],col = 'blue') ## should look reasonable
legend('topright', pch =1, legend = c('stay in a1', 'end up in a2'), col = c('black','blue'))
plot(tmp$NPR[1,,2], main = 'A2 SRC');points(tmp$NPR[2,,2],col = 'blue') ## should look reasonable

## test run with no movement
dat <- makeDat(wa = c(5,5),
               fec_a50 = c(6,6),
               fec_a95 = c(12,12),
               slx_a50 = c(9,9),
               slx_a95 = c(13,13),
               pStay = c(1,1))
tmp <- doPR(dat, FF = c(0,0)) ## defaults, no fishing
all(round(colSums(tmp$NPR[,,1])) == round(colSums(tmp$NPR[,,2]))) ## net exchange should be conserved
plot(tmp$NPR[1,,1], main = 'A1 SRC');points(tmp$NPR[2,,1],col = 'blue') 
legend('topright', pch =1, legend = c('stay in a1', 'end up in a2'), col = c('black','blue'))
plot(tmp$NPR[1,,2], main = 'A2 SRC');points(tmp$NPR[2,,2],col = 'blue') 
dev.off()

## test with movement & some Fs
par(mfrow = c(2,2))
dat <- makeDat(wa = c(5,5),
               fec_a50 = c(6,6),
               fec_a95 = c(12,12),
               slx_a50 = c(9,9),
               slx_a95 = c(13,13),
               pStay = c(0.9,0.6))
tmp <- doPR(dat, FF = c(0.2,0.2)) ## defaults, equally low fishing
all(round(colSums(tmp$NPR[,,1])) == round(colSums(tmp$NPR[,,2]))) ## net exchange should be conserved
plot(tmp$NPR[1,,1],ylim = c(0,1), main = 'A1 SRC');points(tmp$NPR[2,,1],col = 'blue') ## A2 ()
legend('topright', pch =1, legend = c('stay in a1', 'end up in a2'), col = c('black','blue'))
plot(tmp$NPR[1,,2],ylim = c(0,1), main = 'A2 SRC');points(tmp$NPR[2,,2],col = 'blue') 

dat <- makeDat(wa = c(5,5),
               fec_a50 = c(6,6),
               fec_a95 = c(12,12),
               slx_a50 = c(9,9),
               slx_a95 = c(13,13),
               pStay = c(0.9,0.6))
tmp <- doPR(dat, FF = c(0.8,0.8)) ## defaults, no fishing
all(round(colSums(tmp$NPR[,,1])) == round(colSums(tmp$NPR[,,2]))) ## net exchange should be conserved
plot(tmp$NPR[1,,1],ylim = c(0,1), main = 'A1 SRC');points(tmp$NPR[2,,1],col = 'blue') 
legend('topright', pch =1, legend = c('stay in a1', 'end up in a2'), col = c('black','blue'))
plot(tmp$NPR[1,,2], ylim = c(0,1),main = 'A2 SRC');points(tmp$NPR[2,,2],col = 'blue') 


