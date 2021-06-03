

## sanity checking functions ----

## The rate of movement for the base-case scenario is such that the numbers of animals that originally 
## settled in area 2 is about equal between areas 1 and 2 after 25 years [compare with and without F ]. 

## from AEP: Check the numbers-at-age when summed over source area are the same for localized and global recruit and mention that
# 
# dat <- makeDat(wa = c(5,5),
#                fec_a50 = c(6,6),
#                fec_a95 = c(12,12),
#                slx_a50 = c(9,9),
#                slx_a95 = c(13,13),
#                pStay = c(0.9,0.6))
# tmp <- doPR(dat, FF = c(0,0)) ## defaults, no fishing

# plot(tmp$NPR)
# 
# png(here('figs','nominal_srr.png'))
# plot(cbind(1,bh(h=steep, prop = Rprop_input, r0 = R0_global, b0 = 15, bcurr = 1)),
#      ylim = c(0,3),xlim = c(0,100), xlab = 'SBcurrent', ylab = 'BH Rec')
# for(i in 1:100)lines(cbind(i,bh(h=steep, prop = Rprop_input, r0 = R0_global, b0 = 15, bcurr = i)))
# dev.off()


## base case datplot ----
png(here('figs','testdoPR-movement-PANEL.png'), width = 10, height = 7, units = 'in', res = 520)
# load("C:/Users/mkapur/Dropbox/UW/sptlRP/OUTPUT/2021-06-02-h=0.7_0.7-Base-Case/dat.rdata")


layout.matrix <- matrix(c(1:12), ncol = 3, byrow = T)

layout(mat = layout.matrix,
       widths = c(2, 1.75, 2), 
       heights = c(1.75, 2, 2)) 

par(mar = c(0,4,1,1))

#* top row at f=0
tmp <- doPR(dat, FF = c(0,0)) ## defaults, no fishing
all(round(colSums(tmp$NPR[,,1])) == round(colSums(tmp$NPR[,,2]))) ## net exchange should be conserved
plot(tmp$NPR[1,,1], xaxt = 'n', yaxt = 'n', lwd = 2,
     ylim = c(0,1),   xlab = '',type = 'l',     cex.main = 1.5,
     main = as.expression(bquote(Spawned~"in"~"a1,"~F~"="~0)),
     ylab = 'Numbers-per-recruit' );
lines(tmp$NPR[2,,1], lwd = 2, col = 'blue') ## should look reasonable
axis(side = 2, at = seq(0,1,0.2), labels =seq(0,1,0.2) )
text(x = 22, y = 0.95, label = "A", cex = 1.5)

par(mar = c(0,0,1,1))
plot(tmp$NPR[1,,2],type = 'l',
     cex.main = 1.5,
     main = as.expression(bquote(Spawned~"in"~"a2,"~F~"="~0)),
     ylim = c(0,1),
     yaxt = 'n',
     xaxt = 'n',
     lwd = 2,  )
lines(tmp$NPR[2,,2],lwd = 2, col = 'blue') ## should look reasonable
text(x = 22, y = 0.95, label = "B", cex = 1.5)

## total Ns
par(mar = c(0,4,1.2,1))
plot(colSums(tmp$NPR[,,1]),type = 'l',
     main = as.expression(bquote(Total~"Numbers,"~F~"="~0)),
     ylim = c(0,1),
     cex.main = 1.5,
     ylab = 'Numbers-at-Age',
     xaxt = 'n',
     lwd = 2,  )
lines(colSums(tmp$NPR[,,2]),lwd = 2, col = 'blue', type = 'p') ## should look reasonable
legend('topright',lwd = 2, legend = c('Present in a2', 'Present in a1'), col = rev(c('black','blue')))
text(x = 15, y = 0.95, label = "C", cex = 1.5)


#* bottom row at f=fmsy
par( mar = c(4,4,1.2,1))
tmp <- doPR(dat, FF = c(0.98,0.34))
all(round(colSums(tmp$NPR[,,1])) == round(colSums(tmp$NPR[,,2]))) ## net exchange should be conserved
plot(tmp$NPR[1,,1],  yaxt = 'n', lwd = 2,
     cex.main = 1.5,
     ylim = c(0,1),   xlab = 'Age',type = 'l',
     main = as.expression(bquote(Spawned~"in"~"a1,"~F~"="~F[MSY])),
     ylab = 'Numbers-per-recruit' );
lines(tmp$NPR[2,,1], lwd = 2, col = 'blue') ## should look reasonable
axis(side = 2, at = seq(0,1,0.2), labels =seq(0,1,0.2) )
text(x = 22, y = 0.95, label = "D", cex = 1.5)

par(mar = c(4,0,1.2,1))
plot(tmp$NPR[1,,2],type = 'l',
     main =as.expression(bquote(Spawned~"in"~"a2,"~F~"="~F[MSY])),
     ylim = c(0,1),
     cex.main = 1.5,
     yaxt = 'n',
     xlab = 'Age',
     lwd = 2,  )
lines(tmp$NPR[2,,2],lwd = 2, col = 'blue') ## should look reasonable
text(x = 22, y = 0.95, label = "E", cex = 1.5)

par(mar = c(4,4,1.2,1))
plot(colSums(tmp$NPR[,,1]),type = 'l',
     main = as.expression(bquote(Total~"Numbers,"~F~"="~F[MSY])),
     cex.main = 1.5,
     ylim = c(0,1),
     ylab = 'Numbers-at-Age',
     xlab = 'Age',
     lwd = 2,  )
lines(colSums(tmp$NPR[,,2]),lwd = 2, col = 'blue', type = 'l') ## should look reasonable
text(x = 22, y = 0.95, label = "F", cex = 1.5)
dev.off()

## aep 4x datplot ----
## he wants NAA at f0 and Fmsy for each scenario
load("C:/Users/mkapur/Dropbox/UW/sptlRP/output/2021-06-03datlist.rdata") ## aep 4x scen datlist
# par(mfrow = c(2,4))

layout.matrix <- matrix(c(1:8), ncol = 4, byrow = F)

layout(mat = layout.matrix) 

for(s in 1:4){
        tmp <- doPR(datlist[[s]], FF = c(0,0)) ## defaults, no fishing
        plot(colSums(tmp$NPR[,,1]),type = 'l',
             main = as.expression(bquote(Total~"Numbers,"~F~"="~0)),
             ylim = c(0,1),
             cex.main = 1.5,
             ylab = 'Numbers-at-Age',
             # xaxt = 'n',
             lwd = 2,  )
        lines(colSums(tmp$NPR[,,2]),lwd = 2, col = 'blue', type = 'p') ## should look reasonable
        legend('topright',lwd = 2, legend = c('Present in a2', 'Present in a1'), col = rev(c('black','blue')))
        text(x = 15, y = 0.7, label = SCENNAMES[s], cex = 1.5)

        
        ## get fmsy from object
        load(here("output",paste0("2021-06-03-h=0.7_0.7-",SCENNAMES[s],"/out.rdata"))) 
       out2_new <- data.frame(out[,,'new'])
        FFvec <- c(out2_new[which.max(out2_new[,'tyield']),'FF_Area1'],
                   out2_new[which.max(out2_new[,'tyield']),'FF_Area2']) 
        cat(SCENNAMES[s],FFvec,"\n")
        tmp <- doPR(datlist[[s]], FF =FFvec) 
        plot(colSums(tmp$NPR[,,1]),type = 'l',
             main = as.expression(bquote(Total~"Numbers,"~F~"="~F[MSY])),
             ylim = c(0,1),
             cex.main = 1.5,
             ylab = 'Numbers-at-Age',
             # xaxt = 'n',
             lwd = 2,  )
        lines(colSums(tmp$NPR[,,2]),lwd = 2, col = 'blue', type = 'p') ## should look reasonable
        legend('topright',lwd = 2, legend = c('Present in a2', 'Present in a1'), col = rev(c('black','blue')))
        text(x = 15, y = 0.7, label = SCENNAMES[s], cex = 1.5)
}

## test with movement & some Fs
png(here('figs','testdoPR-fishing.png'))
par(mfrow = c(2,2))
dat <- makeDat(wa = c(5,5),
               fec_a50 = c(6,6),
               fec_a95 = c(12,12),
               slx_a50 = c(9,9),
               slx_a95 = c(13,13),
               pStay = c(0.9,0.6))
tmp <- doPR(dat, FF = c(0.2,0.2)) ## defaults, equally low fishing
all(round(colSums(tmp$NPR[,,1])) == round(colSums(tmp$NPR[,,2]))) ## net exchange should be conserved
plot(tmp$NPR[1,,1],ylim = c(0,1), main = 'A1 SRC - F=c(0.2,0.2)');lines(tmp$NPR[2,,1],col = 'blue') ## A2 ()
legend('topright',lwd = 2, legend = c('stay in a1', 'end up in a2'), col = c('black','blue'))
plot(tmp$NPR[1,,2],ylim = c(0,1), main = 'A2 SRC - F=c(0.2,0.2)');lines(tmp$NPR[2,,2],col = 'blue') 
legend('topright',lwd = 2, legend = c('stay in a2', 'end up in a1'), col = rev(c('black','blue')))
dat <- makeDat(wa = c(5,5),
               fec_a50 = c(6,6),
               fec_a95 = c(12,12),
               slx_a50 = c(9,9),
               slx_a95 = c(13,13),
               pStay = c(0.9,0.6))
tmp <- doPR(dat, FF = c(0.8,0.8)) ## defaults, no fishing
all(round(colSums(tmp$NPR[,,1])) == round(colSums(tmp$NPR[,,2]))) ## net exchange should be conserved
plot(tmp$NPR[1,,1],ylim = c(0,1), main = 'A1 SRC - F=c(0.8,0.8)');lines(tmp$NPR[2,,1],col = 'blue') 
legend('topright',lwd = 2, legend = c('stay in a1', 'end up in a2'), col = c('black','blue'))
plot(tmp$NPR[1,,2], ylim = c(0,1),main = 'A2 SRC - F=c(0.8,0.8)');lines(tmp$NPR[2,,2],col = 'blue') 
legend('topright',lwd = 2, legend = c('stay in a2', 'end up in a1'), col = rev(c('black','blue')))
dev.off()

