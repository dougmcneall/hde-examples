# hde-direct-vs-pc.R
# An accuracy comparison of direct (by-gridbox)
# emulation vs emulation by dimension reduction.

library(devtools)

install_github(repo = "dougmcneall/hde")

library(hde)
library(RColorBrewer)
library(fields)

source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/emtools.R")
source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/imptools.R")
source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/vistools.R")

yg = brewer.pal(9, "YlGn")
ryb = brewer.pal(9, "RdYlBu")
byr = rev(ryb)

X = full_frac[,2:8]
X.norm = normalize(X)
X.stan.norm = normalize(matrix(X.standard, nrow = 1), wrt=X)

# test direct.pred
# test = direct.pred(form=~., X=X.norm, Y = bl.frac.ens[,510:520], Xnew=X.stan.norm)

## --------------------------------------------------------------------------------
## ran this block of code to do a comparison of PC emulation
## vs direct emulation.
## # # LOOCV reconstruct on a per-gridbox basis
## bl.frac.ens.direct.cv = matrix(NA, nrow=nrow(bl.frac.ens), ncol = ncol(bl.frac.ens))

##  for(i in 1:nrow(X)){
##   X.trunc = X.norm[-i, ]
##   Y.trunc = bl.frac.ens[-i, ]
##   X.target = matrix(X.norm[i, ], nrow=1)
##   Y.target = bl.frac.ens[i, ]
##   Y.recon = direct.pred(form=~., X=X.trunc,Y=Y.trunc, Xnew=X.target)
##   bl.frac.ens.direct.cv[i, ] = Y.recon$mean
## }


## # LOOCV reconstruct using dimension reduction
## bl.frac.ens.pc.cv = matrix(NA, nrow=nrow(bl.frac.ens), ncol = ncol(bl.frac.ens))
## for(i in 1:nrow(X)){
##   X.trunc = X.norm[-i, ]
##   Y.trunc = bl.frac.ens[-i, ]
##   X.target = X.norm[i, ]
##   Y.target = bl.frac.ens[i, ]
##   Y.recon = kmpar.pc(Y=Y.trunc, X=X.trunc, newdata=X.target, num.pc=6)
##   bl.frac.ens.pc.cv[i, ] = Y.recon$tens
## }
## --------------------------------------------------------------------------------

load(file='hde-direct-vs-pc.Rdata')


direct.err = bl.frac.ens - bl.frac.ens.direct.cv
pc.err = bl.frac.ens - bl.frac.ens.pc.cv

direct.abserr = abs(bl.frac.ens - bl.frac.ens.direct.cv)
direct.mae = apply(direct.abserr, 2, mean, na.rm=TRUE)

pc.abserr = abs(bl.frac.ens - bl.frac.ens.pc.cv)
pc.mae = apply(pc.abserr, 2, mean, na.rm=TRUE)

# Totals
pc.mmae = mean(pc.abserr, na.rm=TRUE)
direct.mmae = mean(direct.abserr, na.rm=TRUE)
 

# It looks as though there is not very much difference in
# error comparing "direct" emulation with PC emulation. In
# fact, the PC emulation has a smaller mean absolute error.

pdf(file='../graphics/direct-vs-pc-error-maps.pdf', width=  12, height = 8)
#dev.new(width=12, height=8)
par(mfrow = c(2,2), mar = c(2,2,2,8))
image.plot(longs, rev(lats),
           remap.famous(direct.mae,longs, lats),
           col=yg, zlim = c(0,0.2), xlab = '', ylab = '', axes=FALSE, main='Direct MAE')
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)

image.plot(longs, rev(lats),
           remap.famous(pc.mae,longs, lats),
           col=yg, zlim = c(0,0.2), xlab='', ylab='', axes=FALSE, main='PC MAE')
map("world2", ylim=c(-90,90), xlim = c(0,360), add=TRUE)

image.plot(longs, rev(lats),
           remap.famous(apply(abs(bl.frac.ens.direct.cv - bl.frac.ens.pc.cv),2, mean, na.rm=TRUE) ,longs, lats),
           col=byr, xlab = '', ylab = '', axes=FALSE, main='Mean absolute difference')
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)

image.plot(longs, rev(lats),
           remap.famous(apply((bl.frac.ens.direct.cv - bl.frac.ens.pc.cv),2, mean, na.rm=TRUE) ,longs, lats),
           col=byr, zlim = c(-0.02, 0.02), xlab = '', ylab = '', axes=FALSE, main='Mean difference')
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)
dev.off()


# save.image(file='hde-direct-vs-pc.Rdata')

pdf(file='../graphics/direct-vs-pc-error-plots.pdf', width=10, height=5)
#dev.new(width=10, height=5)
par(mfrow = c(1,2))
plot(bl.frac.ens, bl.frac.ens.direct.cv, col = rgb(0,0,0,alpha=0.3),
     xlab='observed', ylab='predicted')
points(bl.frac.ens, bl.frac.ens.pc.cv, col=rgb(0,0,1,alpha=0.3))
abline(0,1, col = 'white')
legend('bottomright', c('Direct', 'PC') col=c('black', 'blue'), pch = 19)

plot(ecdf(direct.abserr), xlim=c(0,0.4), main = 'ecdf of absolute error')
lines(ecdf(pc.abserr), col='blue')
legend('bottomright', c('Direct', 'PC'), col=c('black', 'blue'), lty = 'solid', bty='n')

text(0.1,0.2, paste('PC MAE=',round(pc.mmae,3), '\nDirect MAE =',round(direct.mmae,3)) )
dev.off()

