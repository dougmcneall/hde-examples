# hde_examples.R
# Example code for the hde package.

library(devtools)

install_github(repo = "dougmcneall/hde")

library(hde)
library(RColorBrewer)
library(fields)

source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/emtools.R")
source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/imptools.R")
source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/vistools.R")

yg <- brewer.pal(9, "YlGn")

X = full_frac[,2:8]
X.norm = normalize(X)
X.stan.norm = normalize(matrix(X.standard, nrow = 1), wrt=X)

bl.standard = kmpar.pc(Y = bl.frac.ens, X = X.norm, newdata = X.stan.norm, num.pc = 3)
image.plot(longs, rev(lats),
           remap.famous(bl.standard$tens,longs, lats),
           col=yg)
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)

# How many PCs minimises reconstruction error?
# Test a direct prediction against the best PC prediction.
# (figure out how to deal with the NAs)

# ptm <- proc.time()
# n.pcs = 2:10
# mae.recon = rep(NA, max(n.pcs))
# 
# for(j in 2:max(n.pcs)){
#   bl.frac.ens.pc.cv = matrix(NA, nrow=nrow(bl.frac.ens), ncol = ncol(bl.frac.ens))
#   for(i in 1:nrow(X)){
#     X.trunc = X.norm[-i, ]
#     Y.trunc = bl.frac.ens[-i, ]
#     X.target = X.norm[i, ]
#     Y.target = bl.frac.ens[i, ]
#     Y.recon = kmpar.pc(Y=Y.trunc, X=X.trunc, newdata=X.target, num.pc=n.pcs[j])
#     bl.frac.ens.pc.cv[i, ] = Y.recon$tens
#   }
#   mae.recon[j] = mean(abs(bl.frac.ens.pc.cv - bl.frac.ens), na.rm = TRUE)
# }

# Takes about 35 minutes to test PCs 2:9 on desktop
# loop.time = proc.time() - ptm

# There is evidence that mean absolute error bottoms
# Out at 6 or 7 PCs
# pdf(file='../graphics/pc_recon2_9.pdf')
# plot(mae.recon, type = 'b', xlab='Reconstruction PCs used', ylab='Mean absolute cv reconstruction error')
# dev.off()

ptm <- proc.time()
# Let's have a look at 6 PCs (takes about 4 minutes on desktop)
bl.frac.ens.pc.cv = matrix(NA, nrow=nrow(bl.frac.ens), ncol = ncol(bl.frac.ens))
for(i in 1:nrow(X)){
  X.trunc = X.norm[-i, ]
  Y.trunc = bl.frac.ens[-i, ]
  X.target = X.norm[i, ]
  Y.target = bl.frac.ens[i, ]
  Y.recon = kmpar.pc(Y=Y.trunc, X=X.trunc, newdata=X.target, num.pc=6)
  bl.frac.ens.pc.cv[i, ] = Y.recon$tens
}

loop.time = proc.time() - ptm

bl.frac.abserr = abs(bl.frac.ens.pc.cv - bl.frac.ens)
bl.frac.sd = apply(bl.frac.ens,2, FUN = sd, na.rm = TRUE)
bl.frac.mmae = apply(bl.frac.abserr, 2, FUN = mean, na.rm = TRUE)
bl.frac.mmae.norm = bl.frac.mmae / bl.frac.sd
# Bit of a tricky way to replace NaN and inf with NAs
bl.frac.mmae.norm[is.finite(bl.frac.mmae.norm)==FALSE] <- NA

dev.new(width = 8, height =6)
image.plot(longs, rev(lats),
           remap.famous(bl.frac.mmae,longs, lats),
           col=yg)
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)

dev.new()
hist(bl.frac.mmae.norm)

dev.new(width = 8, height =6)
image.plot(longs, rev(lats),
           remap.famous(bl.frac.mmae.norm,longs, lats),
           col=yg, zlim = c(0,1))
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)
