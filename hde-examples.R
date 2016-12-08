# hde_examples.R
# Example code for the hde package.
# Doug McNeall dougmcneall@gmail.com

# -------------------------------------------------------------------
# 0. Administration
# -------------------------------------------------------------------
library(devtools)
install_github(repo = "dougmcneall/hde")
library(hde)

# some useful emulation and visualisation tools
source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/emtools.R")
source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/imptools.R")
source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/vistools.R")
library(fields)
library(RColorBrewer)
yg <- brewer.pal(9, "YlGn")

# -------------------------------------------------------------------
# 1. Emulate forest fraction in FAMOUS at the standard 
# parameters
# -------------------------------------------------------------------
X = full_frac[,2:8]
X.norm = normalize(X)
X.stan.norm = normalize(matrix(X.standard, nrow = 1), wrt=X)

bl.standard = kmpar.pc(Y = bl.frac.ens, X = X.norm, newdata = X.stan.norm, num.pc = 3)
image.plot(longs, rev(lats),
           remap.famous(bl.standard$tens,longs, lats),
           col=yg)
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)

# -------------------------------------------------------------------
# 2. (slow) How many PCs minimises reconstruction error?
# Test a direct prediction against the best PC prediction.
# -------------------------------------------------------------------
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

# -------------------------------------------------------------------
# 3. Cross validation reconstruction error with 6 Principal components
# -------------------------------------------------------------------
ptm <- proc.time()
# (takes about 4 minutes on desktop, 5.4 on the mac)
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
bl.frac.sd = apply(bl.frac.ens,2, FUN=sd, na.rm=TRUE)
bl.frac.mmae = apply(bl.frac.abserr, 2, FUN=mean, na.rm=TRUE)
bl.frac.mmae.norm = bl.frac.mmae / bl.frac.sd
# Bit of a tricky way to replace NaN and inf with NAs
bl.frac.mmae.norm[is.finite(bl.frac.mmae.norm)==FALSE] <- NA

# Map the mean absolute error of LOOCV reconstruction
dev.new(width = 8, height =6)
image.plot(longs, rev(lats),
           remap.famous(bl.frac.mmae,longs, lats),
           col=yg, main='Mean absolute error 6 PCs', xlab='', ylab='')
map("world2", ylim=c(-90,90), xlim=c(0,360), add=TRUE)

dev.new()
hist(bl.frac.mmae)

# Map the normalised mean absolute error of LOOCV reconstruction
dev.new(width = 8, height =6)
image.plot(longs, rev(lats),
           remap.famous(bl.frac.mmae.norm,longs, lats),
           col=yg, zlim = c(0,1), main='Normalised mean absolute error 6 PCs', xlab='', ylab='')
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)

# Broadly, emulator error scales with standard deviation in the ensemble, and
# works out at just under half of one standard deviation.
mean(bl.frac.mmae.norm, na.rm = TRUE)

# -------------------------------------------------------------------
# 4. How would emulator error compare if you were to just pick a random
# ensemble member in place of the LOOCV emulator reconstruction?
# -------------------------------------------------------------------

bl.random.recon = bl.frac.ens[sample(1:nrow(bl.frac.ens), replace=FALSE), ]
bl.random.recon.abserr = abs(bl.random.recon - bl.frac.ens)
bl.random.recon.mae = apply(bl.random.recon.abserr, 2, mean, na.rm=TRUE)

dev.new(width=8, height=8)
par(mfrow = c(2,1), mar=c(2,2,2,2))
image.plot(longs, rev(lats),
           remap.famous(bl.random.recon.mae,longs, lats),
           col=yg, main='Mean absolute error random', xlab='', ylab='', zlim = c(0,0.4))
map("world2", ylim=c(-90,90), xlim=c(0,360), add=TRUE)

image.plot(longs, rev(lats),
           remap.famous(bl.frac.mmae,longs, lats),
           col=yg, main='Mean absolute error 6 PCs', xlab='', ylab='', zlim = c(0,0.4))
map("world2", ylim=c(-90,90), xlim=c(0,360), add=TRUE)

# The mean of the error normalised by standard deviation is a little under
# double that of using the emulator - closer to a single
# standard deviation of the ensemble.
mean((bl.random.recon.mae / bl.frac.sd), na.rm = TRUE)




