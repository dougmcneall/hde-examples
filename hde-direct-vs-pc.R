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

yg <- brewer.pal(9, "YlGn")

X = full_frac[,2:8]
X.norm = normalize(X)
X.stan.norm = normalize(matrix(X.standard, nrow = 1), wrt=X)


# LOOCV reconstruction by direct emulation

# Looking at a 24 hour compute time for direct prediction on the mac
# Not working with NAs very well at the moment
ptm = proc.time()
test = direct.pred(form = ~., X = X.norm, Y = bl.frac.ens[ , 512:520], Xnew=X.stan.norm)
direct.time = proc.time() - ptm

# LOOCV reconstruct using dimension reduction
bl.frac.ens.pc.cv = matrix(NA, nrow=nrow(bl.frac.ens), ncol = ncol(bl.frac.ens))
for(i in 1:nrow(X)){
  X.trunc = X.norm[-i, ]
  Y.trunc = bl.frac.ens[-i, ]
  X.target = X.norm[i, ]
  Y.target = bl.frac.ens[i, ]
  Y.recon = kmpar.pc(Y=Y.trunc, X=X.trunc, newdata=X.target, num.pc=6)
  bl.frac.ens.pc.cv[i, ] = Y.recon$tens
}

# LOOCV reconstruct on a per-gridbox basis
