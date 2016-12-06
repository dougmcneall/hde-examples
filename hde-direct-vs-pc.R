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

km.wrap2 = function (form, em, ...){
  out = NA
  fit = try(DiceKriging::km(form, design = em$X, response = em$y, 
                            control = list(trace = FALSE), ...), silent = TRUE)
  out = fit
  out
}

km.pred.wrap = function(kmobj, Xnew, ...){
  
  pred = try(DiceKriging::predict.km(kmobj, newdata=Xnew, ...))
  pred
             
}

extract.predmean = function(predobj){
  if (class(predobj) == "try-error"){
    out=NA
  } 
  else {
    out = predobj$mean
  }
  out
}

extract.predsd = function(predobj){
  if (class(predobj) == "try-error"){
    out = NA
  } 
  else {
    out = predobj$sd
  }
  out
}

direct.pred2 = function (form, X, Y, Xnew, ...){
  ens.list = emlist(X = X, Y = Y)
  km.list = mclapply(ens.list, FUN = km.wrap2, form = form)
  
  # function fails here - a single NA at the start produces
  # alternating NAs
  #pred.list = mclapply(km.list, FUN = predict, newdata = as.matrix(Xnew, nrow = 1), type = "UK")
  pred.list = mclapply(km.list, FUN = km.pred.wrap, Xnew = as.matrix(Xnew, nrow = 1), type = "UK")
  
  out.mean = sapply(pred.list, FUN=extract.predmean)
  # out.mean = sapply(pred.list, function(x) x$mean)
  out.sd = sapply(pred.list, FUN=extract.predsd)
  #out.sd = sapply(pred.list, function(x) x$sd)
  return(list(mean = out.mean, sd = out.sd))
}

# LOOCV reconstruction by direct emulation
# Looking at a 24 hour compute time for direct prediction on the mac
# direct.pred() isn't working with NAs, and is crapping out the whole mclapply call.
#ptm = proc.time()

test = direct.pred2(form = ~., X = X.norm, 
                    Y = bl.frac.ens[ , 511:514], Xnew=X.stan.norm)

#direct.time = proc.time() - ptm