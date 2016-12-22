## Cross-validation example
## by: Tom.Hengl@isric.org, Maria.RuiperezGonzales@wur.nl and Gerard.Heuvelink@wur.nl
setwd("~/Google Drive/Dissertation Research/Validation Data")

library(sp)
library(randomForest)
library(nnet)
library(plotKML)
library(GSIF)
library(plyr)
library(ROCR)
library(snowfall)
library(mda)
library(psych)
library(hexbin)
library(gridExtra)
library(lattice)
library(grDevices)
library(h2o)
library(scales)
library(ranger)
library(xgboost)
library(caret)
library(doParallel)
library(RCurl)
library(viridis)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
source("cv_functions.R")

## load the data
df <- readRDS("ovA_NCSS_peds.rds")

#### Cross-validation numeric soil property:####

## clean-up target variable:

#mP <- na.omit(mP)
#formulaStringP = as.formula(paste('SNDMHT_A ~ ', paste(cov.lst, collapse="+")))
#test.prop <- cv_numeric(formulaStringP, rmatrix=mP, nfold=5, idcol="ID")
#str(test.prop)

#summary(df$clay)
#dfP <- na.omit(df$clay)

sel.na <- complete.cases(df[,all.vars(formulaStringClay)])
summary(sel.na)

#x <- getURL("https://github.com/ISRICWorldSoil/US48SoilGrids/blob/master/covs100m/SoilGrids_USA48_Covs100m.csv")
#des <- read.csv(text = x)
cov <- colnames(dfP)
cov <- cov[15:345]

####Clay####
formulaStringClay = as.formula(paste('clay ~ ', paste(cov, collapse="+")))
sel.na.clay <- complete.cases(df[,all.vars(formulaStringClay)])
summary(sel.na.clay)
df.clay = df[sel.na.clay,]
clay.prop <- cv_numeric(formulaStringClay, rmatrix=df.clay, nfold=5, idcol="LOC_ID")
str(clay.prop)

####Sand####

formulaStringSand= as.formula(paste('sand ~ ', paste(cov, collapse="+")))
sel.na.sand <- complete.cases(df[,all.vars(formulaStringSand)])
summary(sel.na.sand)
df.sand = df[sel.na.sand,]
sand.prop <- cv_numeric(formulaStringSand, rmatrix=df.sand, nfold=5, idcol="LOC_ID")
str(sand.prop)

####SOC####

formulaStringSOC= as.formula(paste('soc ~ ', paste(cov, collapse="+")))
sel.na.soc <- complete.cases(df[,all.vars(formulaStringSOC)])
summary(sel.na.soc)
df.soc = df[sel.na.soc,]
soc.prop <- cv_numeric(formulaStringSOC, rmatrix=df.soc, nfold=5, idcol="LOC_ID")
str(soc.prop)

#### Bulk Density####

formulaStringBd= as.formula(paste('bd ~ ', paste(cov, collapse="+")))
sel.na.bd <- complete.cases(df[,all.vars(formulaStringBd)])
summary(sel.na.bd)
df.bd = df[sel.na.bd,]
bd.prop <- cv_numeric(formulaStringBd, rmatrix=df.bd, nfold=5, idcol="LOC_ID")
str(bd.prop)

####N_tot####

formulaStringN= as.formula(paste('n_tot ~ ', paste(cov, collapse="+")))
sel.na.n <- complete.cases(df[,all.vars(formulaStringN)])
summary(sel.na.n)
df.n = df[sel.na.n,]
n_tot.prop <- cv_numeric(formulaStringN, rmatrix=df,n, nfold=5, idcol="LOC_ID")
str(n_tot.prop)




#mP2 <- m2[complete.cases(m2[,all.vars(formulaStringP2)]),]

# ## Plot CV results:
plt_clay <- xyplot(soc.prop[[1]]$Predicted~soc.prop[[1]]$Observed, asp=1, par.settings=list(plot.symbol = list(col=alpha("black", 0.6), fill=alpha("#404788FF", 0.6), pch=21, cex=0.9)), scales=list(x=list(log=TRUE, equispaced.log=FALSE), y=list(log=TRUE, equispaced.log=FALSE)), xlab="measured", ylab="predicted (machine learning)")
# #scales=list(x=list(log=TRUE, equispaced.log=FALSE), y=list(log=TRUE, equispaced.log=FALSE)) # Log scale
# plt_clay


## Hexbin plot - Clay ##

d.meas.clay <- min(clay.prop[[1]]$Observed, na.rm=TRUE)
pred.clay <- clay.prop[[1]]$Predicted+ifelse(d.meas.clay==0, 1, d.meas.clay)
meas.clay <- clay.prop[[1]]$Observed+ifelse(d.meas.clay==0, 1, d.meas.clay)
lim.clay <- range(clay.prop[[1]]$Observed, na.rm=TRUE)

## Hexbin plot - Sand ##

d.meas.sand <- min(sand.prop[[1]]$Observed, na.rm=TRUE)
pred.sand <- sand.prop[[1]]$Predicted+ifelse(d.meas.sand==0, 1, d.meas.sand)
meas.sand <- sand.prop[[1]]$Observed+ifelse(d.meas.sand==0, 1, d.meas.sand)
lim.sand <- range(sand.prop[[1]]$Observed, na.rm=TRUE)

## Hexbin plot - SOC ##

d.meas.soc <- min(soc.prop[[1]]$Observed, na.rm=TRUE)
pred.soc <- soc.prop[[1]]$Predicted+ifelse(d.meas.soc==0, 1, d.meas.soc)
meas.soc <- soc.prop[[1]]$Observed+ifelse(d.meas.soc==0, 1, d.meas.soc)
lim.soc <- range(soc.prop[[1]]$Observed, na.rm=TRUE)
lim.soc[1] <- ifelse(d.meas.soc==0,lim.soc[1]<-1, lim.soc[1])

## Hexbin plot - Bulk Density ##

d.meas.bd <- min(bd.prop[[1]]$Observed, na.rm=TRUE)
pred.bd <- bd.prop[[1]]$Predicted+ifelse(d.meas.bd==0, 1, d.meas.bd)
meas.bd <- bd.prop[[1]]$Observed+ifelse(d.meas.bd==0, 1, d.meas.bd)
lim.bd <- range(bd.prop[[1]]$Observed, na.rm=TRUE)


## Hexbin plot - N total ##

d.meas.n <- min(n_tot.prop[[1]]$Observed, na.rm=TRUE)
pred.n <- n_tot.prop[[1]]$Predicted+ifelse(d.meas.n==0, 1, d.meas.n)
meas.n <- n_tot.prop[[1]]$Observed+ifelse(d.meas.n==0, 1, d.meas.n)
lim.n <- range(n_tot.prop[[1]]$Observed, na.rm=TRUE)


## Correlation plot:
pfun <- function(x,y, ...){
  panel.hexbinplot(x,y, ...)
  panel.abline(0,1,lty=1,lw=2,col="black")
  ## To plot RMSE around the 1:1 line:
  #panel.abline(0+test.ORC$Summary$logRMSE,1,lty=2,lw=2,col="black")
  #panel.abline(0-test.ORC$Summary$logRMSE,1,lty=2,lw=2,col="black")
}

# color scheme
viri <- c("#440154FF", "#39568CFF", "#1F968BFF", "#73D055FF", "#FDE725FF")

plt.clay <- hexbinplot(pred.clay~meas.clay,colramp=colorRampPalette(rev(viri)), main="Percent Clay", xlab="Measured", ylab="Predicted", type="g", lwd=1, lcex=8, inner=.2, cex.labels=.8, asp=1, xbins=25, density=40, xlim=lim.clay, ylim=lim.clay, panel=pfun)

plt.sand <- hexbinplot(pred.sand~meas.sand,colramp=colorRampPalette(rev(viri)), main="Percent Sand", xlab="Measured", ylab="Predicted", type="g", lwd=1, lcex=8, inner=.2, cex.labels=.8, asp=1, xbins=25, density=40, xlim=lim.sand, ylim=lim.sand, panel=pfun)

plt.soc <- hexbinplot(pred.soc~meas.soc,colramp=colorRampPalette(rev(viri)), main="log(Percent SOC)", xlab="Measured", ylab="Predicted", type="g", scales=list(x=list(log=TRUE, equispaced.log=FALSE), y=list(log=TRUE, equispaced.log=FALSE)), lwd=1, lcex=8, inner=.2, cex.labels=.8, asp=1, xbins=25, density=40, xlim=lim.soc, ylim=lim.soc, panel=pfun)

plt.bd <- hexbinplot(pred.bd~meas.bd,colramp=colorRampPalette(rev(viri)), main="Bulk Density", xlab="Measured", ylab="Predicted", type="g", lwd=1, lcex=8, inner=.2, cex.labels=.8, asp=1, xbins=25, density=40, xlim=lim.bd, ylim=lim.bd, panel=pfun)

plt.n <- hexbinplot(pred.n~meas.n,colramp=colorRampPalette(rev(viri)), main="log(Total N)", xlab="Measured", ylab="Predicted", type="g", scales=list(x=list(log=TRUE, equispaced.log=FALSE), y=list(log=TRUE, equispaced.log=FALSE)),  lwd=1, lcex=8, inner=.2, cex.labels=.8, asp=1, xbins=25, density=40, xlim=lim.n, ylim=lim.n, panel=pfun)

# log plot for n and soc.

#colramp=colorRampPalette(R_pal[["bpy_colors"]][1:18])
#brewer.pal(11,"Spectral"))(100)




####Soil Class Cross Validation####


df_gg <- readRDS("ovA_TAX_gg.pnts.rds")

## clean-up target variable:
xg = summary(df_gg$soiltype, maxsum=length(levels(df_gg$soiltype)))
selg.levs = attr(xg, "names")[xg > 5]
summary(df_gg$soiltype)
## regression matrix:
cov_gg <- colnames(df_gg)
cov_gg <- cov_gg[c(7,14:281)]

formulaStringGG = as.formula(paste('soiltype ~', paste(cov_gg, collapse="+")))
df_gg <- df_gg[complete.cases(df_gg[,all.vars(formulaString)]),]

## Cross-validation factor-type variable:
test.CLASS <- cv_factor(formulaString, rmatrix=df_gg, nfold=5, idcol="LOC_ID")
str(test.CLASS)
test.CLASS[["Classes"]]
test.CLASS[["Purity"]]
test.CLASS[["Confusion.matrix"]]
