# Calibrate data using probit breakpoints

rse.fold <- 1.4 # GSD of residual error - so .mc is assumed to be the "true" value
set.seed(3.14159)
cleandata$IU.per100k.mc <- cleandata$IU.per100k * rse.fold^rnorm(nrow(cleandata)) # "True"

## Get breaks
cdc.parameters <- fread(file.path(resultsfolder,"CDC_probit_breakpoints.csv"))
IU.breaks <- as.numeric(cdc.parameters[1,-(1:2)])
# Two significant digits
IU.breaks <- signif(IU.breaks,2)

## "True" values 
cleandata$IU.Level.mc <- 0
for (j in 1:3) cleandata$IU.Level.mc[cleandata$IU.per100k.mc>=IU.breaks[j]] <- j
cleandata$IU.Level.mc.Text <- factor(c("Low","Moderate","Substantial","High")[cleandata$IU.Level.mc+1],
                                     levels=c("Low","Moderate","Substantial","High"))

## GM based values
cleandata$IU.Level.gm <- 0
for (j in 1:3) cleandata$IU.Level.gm[cleandata$IU.per100k>=IU.breaks[j]] <- j
cleandata$IU.Level.gm.Text <- factor(c("Low","Moderate","Substantial","High")[cleandata$IU.Level.gm+1],
                                     levels=c("Low","Moderate","Substantial","High"))

