library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(MASS)
functionfolder<-"Functions"
resultsfolder<-"Results"
source(file.path(functionfolder,"load_data.R"))

rse.fold <- 1.4 # GSD of residual error - so .mc is assumed to be the "true" value
set.seed(3.14159)
cleandata$IU.per100k.mc <- cleandata$IU.per100k * rse.fold^rnorm(nrow(cleandata)) # "True"

# "Predictor" is "true" data - we want to know how well the categories "predict" the true value of IU
cleandata$x <- log(cleandata$IU.per100k.mc)
set.seed(3.14159)
cleandata.samp<-rbind(sample_n(subset(cleandata,type=="state"),20000),
                      sample_n(subset(cleandata,type=="cbsa"),20000),
                      sample_n(subset(cleandata,type=="county"),20000))

res.cdc.samp <- polr(CDCLevelCommTrans ~ x,
              data=cleandata.samp,start=c(1,log(c(10,50,100))),
              method="probit",Hess = TRUE)
print(summary(res.cdc.samp))
print(exp(1/res.cdc.samp$coefficients))
print(exp(res.cdc.samp$zeta/res.cdc.samp$coefficients))

res.cdc <- polr(CDCLevelCommTrans ~ x,
                data=cleandata,
                start=c(res.cdc.samp$coefficients,
                        res.cdc.samp$zeta),
                method="probit",Hess = TRUE)
print(summary(res.cdc))
print(exp(1/res.cdc$coefficients))
print(exp(res.cdc$zeta/res.cdc$coefficients))

gsd.err.CDC <- exp(1/res.cdc$coefficients)
IU.breaks.CDC <- exp(res.cdc$zeta/res.cdc$coefficients)

#####

res.cdc.cases <- polr(CDCLevelCommTrans.Cases ~ x,
                data=cleandata.samp,start=c(1,log(IU.breaks.CDC)),
                method="probit",Hess = TRUE)
print(summary(res.cdc.cases))
print(exp(1/res.cdc.cases$coefficients))
print(exp(res.cdc.cases$zeta/res.cdc.cases$coefficients))

res.cdc.pos <- polr(CDCLevelCommTrans.Pos ~ x,
                      data=cleandata.samp,start=c(1,log(IU.breaks.CDC)),
                      method="probit",Hess = TRUE)
print(summary(res.cdc.pos))
print(exp(1/res.cdc.pos$coefficients))
print(exp(res.cdc.pos$zeta/res.cdc.pos$coefficients))

cdc.parameters <- data.frame(basis=c("Overall","Cases","Positivity"),
                             gsd.err=c(exp(1/res.cdc$coefficients),
                                       exp(1/res.cdc.cases$coefficients),
                                       exp(1/res.cdc.pos$coefficients)))
cdc.parameters <- cbind(cdc.parameters,
                        exp(as.data.frame(rbind(res.cdc$zeta/res.cdc$coefficients,
                                            res.cdc.cases$zeta/res.cdc.cases$coefficients,
                                            res.cdc.pos$zeta/res.cdc.pos$coefficients))))

fwrite(cdc.parameters,file.path(resultsfolder,"CDC_probit_breakpoints.csv"))

