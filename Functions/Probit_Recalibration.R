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
cdc.parameters <- fread(file.path(resultsfolder,"CDC_probit_breakpoints.csv"))

# Remove middle break
IU.breaks.Rec <- as.numeric(cdc.parameters[1,-(1:2)])[-2]

# "True" Recalibrated levels
cleandata$IU.Level.Rec.mc <- 0
for (j in 1:2) cleandata$IU.Level.Rec.mc[cleandata$IU.per100k.mc>=IU.breaks.Rec[j]] <- j
cleandata$IU.Level.Rec.mc.Text <- factor(c("Low","Moderate","High")[cleandata$IU.Level.Rec.mc+1],
                                     levels=c("Low","Moderate","High"))

# Find cases break points
cleandata$x.cases <- log(cleandata$Cases_tau_pct*7000) # weekly per 100,000
cleandata$x.pos <- log(cleandata$PositivePct_tau) # 

set.seed(3.14159)
cleandata.samp<-rbind(sample_n(subset(cleandata,type=="state"),20000),
                      sample_n(subset(cleandata,type=="cbsa"),20000),
                      sample_n(subset(cleandata,type=="county"),20000))

res.cases.samp <- polr(IU.Level.Rec.mc.Text ~ x.cases,
                     data=cleandata.samp,start=c(1,log(c(10,100))),
                     method="probit",Hess = TRUE)
print(summary(res.cases.samp))
print(exp(1/res.cases.samp$coefficients))
print(exp(res.cases.samp$zeta/res.cases.samp$coefficients))

res.cases <- polr(IU.Level.Rec.mc.Text ~ x.cases,
                       data=cleandata,
                  start=c(res.cases.samp$coefficients,res.cases.samp$zeta),
                       method="probit",Hess = TRUE)
print(summary(res.cases))
print(exp(1/res.cases$coefficients))
print(exp(res.cases$zeta/res.cases$coefficients))

Cases.breaks<-exp(res.cases$zeta/res.cases$coefficients)

## Unconditional positivity

res.pos.samp <- polr(IU.Level.Rec.mc.Text ~ x.pos,
                       data=cleandata.samp,start=c(1,log(c(1,10))),
                       method="probit",Hess = TRUE)
print(summary(res.pos.samp))
print(exp(1/res.pos.samp$coefficients))
print(exp(res.pos.samp$zeta/res.pos.samp$coefficients))

res.pos <- polr(IU.Level.Rec.mc.Text ~ x.pos,
                     data=cleandata,start=c(res.pos.samp$coefficients,res.pos.samp$zeta),
                     method="probit",Hess = TRUE)
print(summary(res.pos))
print(exp(1/res.pos$coefficients))
print(exp(res.pos$zeta/res.pos$coefficients))

Pos.breaks<-exp(res.pos$zeta/res.pos$coefficients)
Pos.breaks[Pos.breaks>100] <- 100
Pos.breaks[Pos.breaks<0.1] <- 0

## Conditional positivity

res.pos.1 <- polr(IU.Level.Rec.mc.Text ~ x.pos,
                data=subset(cleandata,Cases_tau_pct*7000 < Cases.breaks[1]),
                start=c(res.pos.samp$coefficients,res.pos.samp$zeta),
                method="probit",Hess = TRUE)
print(summary(res.pos.1))
print(exp(1/res.pos.1$coefficients))
print(exp(res.pos.1$zeta/res.pos.1$coefficients))

Pos.breaks.1<-exp(res.pos.1$zeta/res.pos.1$coefficients)
Pos.breaks.1[Pos.breaks.1>100] <- 100
Pos.breaks.1[Pos.breaks.1<0.1] <- 0

res.pos.2 <- polr(IU.Level.Rec.mc.Text ~ x.pos,
                  data=subset(cleandata,Cases_tau_pct*7000 >= Cases.breaks[1] &
                                Cases_tau_pct*7000 < Cases.breaks[2]),
                  start=c(res.pos.samp$coefficients,res.pos.samp$zeta),
                  method="probit",Hess = TRUE)
print(summary(res.pos.2))
print(exp(1/res.pos.2$coefficients))
print(exp(res.pos.2$zeta/res.pos.2$coefficients))

Pos.breaks.2<-exp(res.pos.2$zeta/res.pos.2$coefficients)
Pos.breaks.2[Pos.breaks.2>100] <- 100
Pos.breaks.2[Pos.breaks.2<0.1] <- 0

res.pos.3 <- polr(IU.Level.Rec.mc.Text ~ x.pos,
                  data=subset(cleandata,Cases_tau_pct*7000 >= Cases.breaks[2]),
                  start=c(res.pos.samp$coefficients,res.pos.samp$zeta),
                  method="probit",Hess = TRUE)
print(summary(res.pos.3))
print(exp(1/res.pos.3$coefficients))
print(exp(res.pos.3$zeta/res.pos.3$coefficients))

Pos.breaks.3<-exp(res.pos.3$zeta/res.pos.3$coefficients)
Pos.breaks.3[Pos.breaks.3>100] <- 100
Pos.breaks.3[Pos.breaks.3<0.1] <- 0

### Summary of parameters

Rec.parameters <- data.frame(basis=c("Cases","Positivity","Positivity.1","Positivity.2","Positivity.3"),
                             gsd.err=c(exp(1/res.cases$coefficients),
                                       exp(1/res.pos$coefficients),
                                       exp(1/res.pos.1$coefficients),
                                       exp(1/res.pos.2$coefficients),
                                       exp(1/res.pos.3$coefficients)))
Rec.parameters <- cbind(Rec.parameters,
                        as.data.frame(rbind(Cases.breaks,
                                                Pos.breaks,
                                                Pos.breaks.1,
                                                Pos.breaks.2,
                                                Pos.breaks.3)))

fwrite(Rec.parameters,file.path(resultsfolder,"Rec_probit_breakpoints.csv"))
