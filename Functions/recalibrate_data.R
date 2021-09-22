## Modified CDC criteria - combined Moderate and Substantial

cleandata$CDCLevel.Mod <- cleandata$CDCLevel
cleandata$CDCLevel.Mod[cleandata$CDCLevel.Mod==2] <- 1
cleandata$CDCLevel.Mod[cleandata$CDCLevel.Mod==3] <- 2
cleandata$CDCLevelCommTrans.Mod <- factor(c("Low","Moderate","High")[cleandata$CDCLevel.Mod+1],
                                          levels=c("Low","Moderate","High"))

cleandata$CDCLevel.Mod.Cases <- cleandata$CDCLevel.Cases
cleandata$CDCLevel.Mod.Cases[cleandata$CDCLevel.Mod.Cases==2] <- 1
cleandata$CDCLevel.Mod.Cases[cleandata$CDCLevel.Mod.Cases==3] <- 2
cleandata$CDCLevelCommTrans.Mod.Cases <- factor(c("Low","Moderate","High")[cleandata$CDCLevel.Mod.Cases+1],
                                                levels=c("Low","Moderate","High"))

cleandata$CDCLevel.Mod.Pos <- cleandata$CDCLevel.Pos
cleandata$CDCLevel.Mod.Pos[cleandata$CDCLevel.Mod.Pos==2] <- 1
cleandata$CDCLevel.Mod.Pos[cleandata$CDCLevel.Mod.Pos==3] <- 2
cleandata$CDCLevelCommTrans.Mod.Pos <- factor(c("Low","Moderate","High")[cleandata$CDCLevel.Mod.Pos+1],
                                              levels=c("Low","Moderate","High"))

## Get breaks for recalibrated levels
cdc.parameters <- fread(file.path(resultsfolder,"CDC_probit_breakpoints.csv"))
IU.breaks.Rec <- as.numeric(cdc.parameters[1,-(1:2)])
# Two significant digits, drop middle break
IU.breaks.Rec <- signif(IU.breaks[-2],2)

## "True" Recalibrated levels
cleandata$IU.Level.Rec.mc <- 0
for (j in 1:2) cleandata$IU.Level.Rec.mc[cleandata$IU.per100k.mc>=IU.breaks.Rec[j]] <- j
cleandata$IU.Level.Rec.mc.Text <- factor(c("Low","Moderate","High")[cleandata$IU.Level.Rec.mc+1],
                                         levels=c("Low","Moderate","High"))

# Recalibration breakpoints
Rec.parameters <- fread(file.path(resultsfolder,"Rec_probit_breakpoints.csv"))
# 1 significant digits only
Rec.parameters <- cbind(Rec.parameters[,1],signif(Rec.parameters[,-1],1))
# 
Cases.breaks <- as.numeric(Rec.parameters[1,3:4])
Pos.breaks <- as.numeric(Rec.parameters[2,3:4])
Cases.Pos.breaks <- Rec.parameters[3:5,3:4]

# Recalibrated levels
cleandata$IU.Level.Rec.Cases <- 0
for (j in 1:2) cleandata$IU.Level.Rec.Cases[(cleandata$Cases_tau_pct*7000)>=Cases.breaks[j]] <- j
cleandata$IU.Level.Rec.Cases.Text <- 
  factor(c("Low","Moderate","High")[cleandata$IU.Level.Rec.Cases+1],
         levels=c("Low","Moderate","High"))

cleandata$IU.Level.Rec.Pos <- 0
for (j in 1:2) cleandata$IU.Level.Rec.Pos[cleandata$PositivePct_tau>=Pos.breaks[j]] <- j
cleandata$IU.Level.Rec.Pos.Text <- 
  factor(c("Low","Moderate","High")[cleandata$IU.Level.Rec.Pos+1],
         levels=c("Low","Moderate","High"))

cleandata$IU.Level.Rec.Cases.Pos <- 0
for (i in 0:2) {
  indx <- (cleandata$IU.Level.Rec.Cases == i)
  for (j in 1:2) {
    cleandata$IU.Level.Rec.Cases.Pos[indx & cleandata$PositivePct_tau >=
                                       Cases.Pos.breaks[i+1][[j]]] <- j
  }
}
cleandata$IU.Level.Rec.Cases.Pos.Text <- 
  factor(c("Low","Moderate","High")[cleandata$IU.Level.Rec.Cases.Pos+1],
         levels=c("Low","Moderate","High"))

cleandata$IU.Level.Rec.gm <- 0
for (j in 1:2) cleandata$IU.Level.Rec.gm[cleandata$IU.per100k>=IU.breaks.Rec[j]] <- j
cleandata$IU.Level.Rec.gm.Text <- factor(c("Low","Moderate","High")[cleandata$IU.Level.Rec.gm+1],
                                         levels=c("Low","Moderate","High"))

risklevels <- c("Low","Moderate","High")
riskfactors <- factor(risklevels,levels=risklevels)

