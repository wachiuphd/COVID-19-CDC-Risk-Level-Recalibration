## Update calculations of prevalence and incidence
## ~10 minutes
library(data.table)
library(dplyr)
library(tidyr)
library(boot)
library(coda)
library(googledrive)
library(RCurl)
library(stringr)
datezero <- "2019-12-31"
mindate <- "2020-03-19"
functionfolder<-"Functions"
datafolder<-"Data"

# Read test positivity data
load(file.path(datafolder,"TestPositivity.Rdata"))
alldata <- subset(alldata,date >= mindate)

# If positivity missing, use state-level values if available
statedata <- as.data.frame(subset(alldata,type=="state"))
rownames(statedata) <- paste(statedata$state,statedata$numDate)
indx.pos <- is.na(alldata$PositivePct_tau)
alldata$PositivePct_tau[indx.pos] <- statedata[paste(alldata$state[indx.pos],
                                                     alldata$numDate[indx.pos]),
                                               "PositivePct_tau"]

# define different geographic entities
alldata$type <- factor(alldata$type,levels=c("state","cbsa","county"))
alldata$typename <- factor(c("States","CBSAs","Counties")[as.numeric(alldata$type)],
                           levels=c("States","CBSAs","Counties"))

### Empirical results

alldata$IU.per100k <- sqrt(alldata$Cases_tau_pct*alldata$PositivePct_tau)*1000
alldata$Testing.Rate <- alldata$Cases_tau_pct/alldata$PositivePct_tau
alldata$Testing.Rate[is.infinite(alldata$Testing.Rate)] <- NA
alldata$I.Testing.Rate <- sqrt(alldata$Testing.Rate)

### CDC Risk Levels

alldata$CDCLevel.Cases <- NA
indx0 <- (alldata$Cases_tau_pct < (10/7000)) 
indx0[is.na(indx0)] <- FALSE
alldata$CDCLevel.Cases[indx0] <- 0
indx1 <- (alldata$Cases_tau_pct >= (10/7000)) 
indx1[is.na(indx1)] <- FALSE
alldata$CDCLevel.Cases[indx1] <- 1
indx2 <- (alldata$Cases_tau_pct >= (50/7000)) 
indx2[is.na(indx2)] <- FALSE
alldata$CDCLevel.Cases[indx2] <- 2
indx3 <- (alldata$Cases_tau_pct >= (100/7000)) 
indx3[is.na(indx3)] <- FALSE
alldata$CDCLevel.Cases[indx3] <- 3
alldata$CDCLevelCommTrans.Cases <- factor(c("Low","Moderate","Substantial","High")[alldata$CDCLevel.Cases+1],
                                          levels=c("Low","Moderate","Substantial","High"))

alldata$CDCLevel.Pos <- NA
indx0 <- alldata$PositivePct_tau < 5 # 7-day positivity < 5%
indx0[is.na(indx0)] <- FALSE
alldata$CDCLevel.Pos[indx0] <- 0
indx1 <- alldata$PositivePct_tau >= 5 # 7-day positivity >= 5%
indx1[is.na(indx1)] <- FALSE
alldata$CDCLevel.Pos[indx1] <- 1
indx2 <- alldata$PositivePct_tau >= 8 # 7-day positivity >= 8%
indx2[is.na(indx2)] <- FALSE
alldata$CDCLevel.Pos[indx2] <- 2
indx3 <- alldata$PositivePct_tau >= 10 # 7-day positivity >= 10%
indx3[is.na(indx3)] <- FALSE
alldata$CDCLevel.Pos[indx3] <- 3
alldata$CDCLevelCommTrans.Pos <- factor(c("Low","Moderate","Substantial","High")[alldata$CDCLevel.Pos+1],
                                        levels=c("Low","Moderate","Substantial","High"))

alldata$CDCLevel <- NA
indx0 <- (alldata$Cases_tau_pct < (10/7000)) & # total last 7 days < 10 per 100,000
  alldata$PositivePct_tau < 5 # 7-day positivity < 5%
indx0[is.na(indx0)] <- FALSE
alldata$CDCLevel[indx0] <- 0
indx1 <- (alldata$Cases_tau_pct >= (10/7000)) | # total last 7 days >= 10 per 100,000
  alldata$PositivePct_tau >= 5 # 7-day positivity >= 5%
indx1[is.na(indx1)] <- FALSE
alldata$CDCLevel[indx1] <- 1
indx2 <- (alldata$Cases_tau_pct >= (50/7000)) | # total last 7 days >= 50 per 100,000
  alldata$PositivePct_tau >= 8 # 7-day positivity >= 8%
indx2[is.na(indx2)] <- FALSE
alldata$CDCLevel[indx2] <- 2
indx3 <- (alldata$Cases_tau_pct >= (100/7000)) | # total last 7 days >= 100 per 100,000
  alldata$PositivePct_tau >= 10 # 7-day positivity >= 10%
indx3[is.na(indx3)] <- FALSE
alldata$CDCLevel[indx3] <- 3
alldata$CDCLevelCommTrans <- factor(c("Low","Moderate","Substantial","High")[alldata$CDCLevel+1],
                                    levels=c("Low","Moderate","Substantial","High"))

### Incidence calculation 
Trec <- 10
ids <- unique(alldata$id)
alldata$Incidence.wk.per100k <- NA
for (j in 1:length(ids)) {
  idnow <- ids[j]
  if (j %% 10 == 0) print(paste(j,idnow))
  indx <- alldata$id==idnow
  alldata.now <- alldata[indx,]
  Incidence.wkly <- c(rep(0,7),diff(alldata.now$IU.per100k,7)) + # Difference in prevalence
    frollsum(alldata.now$Cases_tau_pct*1000,7,align="right") + # Cases the last week
    frollsum(alldata.now$IU.per100k,7,align = "center")/Trec # Correction for recovered cases
  alldata$Incidence.wk.per100k[indx] <- Incidence.wkly
}

cleandata <- subset(alldata,!is.na(Cases_tau_pct) & !is.na(PositivePct_tau))
cleandata <- subset(cleandata,Cases_tau_pct > 0 & PositivePct_tau > 0)
save(alldata,file=file.path(datafolder,"Prevalence_Incidence_all.Rdata"))
save(cleandata,file=file.path(datafolder,"Prevalence_Incidence_cleaned.Rdata"))
