## Run this if you want to upload and proccess the latest data from CovidActNow using the API
## ~4 minutes
library(RCurl)
library(data.table)
library(stringr)
datezero <- "2019-12-31"
datafolder<-"Data"
fips_table <- read.csv(file.path(datafolder,"FIPS_TABLE.csv"),colClasses=c(
  rep("character",4),rep("numeric",2)
))
rownames(fips_table)<-as.character(fips_table$Alpha.code)
statesvec <- fips_table$Alpha.code[2:52]

alldata <- data.frame()
datanames <- c("states","cbsas","counties")
datatypes <- c("state","cbsa","county")
names(datatypes)<-datanames
for (datasetnow in datanames) {
  censusdata <- read.csv(file.path(datafolder,paste0(datasetnow,"-est2019-alldata.csv")),
                         as.is=TRUE,colClasses="character")
  if (datasetnow == "states") {
    censusdata <- subset(censusdata,SUMLEV=="040")
    rownames(censusdata)<-censusdata$STATE
  } else if (datasetnow == "cbsas") {
    censusdata <- subset(censusdata,LSAD=="Metropolitan Statistical Area" |
                           LSAD=="Micropolitan Statistical Area")
    rownames(censusdata)<- censusdata$CBSA
  } else if (datasetnow == "counties") {
    censusdata <- subset(censusdata,SUMLEV=="050")
    censusdata$NAME <- censusdata$CTYNAME
    rownames(censusdata)<-paste0(censusdata$STATE,censusdata$COUNTY)
  }
  download <- getURL(paste0("https://api.covidactnow.org/v2/",
                            datasetnow,
                            ".timeseries.csv?apiKey=f3dba09c8f524a148127fb8f6c689e2e"))
  datnow <- read.csv(text = download,as.is=TRUE,colClasses="character")
  if (datasetnow != "cbsas") {
    datnow <- subset(datnow,state %in% statesvec)
  }
  datnow$name <- censusdata[datnow$fips,"NAME"]
  datnow$pop <- as.numeric(censusdata[datnow$fips,"POPESTIMATE2019"])
  datnow$date <- as.Date(datnow$date)
  datnow$numDate <- as.numeric(datnow$date)-as.numeric(as.Date(datezero))
  datnow$Cases_tau_pct <- as.numeric(datnow$metrics.caseDensity)/1000
  datnow$PositivePct_tau <- as.numeric(datnow$metrics.testPositivityRatio)*100
  datnow <- datnow[order(datnow$state,datnow$numDate),]
  datnow$CumulPosPct <- 0
  for (idnow in unique(datnow$locationId)) {
    indx <- datnow$locationId == idnow
    tmpcases <- datnow$Cases_tau_pct[indx]
    tmpcases[is.na(tmpcases)] <- 0
    datnow$CumulPosPct[indx] <- cumsum(tmpcases) 
  }
  datnow$Cumulcases <- as.numeric(datnow$actuals.cases)
  datnow$Cumuldeath <- as.numeric(datnow$actuals.deaths)
  datnow$icu <- as.numeric(datnow$actuals.icuBeds.currentUsageCovid)
  datnow$hospital <- as.numeric(datnow$actuals.hospitalBeds.currentUsageCovid)
  datnow$death <- as.numeric(datnow$actuals.newDeaths)
  datnow$vaccinated <- as.numeric(datnow$metrics.vaccinationsCompletedRatio)
  datnow$type <- datatypes[datasetnow]
  datnow <- datnow[,c("date","numDate","type","state","county","fips","locationId",
                      "name","pop",
                      "Cases_tau_pct","PositivePct_tau","CumulPosPct",
                      "Cumulcases","Cumuldeath",
                      "icu","hospital","death","vaccinated")]
  # fwrite(datnow,file.path(datafolder,
  #                         paste0("TestPositivity_",datasetnow,".csv")))
  alldata <- rbind(alldata,datnow)
}

# set up fips data frame
fips.df <- alldata[,c("type","state","county","name","fips")]
fips.df <- fips.df[!duplicated(fips.df),]
fips.df <- fips.df[order(factor(fips.df$type,levels=c("state","cbsa","county")),
                         fips.df$fips),]
fips.df$id <- 1:nrow(fips.df)
## Use the "primary state" for cbsas
for (j in 1:nrow(fips.df)) {
  if (fips.df$type[j]=="cbsa") {
    cbsanames <- str_split(fips.df$name[j],", ")[[1]]
    if (length(cbsanames)==2) {
      cbsastate <- str_split(cbsanames[2],"-")[[1]]
      fips.df$state[j] <- cbsastate[1]
    }
  }
}
fips_states.df <- as.data.frame(subset(fips.df,type=="state"))
rownames(fips_states.df) <- fips_states.df$state
fips.df$n.state <- NA
hasstate <- fips.df$state != ""
fips.df$n.state[hasstate] <- fips_states.df[fips.df$state[hasstate],"id"]
fips.df <- as.data.frame(fips.df)
rownames(fips.df) <- fips.df$fips
alldata$state <- fips.df[alldata$fips,"state"]
alldata$id <- fips.df[alldata$fips,"id"]
alldata <- alldata[order(alldata$id,alldata$numDate),]

fwrite(fips.df,file.path(datafolder,"fips.df.csv"))
fwrite(fips_states.df,file.path(datafolder,"fips_states.df.csv"))
save(alldata,file=file.path(datafolder,"TestPositivity.Rdata"))
