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

# Read fips
fips.df <- fread(file.path(datafolder,"fips.df.csv"))
fips_states.df <- fread(file.path(datafolder,"fips_states.df.csv"))

# All data
load(file.path(datafolder,"Prevalence_Incidence_all.Rdata"))
# Cleaned is only >0 cases and positivity and no NA
load(file.path(datafolder,"Prevalence_Incidence_cleaned.Rdata"))

