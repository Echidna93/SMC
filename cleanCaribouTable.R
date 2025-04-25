# LOAD PACKAGES ----------------------------------------------------------------
library(dplyr)
library(RODBC)
library(amt)
library(dplyr)
# READ IN DATA -----------------------------------------------------------------

# Caribou table contains telemetry data summarised over the individual


# Column Name   | Descriptor
#--------------------------------------------------------------------------------
# SWCID          | ?
# DataSource     | Collector of data
# Herd: herd     | herd
# WLHID          | ?
# IndividualID   | Individual caribou ID
# TelemetryCount | # of telemetry fixes for an individual
# AnimalNotes    | Notes

## open channel
chnl <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=./data/SMC.accdb")
## create query
query <- "select * from Caribou"
## read in Telemetry table
dat <- sqlQuery(chnl, query)
