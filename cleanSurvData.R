# LOAD PACKAGES ----------------------------------------------------------------
library(dplyr)
library(RODBC)
library(amt)
library(dplyr)
# READ IN DATA -----------------------------------------------------------------

## open channel
chnl <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=./data/SMC.accdb")
## create query
query <- "select * from Survival"
## read in Telemetry table
dat <- sqlQuery(chnl, query)

## Spatial Data
# humanDist <- terra::rast(sf::st_read("./data/spatial-data/human-disturbance/BC_CEF_Human_Disturbance_2021.gdb"))

# PREP DATA --------------------------------------------------------------------

# send response to binary outcome
dat$Outcome <- if_else(!dat$Outcome %in% c("dead", "alive", "Alive", "Dead", "Mortality"), NA, dat$Outcome)

# rename columns
names(dat) <- c("SWCID", "survPKey",
								"survSource", "enteredBy", "idRSCL", "WHLID",
								"herdRSCL", "herdCJN", "sex", "ageCollared", "dateOfEntry",
								"dateOfExit", "suspectedMortalityDate", "outcome", "causeOfMortality",
								"mortalityLocEasting", "mortalityLocNorthing", "comment", "daysMonitored",
								"commentCJNC")

# standardise names
dat <- dat %>% mutate(outcome = if_else(is.na(outcome), outcome, tolower(outcome)),
											status = NA, # create new column
											status = if_else(outcome == "alive", 1, NA),  # send alive to 1
											status = if_else(outcome == "dead", 0, NA), # send dead to zero
											sex = tolower(sex),
											sex = trimws(sex), # remove white spaces
											sex = if_else(!sex %in% c("f ", "m", "f"), NA, sex),
											causeOfMortality = tolower(causeOfMortality),
											causeOfMortality = if_else(grepl("unknown.*|uk|unk|other",causeOfMortality), NA, causeOfMortality),
											ageCollared = tolower(ageCollared),
											ageCollared = if_else(ageCollared == "unknown", NA, ageCollared),
											ageCollared = if_else(ageCollared %in% c("adult", "adult-mid"), "a", ageCollared),
											ageCollared = if_else(ageCollared %in% c("juv","subadult"), "j", ageCollared),
											ageCollared = if_else(ageCollared %in% c("yearling", "calf"), "y", ageCollared)) %>% # rename calf
											# fix unknown
					 select(-c(outcome)) # remove redundant outcome column

