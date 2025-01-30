# LOAD PACKAGES ----------------------------------------------------------------
library(dplyr)
library(RODBC)
library(amt)
library(dplyr)
# READ IN DATA -----------------------------------------------------------------


# how to select all from a table with " " in name
# query2 <- "select * from [Metadata - Work in Progress]"

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


# create new column for yearOfEntry and yearOfExit
dat$yearOfEntry <- ""
dat$yearOfExit <- ""
# currently POSIX data type; convert to char to use stringsplit
dat$dateOfExit <- as.character(dat$dateOfExit)
# get the year only as a column
for(i in 1:nrow(dat)){
	# dat[i,]$yearOfEntry <- if_else(is.na(dat[i,]$dateOfEntry), NA, strsplit(dat[i,]$dateOfEntry,"-")[[1]][1])
	dat[i,]$yearOfEntry <- if_else(grepl("/", dat[i,]$dateOfEntry), strsplit(dat[i,]$dateOfEntry,"/")[[1]][1], strsplit(dat[i,]$dateOfEntry,"-")[[1]][1])
	dat[i,]$yearOfExit <- if_else(is.na(dat[i,]$dateOfExit), NA, strsplit(dat[i,]$dateOfExit,"-")[[1]][1])
}

# standardise names
dat <- dat %>% mutate(outcome = if_else(is.na(outcome), outcome, tolower(outcome)),
											status = NA, # create new column
											status = if_else(outcome == "alive", 1, NA),  # send alive to 1
											status = if_else(outcome == "dead", 0, NA), # send dead to zero
											sex = tolower(sex),
											sex = trimws(sex), # remove white spaces
											sex = if_else(!sex %in% c("f ", "m", "f"), NA, sex),
											causeOfMortality = tolower(causeOfMortality),
											causeOfMortality = if_else(grepl("unknown.*|uk|unk|other|recorded|maybe|train|possible|mva|death|dead|acci",causeOfMortality), "unknown", causeOfMortality),
											# send all cause of mortality to censored if outcome uncertain
											causeOfMortality = if_else(grepl("off-air|lost|failed*.|drop|alive*.|fail|remove|other|end*.|blown*.|malfunction|fade|moved*.",causeOfMortality), "censored", causeOfMortality),
											causeOfMortality = if_else(grepl("wolves*.|wolf*.|grizzly*.|bear*.|predation*.|pred*.|cougar*.", causeOfMortality), "predation", causeOfMortality),
											causeOfMortality = if_else(grepl("health*.|infection|starvation|injury*.|birth*.|condition|calving|malnutrition|avalanche", causeOfMortality), "natural", causeOfMortality),
											causeOfMortality = if_else(grepl("myop*.|strangled|research|poaching*.|train|hunt*.|euthanize*.|collision|pen", causeOfMortality), "human-caused", causeOfMortality),
											causeOfMortality = if_else(grepl("03", yearOfEntry), "2003", yearOfEntry),
											ageCollared = tolower(ageCollared),
											ageCollared = if_else(ageCollared == "unknown", NA, ageCollared),
											ageCollared = if_else(ageCollared %in% c("adult", "adult-mid"), "a", ageCollared),
											ageCollared = if_else(ageCollared %in% c("juv","subadult"), "j", ageCollared),
											ageCollared = if_else(ageCollared %in% c("yearling", "calf"), "y", ageCollared)) %>% # rename calf
											# fix unknown
					 select(-c(outcome)) # remove redundant outcome column



