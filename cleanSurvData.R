# LOAD PACKAGES ----------------------------------------------------------------
library(dplyr)
library(RODBC)
library(amt)
library(dplyr)
library(IPMbook)
library(lubridate)
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
dat$monthOfEntry <- ""
dat$monthOfExit <- ""
# currently POSIX data type; convert to char to use stringsplit
dat$dateOfExit <- as.character(dat$dateOfExit)
# get the year only as a column
for(i in 1:nrow(dat)){
	dat[i,]$yearOfEntry <- if_else(grepl("/", dat[i,]$dateOfEntry), strsplit(dat[i,]$dateOfEntry,"/")[[1]][1], strsplit(dat[i,]$dateOfEntry,"-")[[1]][1])
	dat[i,]$yearOfExit <- if_else(is.na(dat[i,]$dateOfExit), NA, strsplit(dat[i,]$dateOfExit,"-")[[1]][1])
	dat[i,]$monthOfEntry <- if_else(grepl("/", dat[i,]$dateOfEntry), strsplit(dat[i,]$dateOfEntry,"/")[[1]][2], strsplit(dat[i,]$dateOfEntry,"-")[[1]][2])
	dat[i,]$monthOfExit <- if_else(is.na(dat[i,]$dateOfExit), NA, strsplit(dat[i,]$dateOfExit,"-")[[1]][2])
}
dat[10,]$yearOfEntry <- "2019"
# standardise names
dat <- dat %>% mutate(outcome = if_else(is.na(outcome), outcome, tolower(outcome)),
											status = NA, # create new column
											status = if_else(grepl("dead|mortality", outcome), 1, NA), # send dead to 1
											status = if_else(outcome == "alive", 0, status),  # send alive to 0
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
											yearOfEntry = if_else(grepl("03", yearOfEntry), "2003", yearOfEntry),
											ageCollared = tolower(ageCollared),
											ageCollared = if_else(ageCollared == "unknown", NA, ageCollared),
											ageCollared = if_else(ageCollared %in% c("adult", "adult-mid"), "a", ageCollared),
											ageCollared = if_else(ageCollared %in% c("juv","subadult"), "j", ageCollared),
											ageCollared = if_else(ageCollared %in% c("yearling", "calf"), "y", ageCollared),
											age = NA,
											age = if_else(ageCollared == "y", 1, age),
											age = if_else(ageCollared == "j", 2, age),
											age = if_else(ageCollared == "a", 3, age)) %>% filter(!is.na(dateOfEntry))
											# fix unknow n
					 #select(-c(outcome)) # remove redundant outcome column
dat[476,]$yearOfEntry <- 2019
dat[476,]$yearOfExit <- 2021
dat <- dat[-c(2088:2110),] # remove years with unclear end

#dat$daysMonitored <- if_else(is.na(dat$daysMonitored), abs(with(dat, as.numeric(difftime(as.Date(dat$dateOfEntry),
#																	as.Date(dat$dateOfExit)), units = "days"))), dat$daysMonitored)

#dat$yearsMonitored <- dat$daysMonitored / 365
# create a new variable for finding min month-year
dat <- dat %>% mutate(monthYearOfEntry = as.numeric(yearOfEntry) + as.numeric(monthOfEntry))
dat <- dat %>% filter(!is.na(yearOfExit), !is.na(monthOfExit)) %>%
	mutate(monthYearOfExit = as.numeric(yearOfExit) + as.numeric(monthOfExit))
firstDate <- dat[min(dat$monthYearOfEntry),]$dateOfEntry # get first year
lastDate <- dat[max(dat$monthYearOfExit),]$dateOfExit # get first year

# make matrix
minYear <- unique(min(as.numeric(dat$yearOfEntry), na.rm = TRUE)) # grab min year
maxYear <- unique(max(as.numeric(dat$yearOfExit), na.rm = TRUE)) # grab max year
years <- as.character(seq(minYear, maxYear, by = 1)) # character vector of years

firstMonth <- dat[min(dat$monthYearOfEntry),]$monthOfEntry
lastMonth <- dat[max(dat$monthYearOfExit),]$monthOfExit # get first year
monthYears <- c()
# get months for first year
firstMonths <- seq(firstMonth, 12)
# get months for last year
lastMonths <- seq(1, as.numeric(lastMonth))
# grab the sequences of months
months <- c(firstMonths, rep(seq(1, 12, 1), (length(years) - 1)), lastMonths)

# get unique combinations of months and years
monthYears <- c()
k <- 1
for(i in 1:(length(years) - 1)){
	for(j in 1:12){
		monthYears[k] <- paste0(j, "-", years[i + 1])
		k <- k + 1
	}
}
firstMonthYears <- paste0(firstMonths,"-", years[1])
lastMonthYears <- paste0(lastMonths,"-", years[length(years)])
monthYears <- c(firstMonthYears, monthYears, lastMonthYears)

survMat <- data.frame(matrix(0, nrow = nrow(dat), ncol = length(monthYears)))
names(survMat) <- monthYears

dat <- dat %>% mutate(monthYearOfEntry = paste0(monthOfEntry, "-", yearOfEntry),
											 monthYearOfExit = paste0(monthOfExit, "-", yearOfExit))
for(i in 1:nrow(dat)){
	if(!is.na(dat[i,]$monthYearOfExit)){
	survMat[i,][as.character(dat[i,]$monthYearOfEntry)] <- 1
	survMat[i,][as.character(dat[i,]$monthYearOfExit)] <- 1
	}
	else{
	survMat[i,][as.character(dat[i,]$monthYearOfEntry)] <- 1
	}
}

# convert to matrix for NIMBLE
survMat <- as.matrix(survMat)
# create f-array
# holds first non-NA entry for each row of survMat
f <- c()
for(i in 1:nrow(survMat)){
	f[i] <- which(!is.na(survMat[i,])) %>% first()
}
ninds <- length(f)
nyears <- ncol(survMat)
z <- array(NA, dim=c(ninds, nyears)) # Empty alive/dead matrix  # Initial conditions: all individuals alive at f(i) for (i in 1:nind){ z[i,f[i]]

for (i in 1:ninds){
	z[i,f[i]] <- 1
}

# use IPMbook to get our mdeadArray
out <- IPMbook::marrayDead(survMat)
