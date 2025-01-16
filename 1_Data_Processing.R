##%######################################################%##
#                                                          #
####                 UK data processing                 ####
#                                                          #
##%######################################################%##

# This script carries out the processing required on the raw occurence record datasets. 
# Since data from the ladybird survey and irecord are formatted differently, 
# processing of the datasets is required before combining the two.

# clear environment
rm(list = ls())

# load libraries
library(terra)
library(sparta)
library(sf)
library(dplyr)
library(splus2R)
library(ggplot2)
library(rnaturalearth)
library(tidyterra)


# set directories
datadir <- "0_Data/"
outdir <- "0_Data/processed/"
if(!dir.exists(outdir)) dir.create(outdir)

# load in the data from the NBN
ladybird_survey <- read.csv(paste0(datadir, "LadybirdSurvey/Coccinellid_ladybird_non-ladybird_21-11-2024.csv")) # updated dataset from the BRC. excluding iRecord
irecord_ladybirds <- read.csv(paste0(datadir, "iRecord_Ladybirds/records-2024-03-16_-_iRecord_Ladybirds.csv")) # iRecord data downloaded from the NBN

# ladybird_survey - 220647 rows, 77 columns
# irecord_ladybirds - 155365 rows, 3004 columns

# need to organise the datasets so that they can be combined, 
# currently the data from BRC is formatted very differently to that from iRecord via the NBN
# There is some missing info inc lat/lon that needs to be determined.



##%######################################################%##
#                                                          #
####               1. Data explorations                 ####
#                                                          #
##%######################################################%##

# taking a look at the key variables


## Explore ladybird survey data ##

# how many records have same start and end date?
summary(ladybird_survey$STARTDATE == ladybird_survey$ENDDATE)
# Mode      FALSE    TRUE 
# logical   16940  203707 

length(unique(ladybird_survey$NAME)) # 83
length(unique(ladybird_survey$CONCEPT)) # 83
length(unique(ladybird_survey$GRIDREF)) # 72783
table(ladybird_survey$DATE_TYPE)
# Date specified to a number of days                     Date specified to nearest year 
# 1703                                                   7646 
# Date specified to range of months                   Date specified to range of years 
# 235                                                 3940 
# Date specified to the nearest day                   Date specified to the nearest month (1st - last) 
# 203408                                              2498 
# No date or unknown, combined with dataset end date  Only the end date to nearest year known. 
# 352                                                 865 


# convert dates so can extract and assess years
ladybird_survey$STARTDATE <- as.Date(ladybird_survey$STARTDATE, format = "%d/%m/%Y")
ladybird_survey$ENDDATE <- as.Date(ladybird_survey$ENDDATE, format = "%d/%m/%Y")

# extract year of sample
ladybird_survey$year_start <- format(ladybird_survey$STARTDATE, "%Y")
ladybird_survey$year_end <- format(ladybird_survey$ENDDATE, "%Y")

# are start and end years the same?
summary(ladybird_survey$year_start == ladybird_survey$year_end)
#    Mode   FALSE    TRUE    NA's 
# logical    4098  215332    1217 

# look at the range and coverage of years
table(ladybird_survey$year_start) # 1600 - 2021
table(ladybird_survey$year_end) # 1816 - 2021
# most recent records are add to the survey via iRecord


## Explore irecord data ##

# look at column names
names(irecord_ladybirds)

# subset to desired columns
irecord_sub <- irecord_ladybirds[, c(17, 18, 30, 38, 44, 45, 66, 71:73, 87, 95, 111, 129, 130, 138, 141, 150, 155, 163, 179, 190, 191, 194, 205, 211:217, 220:229)]

# a few summaries of the remaining
nrow(irecord_sub) # 155365 observations
table(irecord_sub$basisOfRecord.processed) # all human observation
table(irecord_sub$lifeStage) # Mostly adult records, some larva, very few egg/pupa/other, some unknown
table(irecord_sub$occurrenceStatus.processed) # all present
range(as.Date(irecord_sub$eventDate), na.rm = T) # "1961-07-20" "2024-02-27"
range(irecord_sub$year.processed, na.rm = T) # 1961 2024
range(irecord_sub$eventDate[!irecord_sub$eventDate == ""]) # "1961-07-20" "2024-02-27"
table(irecord_sub$country.processed)
#                United Kingdom 
#           341          155024  
summary(is.na(irecord_sub$coordinatePrecision)) # all NAs
length(unique(irecord_sub$gridReference)) # 50006 unique grid refs
length(unique(irecord_sub$scientificName.processed)) # 63
table(irecord_sub$taxonRank.processed)
# family              form             genus           species species aggregate 
#    146              7110               149            147876                84 

table(irecord_sub$stateProvince.processed)

### subsetting the data ###
#                  England      Isle of Man Northern Ireland         Scotland            Wales 
#     341           137058             1349              678            10584             5355

# n names
length(unique(irecord_sub$scientificName.processed)) #63




### Create plot of n records over time from both datasets ###
# combine datasets, simple combination only here to plot number of records per year. 

# first, remove those where data could span multiple years
lb_surv <- ladybird_survey[!ladybird_survey$DATE_TYPE %in% c("Date specified to range of years", "No date or unknown, combined with dataset end date", "Only the end date to nearest year known."), ]
# 215490 rows

# keep records for 1970 onward
lb_surv <- lb_surv[lb_surv$year_start >= 1970, ]
# 212128 rows

# subset to those with same start and end year
lb_surv <- lb_surv[lb_surv$year_start == lb_surv$year_end, ]
# 211981

# keep records for 1970 onward
lb_irec <- irecord_sub[irecord_sub$year.processed >= 1970, ]
# 155358 rows

# just extract the years for plotting
plotdata <- data.frame(year = c(as.numeric(lb_irec$year.processed), as.numeric(lb_surv$year_end)))
plotdata <- subset(plotdata, year >= 1970 & year <= 2023) # few recs for 2024


#### Figure of number of records over time ####

ggplot(data = plotdata) + 
  geom_bar(aes(x = year), fill = c("#8B2323")) + 
  xlab("Year") +
  ylab("Number of records") + 
  theme_bw() +
  theme(text = element_text(size = 10))

# save
ggsave(filename = paste0(outdir, "/Fig1_nrecords_year.pdf"), 
       dpi = 250, width = 4.5, height = 3, units = "in")




##%######################################################%##
#                                                          #
####   2. Organise data so datasets can be combined     ####
#                                                          #
##%######################################################%##


# UK ladybird survey data is organised differently to the irecord data from the NBN.

## irecord data ##

# Select only those records at the species level
irecord_sub <- irecord_sub[irecord_sub$taxonRank.processed == "species", ] # 147876
length(unique(irecord_sub$scientificName.processed)) # 52 species

# take a look at the species names
sp_names_irec <- unique(irecord_sub$scientificName.processed)
sp_names_irec[order(sp_names_irec)]

# seem to have some additional species here than in the trait dataset
table(irecord_sub$scientificName.processed)

# "Cryptolaemus montrouzieri"  # 6 records
# "Exochomus nigromaculatus"  # 1 records
# "Oenopia conglobata" # 3 records
# "Rodolia cardinalis" # 2 records
# "Scymnus rubromaculatus" # 6 records


# separate names of additional species to subset later
add_sp <- c("Cryptolaemus montrouzieri", "Exochomus nigromaculatus", "Oenopia conglobata",
            "Rodolia cardinalis", "Scymnus rubromaculatus")


# Select records with complete date information
irecord_sub <- irecord_sub[!irecord_sub$eventDate.processed == "", ] # 147876

# only use data from 1970 onwards
table(irecord_sub$year.processed)
hist(irecord_sub$year.processed)
irecord_sub <- irecord_sub[irecord_sub$year.processed >= 1970, ] # 147869

# Select records for UK, excluded Isle of Man and the blanks
irecord_sub <- irecord_sub[irecord_sub$stateProvince.processed %in% c("England", "Scotland", "Wales", "Northern Ireland"), ] # 146212

# drop species that are not included in the database
irecord_sub <- irecord_sub[!irecord_sub$scientificName.processed %in% add_sp, ] ## 146196 

length(unique(irecord_sub$scientificName.processed)) # 47 species



#### Assess location of points

#### Convert lat/lons to spatial points to use for data extraction ####
lb_xy <- vect(irecord_sub, geom =c("decimalLongitude.processed", "decimalLatitude.processed"))
#plot(lb_xy) # take a look

# get world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# subset to the UK
UK <- world[which(world$geounit == "United Kingdom"),]

# match crs
crs(lb_xy) <- crs(world)

# take a look at the spatial spread of points
ggplot() +
  geom_sf(data = UK, fill = NA, col = "black") +
  geom_spatvector(data = lb_xy)



## UK Ladybird Survey data ##

# subset to relevant columns
surv_sub <- ladybird_survey[ , c(2:6, 9:10, 13, 15, 17)] # 220647 records

# extract years and months from dates
surv_sub$year_start <- format(surv_sub$STARTDATE, "%Y")
surv_sub$year_end <- format(surv_sub$ENDDATE, "%Y")

summary(surv_sub$year_start == surv_sub$year_end)
#    Mode   FALSE    TRUE    NA's 
# logical    4098  215332    1217 

# just select records where date to the nearest day
surv_sub <- surv_sub[surv_sub$DATE_TYPE == c("Date specified to the nearest day"), ]
# 203408 rows

# summary(surv_sub$year_start == surv_sub$year_end)

# still some where the start and end dates are different
surv_sub <- surv_sub[surv_sub$STARTDATE == surv_sub$ENDDATE, ] # 203330

# extract months
surv_sub$month <- format(surv_sub$STARTDATE, "%m")


## determine lat/lon from grid refs

table(surv_sub$PRECISION)
#    1     10    100   1000   2000   5000  10000 
# 3125  23243 104893  65482    897     31   5592

# remove records where the grid ref type is not OSGB or OSNI
surv_sub <- surv_sub[surv_sub$GRIDREF_TYPE %in% c("OSGB", "OSNI"), ] # 203211

# convert grid refs to lat/lon (also needed for temp extraction)
latlons <- gr2gps_latlon(gridref = surv_sub$GRIDREF, precision = surv_sub$PRECISION, projection = surv_sub$GRIDREF_TYPE)

# add lat lons into survey data table  
surv_sub <- cbind(surv_sub, latlons)

# gr2gps_latlon function doesn't seem to work on those rows where the precision = 2000 or 5000.
# remove rows where this is the case # 897 records with 2000, 31 with 5000
surv_sub <- surv_sub[!surv_sub$PRECISION == 2000 & !surv_sub$PRECISION == 5000, ] # 202283


## extract country information
# use polygons of uk boundaries to extract country information

# download maps
UKmap <- ne_states(geounit = c("England", "Scotland", "Wales", "Northern Ireland", "Ireland", "Isle of Man"), returnclass = "sv")
#plot(UKmap)

# Convert lat/lons to spatial points to use for data extraction ####
lb_xy <- vect(surv_sub, geom =c("LONGITUDE", "LATITUDE"))
#plot(lb_xy) # take a look

# extract information for those points from the polygon layer
pnt_info <- extract(x = UKmap, y = lb_xy)

# add country info into dataset
surv_sub$country <- pnt_info$geonunit

table(surv_sub$country)
# England          Ireland      Isle of Man Northern Ireland         Scotland            Wales 
#  182824             1158              562             1573             3788             6268 


# remove those from the Isle of Man and Ireland
surv_sub <- surv_sub[!surv_sub$country %in% c("Isle of Man", "Ireland"), ] # 200563

# reextract locations on this new subset
lb_xy <- vect(surv_sub, geom =c("LONGITUDE", "LATITUDE"))

# plot(UKmap)
# plot(lb_xy[surv_sub$country == "Northern Ireland"], add = T)

# subset the NAs for checking
countNA <- surv_sub[is.na(surv_sub$country),] # 6110 where country was not extracted

# check the points in the NA subset
test <- surv_sub[is.na(surv_sub$country), ]
test_xy <- vect(test, geom =c("LONGITUDE", "LATITUDE"))
# plot(UKmap)
# plot(test_xy, add = T)
# seem to be at the boundaries

### Try to get information for the NAs
# set crs for use in nearest function
crs(test_xy) <- crs(UKmap)

# only do this for the NAs as it classifies others incorrectly
nearpol <- nearest(test_xy, UKmap, centroids = T)

# create a new column for the info
surv_sub$neart_cntry_ID <- NA
# add in the extracted info IDs
surv_sub[is.na(surv_sub$country), "neart_cntry_ID"] <- nearpol$to_id # think zeros must be NA as there are 264 pos values

# where 0, convert to NA
surv_sub$neart_cntry_ID[surv_sub$neart_cntry_ID == 0] <- NA

# extract the country name from the UKmap 
surv_sub$country[!is.na(surv_sub$neart_cntry_ID)]  <- UKmap$geonunit[surv_sub$neart_cntry_ID[!is.na(surv_sub$neart_cntry_ID)]]



### still a few points that it says are in Wales but are actually in England

# plot(UKmap)
# plot(lb_xy[surv_sub$country == "Wales"], add = T)

# I identified the 6 points that had somehow been assigned as Wales when they were in England
test <- surv_sub[which(surv_sub$country == "Wales" & surv_sub$VC %in% c(4, 5)), ]
lb_xy_test <- vect(test, geom =c("LONGITUDE", "LATITUDE"))
plot(UKmap)
plot(lb_xy_test, add = T)

# reasign them as Wales
surv_sub[which(surv_sub$country == "Wales" & surv_sub$VC %in% c(4, 5)), "country"] <- "England"
# 200563



## organise species names ##

length(unique(surv_sub$NAME)) #81

# subset to those species of interest from the irecord data, plus nephus bisignatus
surv_sub <- surv_sub[which(surv_sub$NAME %in% sp_names_irec | surv_sub$NAME %in% c("Nephus bisignatus")), ] # 200053
length(unique(surv_sub$NAME)) #51

# remove additional species not covered by our database
surv_sub <- surv_sub[!surv_sub$NAME %in% add_sp, ] # 200039
length(unique(surv_sub$NAME)) #48

#table(surv_sub$NAME)

# save the processed survey data
write.csv(surv_sub, paste0(outdir, "/Ladybird_Survey_data_processed_UK.csv"), row.names = F)
#surv_sub <- read.csv(paste0(outdir, "/Ladybird_Survey_data_processed_UK.csv"))



### combine the two datasets ###

# first subset to required columns
surv_dat <- surv_sub[ , c(2:4, 12, 13:16)]
irec_dat <- irecord_sub[ , c(8, 9, 11, 15:17, 24)]

# reorder colummns
surv_dat <- surv_dat[ , c(1, 6:7, 3, 5, 4, 8)]
irec_dat <- irec_dat[ , c(7, 6, 4, 3, 2, 1, 5)]

# rename columns (make them match irecord names as remaining code uses that format)
names(surv_dat) <- names(irec_dat)

# combine datasets
lb_dat <- rbind(surv_dat, irec_dat)
nrow(lb_dat) # 346235 rows


## final subsetting ##

# Only use records from 1970 onwards
lb_dat <- lb_dat[lb_dat$year.processed >= 1970, ]
# 344333 rows
lb_dat <- lb_dat[lb_dat$year.processed <= 2023, ]
# 343716 rows

# save the organised dataset
write.csv(lb_dat, paste0(outdir, "/Ladybird_occurrences_processed_UK_ALL.csv"), row.names = F)
#lb_dat <- read.csv(paste0(outdir, "/Ladybird_occurrences_processed_UK_ALL.csv"))












