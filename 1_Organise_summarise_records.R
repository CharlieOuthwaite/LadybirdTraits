############################################################
#                                                          #
#                Summarise ladybird records                #
#                                                          #
############################################################

# March 2024, Charlie Outhwaite

# This script is used to summarise the distribution of species records. 
# Column in the trait database linked to each section of the script is stated. 
# Since data from ladybird survey and irecord are formatted differently, 
# processing of the datasets is required first.

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
outdir <- "1_Species_record_summaries/UK/"
if(!dir.exists(outdir)) dir.create(outdir)

# load in the data from the NBN
#survnbn <- read.csv(paste0(datadir, "LadybirdSurvey/records-2024-03-16_LadybirdSurvey.csv")) # this dataset doesn't include data after 2011
ladybird_survey <- read.csv(paste0(datadir, "LadybirdSurvey/Coccinellid_ladybird_non-ladybird_21-11-2024.csv")) # updated dataset from the BRC. excluding iRecord
irecord_ladybirds <- read.csv(paste0(datadir, "iRecord_Ladybirds/records-2024-03-16_-_iRecord_Ladybirds.csv")) # in recent years, all verified records are via iRecord

# ladybird_survey 220647 records, 77 columns
# irecord_ladybirds 155365 records, 204 columns


# need to organise the datasets so that they can be combined, 
# currently the data from BRC is formatted very differently to that from iRecord via the NBN
# There is some missing info inc lat/lon that needs to be determined.


##%######################################################%##
#                                                          #
####               1. Data explorations                 ####
#                                                          #
##%######################################################%##



## explore ladybird survey data ##

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


# convert dates
ladybird_survey$STARTDATE <- as.Date(ladybird_survey$STARTDATE, format = "%d/%m/%Y")
ladybird_survey$ENDDATE <- as.Date(ladybird_survey$ENDDATE, format = "%d/%m/%Y")

# extract year of sample
ladybird_survey$year_start <- format(ladybird_survey$STARTDATE, "%Y")
ladybird_survey$year_end <- format(ladybird_survey$ENDDATE, "%Y")
summary(ladybird_survey$year_start == ladybird_survey$year_end)
#    Mode   FALSE    TRUE    NA's 
# logical    4098  215332    1217 

table(ladybird_survey$year_start) # 1600 - 2021
table(ladybird_survey$year_end) # 1816 - 2021



## explore irecord data ##

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


length(unique(irecord_sub$scientificName.processed)) #63


### Create plot of n records over time from both datasets ###

# limited filtering here, just presenting increase in records over time 

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


# UK ladybird survey data is organised differently to the NBN data from iRecord.


## irecord data ##

# Select only those to the species level
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
irecord_sub <- irecord_sub[!irecord_sub$scientificName.processed %in% add_sp, ] ## 146196 UK excl IoM.

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

# # Just take those points that fall within the country polygons
### commented out in the end as some were very close to boundary but were dropped. 
# # convert to terra object
# UK <- vect(UK)
# 
# # extract information for those points from the polygon layer
# pnt_info <- extract(x = UK, y = lb_xy)
# 
# # remove those that have NA within the name field, so outside polygons
# pnt_info <- pnt_info[!is.na(pnt_info$name_en), ]
# 
# # check the points subset
# ggplot() +
#   geom_sf(data = UK, fill = NA, col = "black") +
#   geom_spatvector(data = lb_xy[pnt_info$id.y])
# 
# 
# # subset data to the points within country polygons only
# lb_dat <- lb_dat[pnt_info$id.y, ] # 256571 rows
# 
# # recreate subset of point locations for later summaries
# lb_xy <- vect(lb_dat, geom =c("decimalLongitude.processed", "decimalLatitude.processed"))
# #plot(lb_xy) # take a look
# 




## UK Ladybird Survey data ##

# subset to relevant columns
surv_sub <- ladybird_survey[ , c(2:6, 9:10, 13, 15, 17)]

# extract years and months from dates
surv_sub$year_start <- format(surv_sub$STARTDATE, "%Y")
surv_sub$year_end <- format(surv_sub$ENDDATE, "%Y")

# summary(surv_sub$year_start == surv_sub$year_end)
# Mode   FALSE    TRUE    NA's 
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

# table(surv_sub$PRECISION)

# remove records where the grid ref type is not OSGB or OSNI
surv_sub <- surv_sub[surv_sub$GRIDREF_TYPE %in% c("OSGB", "OSNI"), ] # 203211

# convert grid refs to lat/lon (also needed for temp extraction)
latlons <- gr2gps_latlon(gridref = surv_sub$GRIDREF, precision = surv_sub$PRECISION, projection = surv_sub$GRIDREF_TYPE)

# add lat lons into survey data table  
surv_sub <- cbind(surv_sub, latlons)

# this function doesn't seem to work on those rows where the precision = 2000 or 5000.
# remove rows where this is the case # 897 records with 2000, 31 eith 5000
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

# remove those from the Isle of Man
surv_sub <- surv_sub[!surv_sub$country %in% c("Isle of Man"), ] # 201721

# reset lat/lon 
lb_xy <- vect(surv_sub, geom =c("LONGITUDE", "LATITUDE"))

# subset the NAs for checking
countNA <- pnt_info[is.na(pnt_info$geonunit), ] # 6110

# check the points in the NA subset
plot(UKmap)
plot(lb_xy[countNA$id.y], add = T)
# most seem to be close to a boundary

### Try to get information for the NAs
# set crs for use in nearest function
crs(lb_xy) <- crs(UKmap)

# only do this for the NAs as it classifies others incorrectly
nearpol <- nearest(lb_xy[countNA$id.y], UKmap, centroids = T)

# create a new column for the info
surv_sub$neart_cntry_ID <- NA
# add in the extracted info IDs
surv_sub[countNA$id.y, "neart_cntry_ID"] <- nearpol$to_id # think zeros must be NA as there are 264 pos values

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
# 202226

# remove records from Ireland
surv_sub <- surv_sub[which(!surv_sub$country == "Ireland"), ] # 194882


test <- ladybird_survey[which(ladybird_survey$NAME == "Nephus bisignatus" & ladybird_survey$STARTDATE == "09/05/1996"), ]
ll <- gr2gps_latlon(gridref = test$GRIDREF, precision = test$PRECISION, projection = test$GRIDREF_TYPE)

lb_xy <- vect(ll, geom =c("LONGITUDE", "LATITUDE"))
plot(UKmap)
plot(lb_xy, add = T)





## organise species names ##

length(unique(surv_sub$NAME)) #81

surv_sub <- surv_sub[which(surv_sub$NAME %in% sp_names_irec | surv_sub$NAME %in% c("Nephus bisignatus")), ] # 153665
length(unique(surv_sub$NAME)) #51

# remove additional species not covered by our database
surv_sub <- surv_sub[!surv_sub$NAME %in% add_sp, ]
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
nrow(lb_dat) # 340541 rows


## final subsetting ##

# Only use records from 1970 onwards
lb_dat <- lb_dat[lb_dat$year.processed >= 1970, ]
# 338769 rows
lb_dat <- lb_dat[lb_dat$year.processed <= 2023, ]
# 338152 rows

# save the organised dataset
write.csv(lb_dat, paste0(outdir, "/Ladybird_occurrences_processed_UK_ALL.csv"), row.names = F)
#lb_dat <- read.csv(paste0(outdir, "/Ladybird_occurrences_processed_UK_ALL.csv"))



############################################################
#                                                          #
#           3. Summarising records by species              #
#                                                          #
############################################################

# Now we summarise the records per species to determine information for the 
# trait database.

# create a table with a row per species to save required information
sp_tab <- data.frame(species = sort(unique(lb_dat$scientificName.processed))) # 48 species
#sp_tab <- read.csv(paste0(outdir, "Species_record_Summaries.csv"))

# create spatial points for later use
# reset lat/lon 
lb_xy <- vect(lb_dat, geom =c("decimalLongitude.processed", "decimalLatitude.processed"))

##%######################################################%##
#                                                          #
####           Number of records per species            ####
#                                                          #
##%######################################################%##

# How many records are in the dataset for each species?

# count number of records per species
vals <- as.data.frame(table(lb_dat$scientificName.processed))

# change column names
names(vals) <- c("species", "n_records")

# merge sp_tab with vals
sp_tab <- merge(sp_tab, vals)



##%######################################################%##
#                                                          #
####                 Year of first record               ####
#                                                          #
##%######################################################%##

# What is the year of the first record for each species?

# get the minimum year processed, grouping by species
yr_tab <- lb_dat %>% group_by(scientificName.processed)  %>% 
  summarise(yr_first = min(year.processed))

# change column names
names(yr_tab) <- c("species", "yr_first")

# merge sp_tab with vals
sp_tab <- merge(sp_tab, yr_tab)

# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries.csv"), row.names = F)



##%######################################################%##
#                                                          #
####             Year of most recent record             ####
#                                                          #
##%######################################################%##

# What is the year of the most recent record for each species?

# get the max year processed, grouing by species
yr_tab <- lb_dat %>% group_by(scientificName.processed)  %>% 
  summarise(yr_recent = max(year.processed))

# change column names
names(yr_tab) <- c("species", "Most_recent_yr")

# merge sp_tab with vals
sp_tab <- merge(sp_tab, yr_tab)

# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries.csv"), row.names = F)


############################################################
#                                                          #
#                    Country presence                      #
#                                                          #
############################################################

# This section determines presence and the number of records in England, 
# Wales and Scotland. 

# create empty spaces
sp_tab$nrecs_England <- NA
sp_tab$Present_England <- NA

sp_tab$nrecs_Wales <- NA
sp_tab$Present_Wales <- NA

sp_tab$nrecs_Scotland <- NA
sp_tab$Present_Scotland <- NA

sp_tab$nrecs_NorthernIreland <- NA
sp_tab$Present_NorthernIreland <- NA

# for each species, determine the number of records in each country
for(i in 1:nrow(sp_tab)){
  # i <- 1
  # subset to the records for the species of interest
  sp_recs <- lb_dat[lb_dat$scientificName.processed == sp_tab[i, 1], ]
  
  if("England" %in% sp_recs$stateProvince.processed) {
    
    sp_tab[i, "nrecs_England"] <- nrow(sp_recs[sp_recs$stateProvince.processed == "England", ])
    sp_tab[i, "Present_England"] <- 1
    
  }else{
    
    sp_tab[i, "nrecs_England"] <- 0
    sp_tab[i, "Present_England"] <- 0
    
  }
  
  if("Wales" %in% sp_recs$stateProvince.processed) {
    
    sp_tab[i, "nrecs_Wales"] <- nrow(sp_recs[sp_recs$stateProvince.processed == "Wales", ])
    sp_tab[i, "Present_Wales"] <- 1
    
  }else{
    
    sp_tab[i, "nrecs_Wales"] <- 0
    sp_tab[i, "Present_Wales"] <- 0
    
  }
  
  if("Scotland" %in% sp_recs$stateProvince.processed) {
    
    sp_tab[i, "nrecs_Scotland"] <- nrow(sp_recs[sp_recs$stateProvince.processed == "Scotland", ])
    sp_tab[i, "Present_Scotland"] <- 1
    
  }else{
    
    sp_tab[i, "nrecs_Scotland"] <- 0
    sp_tab[i, "Present_Scotland"] <- 0
    
  }
  
  if("Northern Ireland" %in% sp_recs$stateProvince.processed) {
    
    sp_tab[i, "nrecs_NorthernIreland"] <- nrow(sp_recs[sp_recs$stateProvince.processed == "Northern Ireland", ])
    sp_tab[i, "Present_NorthernIreland"] <- 1
    
  }else{
    
    sp_tab[i, "nrecs_NorthernIreland"] <- 0
    sp_tab[i, "Present_NorthernIreland"] <- 0
    
  }
  
  
}

# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries.csv"), row.names = F)


############################################################
#                                                          #
#        Number of 10km grid squares with records          #
#                                                          #
############################################################


# This section counts the number of 10km grid squares that each species has
# records in. GB grid squares only.
# This uses layers of the British National Grid, these were downloaded from here:
# https://github.com/OrdnanceSurvey/OS-British-National-Grids


# there are layers at multiple scales
st_layers(paste0(datadir, "OS-British-National-Grids-main/os_bng_grids.gpkg"))

# read in the 
BNG <- st_read(paste0(datadir, "OS-British-National-Grids-main/os_bng_grids.gpkg"), layer = "10km_grid")

# convert to a terra vector object
BNG <- vect(BNG)

# reproject so the grid is in the same format as the points (tried the other way around and it didn't work)
BNG <- project(x = BNG, y = "+proj=longlat +datum=WGS84")

# plot(BNG)
# plot(UK, add = T)
# plot(lb_xy, add = T)

# extract the 10km grid refs for the ladybird data
lb_dat$GridRef10km <- extract(BNG, y = lb_xy)[,2]

# set up space in the table
sp_tab$n_10k <- NA

# now for each species, count how many unique 10km grid refs have records
sp_tab$n_10k <- lb_dat %>%
  group_by(scientificName.processed) %>% # group by species name
  summarise(n_10k = length(unique(GridRef10km))) %>% # summarise the number of unique grid refs
  pull(n_10k) # select just the one column to add to other table

# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries.csv"), row.names = F)



############################################################
#                                                          #
#                 Number of vice-counties                  #
#                                                          #
############################################################

# This section counts the number of vice-counties that each species has records in. 
# shapefiles of the vice-counties are from:
# GB - https://github.com/BiologicalRecordsCentre/vice-counties?tab=readme-ov-file
# NI - https://github.com/SK53/Irish-Vice-Counties

# load in the polygons
# GB
vice_GB <- vect(x = paste0(datadir, "vice-counties-master/3-mile/County_3mile_region.shp"))
# plot(vice_GB)
# plot(lb_xy_GB, add = T)

# NI
vice_NI <- vect(x = paste0(datadir, "Irish-Vice-Counties-master/vice_counties/All_Irish_Vice_Counties_irish_grid.shp"))
# plot(vice_NI)
# plot(lb_xy_NI, add = T)

# reproject the polygons to match the points
vice_GB <- project(x = vice_GB, y = "+proj=longlat +datum=WGS84")
vice_NI <- project(x = vice_NI, y = "+proj=longlat +datum=WGS84")

# separate out point locations for NI
lb_xy_GB <- vect(lb_dat[!lb_dat$stateProvince.processed == "Northern Ireland",], 
                 geom =c("decimalLongitude.processed", "decimalLatitude.processed"), 
                 crs = "+proj=longlat +datum=WGS84")
lb_xy_NI <- vect(lb_dat[lb_dat$stateProvince.processed == "Northern Ireland",], 
                 geom =c("decimalLongitude.processed", "decimalLatitude.processed"),
                 crs = "+proj=longlat +datum=WGS84")

# extract vice country info from the polygons
lb_dat$vicecounty[!lb_dat$stateProvince.processed == "Northern Ireland"] <- extract(vice_GB, lb_xy_GB)[, 3]
lb_dat$vicecounty[lb_dat$stateProvince.processed == "Northern Ireland"] <- extract(vice_NI, lb_xy_NI)[, 4]


# now for each species, count how many unique vice counties
sp_tab$n_vicecounties <- lb_dat %>%
  group_by(scientificName.processed) %>% # group by species name
  summarise(n_vicecounties = length(na.omit(unique(vicecounty)))) %>% # summarise the number of unique viecounties
  pull(n_vicecounties) # select just the one column to add to other table


# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries.csv"), row.names = F)



  
############################################################
#                                                          #
#          Month of first, last and peak records           #
#                                                          #
############################################################


# This section uses the records to determine when the first, last and peak of 
# observations are. Since most records are of adults we assume here that
# these will reflect the adult peaks. Larvae peaks have been determined by PB.
# We also get the highest peak (max_month) for use in calculating the GB STI.

sp_tab$Adult_first <- NA
sp_tab$Adult_last <- NA
sp_tab$Adult_peak1 <- NA
sp_tab$Adult_peak2 <- NA
sp_tab$Max_month <- NA

for(i in 1:nrow(sp_tab)){
  # i <- 1
  
  # subset to each species 
  sp_recs <- lb_dat[lb_dat$scientificName.processed == sp_tab[i, 1], ]
  
  # identify month of first record
  sp_tab[sp_tab$species == sp_tab[i, 1], "Adult_first"] <- min(sp_recs$month.processed)
  
  # identify month of last record
  sp_tab[sp_tab$species == sp_tab[i, 1], "Adult_last"] <- max(sp_recs$month.processed)
  
  
  # determine peaks
  
  # create empty dataframe to ensure all months have data even if no records
  months <- data.frame(month.processed  = 1:12)
  
  # frequency table of months
  months_freq <- sp_recs %>% 
    group_by(month.processed) %>% tally()
  
  # ensure all months have data by merging above with empty data frame
  months_freq <- merge(months, months_freq, by = "month.processed", all.x = T)
  
  # replace NAs with 0s
  months_freq[is.na(months_freq$n), "n"] <- 0
  
  # If there are fewer than 20 records for the species. Set the month with the 
  # max records and the peak months to NA.
  if(nrow(sp_recs) < 20){
    
    sp_tab[sp_tab$species == sp_tab[i, 1], "Max_month"] <- NA
    sp_tab[sp_tab$species == sp_tab[i, 1], "Adult_peak1"] <- NA
    sp_tab[sp_tab$species == sp_tab[i, 1], "Adult_peak2"] <- NA
  
  }else{
  
  # month with the most records
  sp_tab[sp_tab$species == sp_tab[i, 1], "Max_month"] <- months_freq[months_freq$n == max(months_freq$n, na.rm = T), "month.processed"]
  
    # get the first month with the most records
  sp_tab[sp_tab$species == sp_tab[i, 1], "Adult_peak1"] <- grep(TRUE, peaks(months_freq$n, span = 5))[1]
  
  # get the second month with the most records
  sp_tab[sp_tab$species == sp_tab[i, 1], "Adult_peak2"] <- grep(TRUE, peaks(months_freq$n, span = 5))[2]
  
}}


# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries.csv"), row.names = F)



### create histograms for each species to refer to for checking the peaks ###

for(i in 1:nrow(sp_tab)){
  # i <- 1
  
  # subset to each species 
  sp_recs <- lb_dat[lb_dat$scientificName.processed == sp_tab[i, 1], ]
  
  ggplot(sp_recs) + 
    geom_bar(aes(x = factor(month.processed)), 
             bins = 12, 
             col = "darkblue", 
             fill = "lightblue") + 
    #scale_x_continuous(limits = c(1, 12)) + 
    theme_light() + 
    ggtitle(paste0("Frequency of records by month - ", sp_tab[i, 1])) + 
    xlab("Month - January to December") +
    ylab("Count")
  
ggsave(paste0(outdir, "Histograms/", sp_tab[i, 1], "_UPDATE.png"))

}



# save the dataset with all added variables
write.csv(lb_dat, paste0(outdir, "/Ladybird_occurrences_processed_UK_Allvars.csv"), row.names = F)
#lb_dat <- read.csv(paste0(outdir, "/Ladybird_occurrences_processed_UK_Allvars.csv"))



