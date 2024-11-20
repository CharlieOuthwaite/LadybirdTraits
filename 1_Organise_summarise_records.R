############################################################
#                                                          #
#                Summarise ladybird records                #
#                                                          #
############################################################

# March 2024, Charlie Outhwaite

# This script is used to summarise the distribution of species records. 
# Column in the trait database linked to each section of the script is stated. 

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
#ladybird_survey <- read.csv(paste0(datadir, "LadybirdSurvey/records-2024-03-16_LadybirdSurvey.csv")) # this dataset doesn't include data after 2011
ladybird_survey <- read.csv(paste0(datadir, "LadybirdSurvey/BRC_Ladybirds_18-11-2024.csv")) # updated dataset from the BRC
irecord_ladybirds <- read.csv(paste0(datadir, "iRecord_Ladybirds/records-2024-03-16_-_iRecord_Ladybirds.csv"))

# ladybird_survey 168245 records, 75 columns
# irecord_ladybirds 155365 records, 204 columns


# need to organise the datasets so that they can be combined, currently different columns, 
# some missing info inc lat/lon that needs to be determined.


#### 1. Data explorations ####


## explore ladybird survey data ##

# how many records have same start and end date?
summary(ladybird_survey$STARTDATE == ladybird_survey$ENDDATE)
#           FALSE    TRUE 
# logical   13724  154521 

length(unique(ladybird_survey$NAME)) # 54
length(unique(ladybird_survey$CONCEPT)) # 54
length(unique(ladybird_survey$GRIDREF)) # 48405
table(ladybird_survey$DATE_TYPE)
# Date specified to a number of days                     Date specified to nearest year 
#                               1359                                               6730 
# Date specified to range of months                   Date specified to range of years 
#                               214                                               2773 
# Date specified to the nearest day   Date specified to the nearest month (1st - last) 
#                            154233                                               2183 
# No date or unknown, combined with dataset end date           Only the end date to nearest year known. 
#                               299                                                454


# convert dates
ladybird_survey$STARTDATE <- as.Date(ladybird_survey$STARTDATE, format = "%d/%m/%Y")
ladybird_survey$ENDDATE <- as.Date(ladybird_survey$ENDDATE, format = "%d/%m/%Y")

# extract year of sample
ladybird_survey$year_start <- format(ladybird_survey$STARTDATE, "%Y")
ladybird_survey$year_end <- format(ladybird_survey$ENDDATE, "%Y")
summary(ladybird_survey$year_start == ladybird_survey$year_end)
#    Mode   FALSE    TRUE    NA's 
# logical    2890  164602     753 

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
# 164719 rows

# keep records for 1970 onward
lb_surv <- lb_surv[lb_surv$year_start >= 1970, ]
# 161890 rows

# subset to those with same start and end year
lb_surv <- lb_surv[lb_surv$year_start == lb_surv$year_end, ]
# 161777

# keep records for 1970 onward
lb_irec <- irecord_sub[irecord_sub$year.processed >= 1970, ]
# 155358 rows

# just extract the years for plotting
plotdata <- data.frame(year = c(as.numeric(lb_irec$year.processed), as.numeric(lb_surv$year_end)))
plotdata <- subset(plotdata, year >= 1970 & year <= 2023)


#### Figure of number of records over time ####

ggplot(data = plotdata) + 
  geom_bar(aes(x = year), fill = c("#8B2323")) + 
  xlab("Year") +
  ylab("Number of records") + 
  theme_bw() +
  theme(text = element_text(size = 10))

# save
ggsave(filename = paste0(outdir, "/Fig1_nrecords_year.pdf"), 
       dpi = 250, width = 4, height = 3, units = "in")




#### 2. Organise data so datasets can be combined ####

# UK ladybird survey data is organised slightly differently to the NBN data for iRecord.


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

# just select records where date to the nearest day
surv_sub <- surv_sub[surv_sub$DATE_TYPE == c("Date specified to the nearest day"), ]
# 154233 rows

# summary(surv_sub$year_start == surv_sub$year_end)

# still some where the start and end dates are different
surv_sub <- surv_sub[surv_sub$STARTDATE == surv_sub$ENDDATE, ] # 154155

# extract months
surv_sub$month <- format(surv_sub$STARTDATE, "%m")




# organise column names

# save the organised dataset
write.csv(lb_dat, paste0(outdir, "/Ladybird_occurrences_processed_UK.csv"))



############################################################
#                                                          #
#              Summarising records by species              #
#                                                          #
############################################################

# Now we summarise the records per species to determine information for the 
# trait database.

# create a table with a row per species to save required information
sp_tab <- data.frame(species = sort(unique(lb_dat$scientificName.processed))) # 48 species
#sp_tab <- read.csv(paste0(outdir, "Species_record_Summaries.csv"))



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


# Some records have lat/lons but do not have GB grid references, 
# need to fill the gaps first.
# 
# # convert lat/lons to GB grid refs, uses the sparta package
# sub_dat <- lb_dat[!lb_dat$stateProvince.processed == "Northern Ireland", ]
# lb_dat$ConvertGridRef[!lb_dat$stateProvince.processed == "Northern Ireland"] <- gps_latlon2gr(latitude = sub_dat$decimalLatitude.processed, 
#                                                                                               longitude = sub_dat$decimalLongitude.processed, 
#                                                                                               out_projection = "OSGB", 
#                                                                                               return_type = "gr")[,1]
# 
# sub_dat <- lb_dat[lb_dat$stateProvince.processed == "Northern Ireland", ]
# lb_dat$ConvertGridRef[lb_dat$stateProvince.processed == "Northern Ireland"] <- gps_latlon2gr(latitude = sub_dat$decimalLatitude.processed, 
#                                                                                               longitude = sub_dat$decimalLongitude.processed, 
#                                                                                               out_projection = "OSNI", 
#                                                                                               return_type = "gr")[,1]

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
plot(vice_GB)

# NI
vice_NI <- vect(x = paste0(datadir, "Irish-Vice-Counties-master/vice_counties/All_Irish_Vice_Counties_irish_grid.shp"))
plot(vice_NI)

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
  
ggsave(paste0(outdir, "Histograms/", sp_tab[i, 1], ".png"))

}



# save the dataset with all added variables
write.csv(lb_dat, paste0(outdir, "/Ladybird_occurrences_processed_UK_Allvars.csv"))



