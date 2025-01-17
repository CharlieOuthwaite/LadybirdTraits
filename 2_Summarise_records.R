############################################################
#                                                          #
#                Summarise ladybird records                #
#                                                          #
############################################################

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
lbdatadir <- "0_Data/processed/"
datadir <- "0_Data/"
outdir <- "1_Species_record_summaries/UK/"
if(!dir.exists(outdir)) dir.create(outdir)

# load in the processed data
lb_dat <- read.csv(paste0(lbdatadir, "/Ladybird_occurrences_processed_UK_ALL.csv")) # 343716 rows



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

# create spatial points for later use
# reset lat/lon 
lb_xy <- vect(lb_dat, geom =c("decimalLongitude.processed", "decimalLatitude.processed"))

# UKmap <- ne_states(geounit = c("England", "Scotland", "Wales", "Northern Ireland", "Ireland", "Isle of Man"), returnclass = "sv")
# plot(UKmap)
# plot(lb_xy, add = T)

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

plot(BNG)
plot(lb_xy, add = T)

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

# NI
vice_NI <- vect(x = paste0(datadir, "Irish-Vice-Counties-master/vice_counties/All_Irish_Vice_Counties_irish_grid.shp"))

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

# take a look
# plot(vice_GB)
# plot(lb_xy_GB, add = T)
# plot(vice_NI)
# plot(lb_xy_NI, add = T)

# extract vice county info from the polygons
# this is the slowest step
lb_dat$vicecounty[!lb_dat$stateProvince.processed == "Northern Ireland"] <- extract(vice_GB, lb_xy_GB)[, 3]
lb_dat$vicecounty[lb_dat$stateProvince.processed == "Northern Ireland"] <- extract(vice_NI, lb_xy_NI)[, 4]


# now for each species, count how many unique vice counties
sp_tab$n_vicecounties <- lb_dat %>%
  group_by(scientificName.processed) %>% # group by species name
  summarise(n_vicecounties = length(na.omit(unique(vicecounty)))) %>% # summarise the number of unique vice counties
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
  
  print(i)
  
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
  sp_tab[sp_tab$species == sp_tab[i, 1], "Max_month"] <- months_freq[months_freq$n == max(months_freq$n, na.rm = T), "month.processed"][1]
  
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



