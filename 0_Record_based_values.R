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

# set directories
datadir <- "0_Data/"
outdir <- "1_Species_record_summaries/"
if(!dir.exists(outdir)) dir.create(outdir)

# load in the data from the NBN
ladybird_survey <- read.csv(paste0(datadir, "LadybirdSurvey/records-2024-03-16_LadybirdSurvey.csv"))
irecord_ladybirds <- read.csv(paste0(datadir, "iRecord_Ladybirds/records-2024-03-16_-_iRecord_Ladybirds.csv"))

# combine datasets
summary(names(ladybird_survey) == names(irecord_ladybirds)) # column names the same
lb_dat <- rbind(ladybird_survey, irecord_ladybirds)

names(lb_dat)

# subset to desired columns
lb_dat <- lb_dat[, c(17, 18, 30, 38, 44, 45, 66, 71:73, 87, 95, 111, 129, 130, 138, 141, 150, 155, 163, 179, 190, 191, 194, 205, 211:217, 220:229)]

# a few summaries of the remaining
nrow(lb_dat) # 278964 observations
table(lb_dat$basisOfRecord.processed) # all human observation
table(lb_dat$lifeStage) # a lot of options here, will need processing
table(lb_dat$occurrenceStatus.processed) # all present
range(as.Date(lb_dat$eventDate), na.rm = T) #"1834-02-07" "2024-02-27"
range(lb_dat$year.processed, na.rm = T) # 1600 2024
range(lb_dat$eventDate)
table(lb_dat$country.processed)
#                United Kingdom 
#           1135         277829 
summary(is.na(lb_dat$coordinatePrecision)) # all NAs
length(unique(lb_dat$gridReference)) # 51905 unique grid refs
length(unique(lb_dat$scientificName.processed)) 66
table(lb_dat$taxonRank.processed)
# family              form             genus           species species aggregate 
#    146              7110               153            271260               295 

table(lb_dat$stateProvince.processed)

### subsetting the data ###

# Select only those to the species level
lb_dat <- lb_dat[lb_dat$taxonRank.processed == "species", ] # 271260
length(unique(lb_dat$scientificName.processed)) # 54

# take a look at the species names
sp_names <- unique(lb_dat$scientificName.processed)
sp_names[order(sp_names)]

# seem to have some additional species in the dataset

# "Cryptolaemus montrouzieri"  # 7 records
# "Exochomus nigromaculatus"  # records
# "Oenopia conglobata" # 3 records
# "Rhyzobius forestieri" # 216 records
# "Rodolia cardinalis" # 7 records
# "Scymnus rubromaculatus" # 9 records
# "Vibidia duodecimguttata" # 2 records

# separate names in case species removed later
add_sp <- c("Cryptolaemus montrouzieri", "Exochomus nigromaculatus", "Oenopia conglobata",
            "Rhyzobius forestieri", "Rodolia cardinalis", "Scymnus rubromaculatus",
            "Vibidia duodecimguttata")


# Select records with complete date information
lb_dat <- lb_dat[!lb_dat$eventDate.processed == "", ] # 263890

# only use data from 1970 onwards
table(lb_dat$year.processed)
hist(lb_dat$year.processed)
lb_dat <- lb_dat[lb_dat$year.processed >= 1970, ] # 262732

# Select records for Great Britain only
lb_dat <- lb_dat[lb_dat$stateProvince.processed %in% c("England", "Scotland", "Wales"), ] # 259281

# save the organised dataset
write.csv(lb_dat, paste0(outdir, "/GB_ladybird_occurrences_processed.csv"))

############################################################
#                                                          #
#              Summarising records by species              #
#                                                          #
############################################################

# Now we summarise the records per species to determine information for the 
# trait database.

# create a table with a row per species to save required information
sp_tab <- data.frame(species = sort(unique(lb_dat$scientificName.processed))) # 53 species
#sp_tab <- read.csv(paste0(outdir, "Species_record_Summaries.csv"))

#### Convert lat/lons to spatial points to use for data extraction ####

lb_xy <- vect(lb_dat, geom =c("decimalLongitude.processed", "decimalLatitude.processed"))
#plot(lb_xy) # take a look



##%######################################################%##
#                                                          #
####           Number of records per species            ####
#                                                          #
##%######################################################%##

# count number of records per species
vals <- as.data.frame(table(lb_dat$scientificName.processed))

# change column names
names(vals) <- c("species", "n_records")

# merge sp_tab with vals
sp_tab <- merge(sp_tab, vals)



##%######################################################%##
#                                                          #
####             Year of most recent record             ####
#                                                          #
##%######################################################%##



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


# This section determines entries for the following columns: Present_England, 
# Present_Wales, Present_Scotland. 

sp_tab$nrecs_England <- NA
sp_tab$Present_England <- NA

sp_tab$nrecs_Wales <- NA
sp_tab$Present_Wales <- NA

sp_tab$nrecs_Scotland <- NA
sp_tab$Present_Scotland <- NA

for(i in 1:nrow(sp_tab)){
  # i <- 1
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
# This uses layers of the BNG, these were downloaded from here:
# https://github.com/OrdnanceSurvey/OS-British-National-Grids


# Some records have lat/lons but do not have GB grid references, 
# need to fill the gaps first.

# convert lat/lons to GB grid refs, uses the sparta package
lb_dat$ConvertGridRef <- gps_latlon2gr(latitude = lb_dat$decimalLatitude.processed, 
                                         longitude = lb_dat$decimalLongitude.processed, 
                                         out_projection = "OSGB", 
                                         return_type = "gr")[,1]

# there are layers at multiple scales
st_layers(paste0(datadir, "OS-British-National-Grids-main/os_bng_grids.gpkg"))

# read in the 
BNG <- st_read(paste0(datadir, "OS-British-National-Grids-main/os_bng_grids.gpkg"), layer = "10km_grid")

# convert to a terra vector object
BNG <- vect(BNG)

# reproject so the grid is in the same format as the points (tried the other way around and got it didn't work)
BNG <- project(x = BNG, y = "+proj=longlat +datum=WGS84")

# extract the 10km  grid refs for the ladybird data
lb_dat$GridRef10km <- extract(BNG, y = lb_xy)[,2]

# set up space in the table
sp_tab$n_10k <- NA

# now for each species, count how many unique 10km grid refs have records
for(i in 1:nrow(sp_tab)){
  # i <- 1
  # subset to each species 
  sp_recs <- lb_dat[lb_dat$scientificName.processed == sp_tab[i, 1], ]
  
  # determine number of 10km grid squares
  n_10k <- length(unique(sp_recs$GridRef10km))
  
  # add results into table
  sp_tab[i, "n_10k"] <- n_10k
    
}


# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries.csv"), row.names = F)



############################################################
#                                                          #
#                 Number of vicecounties                   #
#                                                          #
############################################################

# This section counts the number of vicecounties that each species has records in. 
# shapefiles of the vicecounties are from the Biological Records Centre resources webpage.

# load in the polygons
vice <- vect(x = paste0(datadir, "vice-counties-master/3-mile/County_3mile_region.shp"))
plot(vice)

# reproject the polygons to match the points
vice <- project(x = vice, y = "+proj=longlat +datum=WGS84")

# extract vice country info from the polygons
lb_dat$vicecounty <- extract(vice, lb_xy)[, 3]

# set up space in the table
sp_tab$n_vicecounties <- NA

# now for each species, count how many unique vicecounties the records are in
for(i in 1:nrow(sp_tab)){
  # i <- 1
  # subset to each species 
  sp_recs <- lb_dat[lb_dat$scientificName.processed == sp_tab[i, 1], ]
  
  # determine number of 10km grid squares
  n_vice <- length(unique(sp_recs$vicecounty))
  
  # add results into table
  sp_tab[i, "n_vicecounties"] <- n_vice
  
}


# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries.csv"), row.names = F)



  
############################################################
#                                                          #
#          Month of first, last and peak records           #
#                                                          #
############################################################


# This section uses the records to determine when the first, last and peak of 
# observations are. We assume that most records are of adults and so results
# here will reflect the adult peaks. Larvae peaks have been determined by PB. 

sp_tab$Adult_first <- NA
sp_tab$Adult_last <- NA
sp_tab$Adult_peak1 <- NA
sp_tab$Adult_peak2 <- NA


for(i in 1:nrow(sp_tab)){
  # i <- 1
  
  # subset to each species 
  sp_recs <- lb_dat[lb_dat$scientificName.processed == sp_tab[i, 1], ]
  
  # identify month of first record
  sp_tab[sp_tab$species == sp_tab[i, 1], "Adult_first"] <- min(sp_recs$month.processed)
  
  # identify month of last record
  sp_tab[sp_tab$species == sp_tab[i, 1], "Adult_last"] <- max(sp_recs$month.processed)
  
  
  # determine peaks
  
  # frequency table of months
  months_freq <- sp_recs %>% 
    group_by(month.processed) %>% tally()
  
    # get the first month with the most records
  sp_tab[sp_tab$species == sp_tab[i, 1], "Adult_peak1"] <- grep(TRUE, peaks(months_freq$n))[1]
  
  # get the second month with the most records
  sp_tab[sp_tab$species == sp_tab[i, 1], "Adult_peak2"] <- grep(TRUE, peaks(months_freq$n))[2]
  
}


# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries.csv"), row.names = F)



# create histograms for each species to refer to

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
    ggtitle(paste0("Frequency of records by month - ", sp_tab[i, 1]))
  
ggsave(paste0(outdir, "Histograms/", sp_tab[i, 1], ".png"))

}




