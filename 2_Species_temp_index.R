############################################################
#                                                          #
#        Determine species temperature preferences         #
#                                                          #
############################################################

# This script uses WorldClim data and species occurrence records to 
# determine the species temperature index for each species

# worldclim data downloaded at the 30 second resolution from:
# https://www.worldclim.org/data/worldclim21.html
# average temperature only.

# clear environment
rm(list = ls())

# load libraries
library(terra)


# set directories
datadir <- "0_Data/"
lbdir <- "1_Species_record_summaries/"
outdir <- "2_Species_temp_index/"
if(!dir.exists(outdir)) dir.create(outdir)


# read in record based values table for peak months 
sp_tab <- read.csv(paste0(lbdir, "Species_record_Summaries.csv"))



#### 1. GB-based STI ####

# load in GB ladybird data
lb_dat <- read.csv(paste0(lbdir, "GB_ladybird_occurrences_processed.csv"))
# this is the data from Script 0_ where GB data from 1970 have been processed

# load in the climate data
# downloaded from https://www.worldclim.org/data/worldclim21.html#google_vignette
# 10 minute resolution
temp_files <- list.files(paste0(datadir, "/WorldClim_version_2_1/"), full.names = T)

# read in a stack of all of the temp files (one for each month of the year)
temp_all <- rast(temp_files)
# plot(temp_all) # take a look

# get the average temperature for the year
ann_mean_temp <- mean(temp_all)
# plot(ann_mean_temp) # take a look


#### First, STI determined using all months ####

# for each species, extract the values for point locations,
# take the average across those points and save as the GB STI

# vector of species names
species <- sp_tab$species

# create space to save results in sp_tab
sp_tab$GB_STI_yr <- NA

# sp <- species[1]
for (sp in species){
  
  sp_dat <- lb_dat[lb_dat$scientificName.processed == sp, ]
  
  # create point locations 
  sp_xy <- vect(sp_dat, geom = c("decimalLongitude.processed", "decimalLatitude.processed"))
  
  # extract values from the annual mean temperature map
  # take the mean to get the species level STI
  sp_STI <- mean(extract(ann_mean_temp, sp_xy, ID = F)[, 1], na.rm = T)
  
  # add result into the sp_tab
  sp_tab[sp_tab$species == sp, "GB_STI_yr"] <- sp_STI
  
}

# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries.csv"), row.names = F)


#### Second, STI determined using just peak month ####

# sp <- species[1]
for (sp in species){
  
  sp_dat <- lb_dat[lb_dat$scientificName.processed == sp, ]
  
  # create point locations 
  sp_xy <- vect(sp_dat, geom = c("decimalLongitude.processed", "decimalLatitude.processed"))
  
  # use peak month for adults determined from the records
  
  
  
  
  
  # extract values from the annual mean temperature map
  # take the mean to get the species level STI
  sp_STI <- mean(extract(ann_mean_temp, sp_xy, ID = F)[, 1], na.rm = T)
  
  # add result into the sp_tab
  sp_tab[sp_tab$species == sp, "GB_STI_yr"] <- sp_STI
  
}

# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries.csv"), row.names = F)






#### 2. Europe-based STI ####