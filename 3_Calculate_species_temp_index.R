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
lbdir <- "1_Species_record_summaries/UK/"
outdir <- "3_Species_temp_index/"
if(!dir.exists(outdir)) dir.create(outdir)

# read in record based values table for peak months 
sp_tab <- read.csv(paste0(lbdir, "Species_record_Summaries.csv"))



##%######################################################%##
#                                                          #
####                  1. GB-based STI                   ####
#                                                          #
##%######################################################%##

# Here we calculated both the mean- and peak-based STIs for each species 
# using the records from GB. 

# load in GB ladybird data
lb_dat <- read.csv(paste0(lbdir, "Ladybird_occurrences_processed_UK.csv"))
# this is the data from Script 0_ where GB data from 1970 have been processed

# load in the climate data
# downloaded from https://www.worldclim.org/data/worldclim21.html#google_vignette
# 10 minute resolution
temp_files <- list.files(paste0(datadir, "/WorldClim_version_2_1/"), full.names = T)

# read in a stack of all of the temp files (one for each month of the year)
temp_all <- rast(temp_files)
# plot(temp_all) # take a look

# get the average temperature across all months
ann_mean_temp <- mean(temp_all)
# plot(ann_mean_temp) # take a look


#### First, STI determined using all months ####

# for each species, extract the values for point locations,
# take the average across those points and save as the GB STI

# vector of species names
species <- sp_tab$species

# create space to save results in sp_tab
sp_tab$UK_STI_yr <- NA

# sp <- species[1]
for (sp in species){
  
  # subset the data to the species of interest
  sp_dat <- lb_dat[lb_dat$scientificName.processed == sp, ]
  
  # create point locations 
  sp_xy <- vect(sp_dat, geom = c("decimalLongitude.processed", "decimalLatitude.processed"))
  
  # check 
  # plot(ann_mean_temp)
  # plot(sp_xy, add = T)
  
  # extract values from the annual mean temperature map
  # take the mean to get the species level STI
  sp_STI <- mean(extract(ann_mean_temp, sp_xy, ID = F)[, 1], na.rm = T)
  
  # add result into the sp_tab
  sp_tab[sp_tab$species == sp, "UK_STI_yr"] <- sp_STI
  
}


# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries_incSTI_UK.csv"), row.names = F)




#### Second, STI determined using just the max month ####

# create space to save results in sp_tab
sp_tab$UK_STI_peak <- NA


# sp <- species[27]
for(sp in species){
  
  # for testing
  #print(sp)
  
  sp_dat <- lb_dat[lb_dat$scientificName.processed == sp, ]
  
  # create point locations 
  sp_xy <- vect(sp_dat, geom = c("decimalLongitude.processed", "decimalLatitude.processed"))
  
  # use max month for adults determined from the records
  peak <- sp_tab$Max_month[sp_tab$species == sp]

  # one species don't have enough records for peaks
  if (is.na(peak) == T){  sp_tab[sp_tab$species == sp, "UK_STI_peak"] <- NA 
  
  }else{
  

   # load in the raster for the peak month
   temp_peak <- rast(temp_files[peak])
   
   # extract values from the temp map for the peak month
   # take the mean to get the species level STI
   sp_STI_peak <- mean(extract(temp_peak, sp_xy, ID = F)[, 1], na.rm = T)
   
   # add result into the sp_tab
   sp_tab[sp_tab$species == sp, "UK_STI_peak"] <- sp_STI_peak
    
  }
  
}

# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries_incSTI_UK.csv"), row.names = F)


# round STI values to 2 dp
sp_tab$UK_STI_yr <- round(sp_tab$UK_STI_yr, digits = 2)
sp_tab$UK_STI_peak <- round(sp_tab$UK_STI_peak, digits = 2)

# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries_incSTI_UK.csv"), row.names = F)



##%######################################################%##
#                                                          #
####                2. Europe-based STI                 ####
#                                                          #
##%######################################################%##

# Here we calculated both the mean- and peak-based STIs for each species 
# using the records from across Europe. 

#sp_tab <- read.csv(paste0(outdir, "Species_record_Summaries_incSTI.csv"))

# load in the GBIF data for Europe
load(file = paste0(datadir, "Ladybirds_Europe_GBIF_processed.rdata"))

# load in the climate data
# downloaded from https://www.worldclim.org/data/worldclim21.html#google_vignette
# 10 minute resolution
temp_files <- list.files(paste0(datadir, "/WorldClim_version_2_1/"), full.names = T)

# read in a stack of all of the temp files (one for each month of the year)
temp_all <- rast(temp_files)
# plot(temp_all) # take a look

# get the average temperature across all months
ann_mean_temp <- mean(temp_all)
# plot(ann_mean_temp) # take a look


#### First, STI determined using all months ####

# for each species, extract the values for point locations,
# take the average across those points and save as the GB STI

# vector of species names
species <- sp_tab$species

# create space to save results in sp_tab
sp_tab$Europe_STI_yr <- NA

# sp <- species[1]
for (sp in species){
  
  sp_dat <- d_EU[d_EU$species == sp, ]
  
  # create point locations 
  sp_xy <- vect(sp_dat, geom = c("decimalLongitude", "decimalLatitude"))
  
  # extract values from the annual mean temperature map
  # take the mean to get the species level STI
  sp_STI <- mean(extract(ann_mean_temp, sp_xy, ID = F)[, 1], na.rm = T)
  
  # add result into the sp_tab
  sp_tab[sp_tab$species == sp, "Europe_STI_yr"] <- sp_STI
  
}

# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries_incSTI_UK.csv"), row.names = F)


#### Second, STI determined using just peak month ####


# create space to save results in sp_tab
sp_tab$Europe_STI_peak <- NA


# sp <- species[31]
for(sp in species){
  
  sp_dat <- d_EU[d_EU$species == sp, ]
  
  # create point locations 
  sp_xy <- vect(sp_dat, geom = c("decimalLongitude", "decimalLatitude"))
  
  # use max month month for adults determined from the records
  peak <- sp_tab$Max_month[sp_tab$species == sp]

  # one species doesn't have enough records for peaks
  if (is.na(peak) == T){  sp_tab[sp_tab$species == sp, "Europe_STI_peak"] <- NA 
  
  } else {
    
    # load in the raster for the peak month
    temp_peak <- rast(temp_files[peak])
    
    # extract values from the temp map for the peak month
    # take the mean to get the species level STI
    sp_STI_peak <- mean(extract(temp_peak, sp_xy, ID = F)[, 1], na.rm = T)
    
    # add result into the sp_tab
    sp_tab[sp_tab$species == sp, "Europe_STI_peak"] <- sp_STI_peak
    
  }
  
  
}

# round STI values to 2 dp
sp_tab$Europe_STI_yr <- round(sp_tab$Europe_STI_yr, digits = 2)
sp_tab$Europe_STI_peak <- round(sp_tab$Europe_STI_peak, digits = 2)

# save the table
write.csv(sp_tab, paste0(outdir, "Species_record_Summaries_incSTI_UK.csv"), row.names = F)


