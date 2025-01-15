############################################################
#                                                          #
#           Download GBIF data for species list            #
#                                                          #
############################################################

# This script downloads GBIF data for the list of species. 
# Data are used from across Europe.

# clear environment
rm(list = ls())

# load libraries
library(rgbif)
library(dplyr)
library(readr) 
library(ggplot2)
library(CoordinateCleaner)

# set directories
datadir <- "0_Data/"
plotdir <- "2_Organise_GBIF_Data/GBIF_plots/"
if(!dir.exists(plotdir)) dir.create(plotdir)

# GBIF details #### REMOVE THESE BEFORE SHARING ####
user <- "charlieouthwaite" # your gbif.org username 
pwd <- "Th3LostC!ty" # your gbif.org password
email <- "charlotte.outhwaite.14@ucl.ac.uk" # your email 

# read in species names list
sp_tab <- read.csv(paste0("1_Species_record_summaries/Species_record_Summaries.csv"))

# match the names with GBIF to get the taxon keys
gbif_taxon_keys <- 
  sp_tab$species %>%
  name_backbone_checklist()  %>% # match to backbone
  filter(!matchType == "NONE") %>% # get matched names
  pull(usageKey) # get the gbif taxonkeys


# download the records
occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  pred_in("basisOfRecord", c('HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred("occurrenceStatus","PRESENT"), 
  pred_gte("year", 1970),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)

#Check status with
occ_download_wait('0045574-240626123714530') # value from print to console.

# import the data
d <- occ_download_get('0045574-240626123714530') %>%
  occ_download_import()
# 1666130 records

# get the citation for the dataset
gbif_citation('0045574-240626123714530')
# GBIF Occurrence Download https://doi.org/10.15468/dl.bntf47 Accessed from R 
# via rgbif (https://github.com/ropensci/rgbif) on 2024-05-29

# filter the data, following advice from https://data-blog.gbif.org/post/gbif-filtering-guide/

d_sub <- d %>%
  #setNames(tolower(names(.))) %>% # set lowercase column names to work with CoordinateCleaner
  filter(coordinatePrecision < 0.01 | is.na(coordinatePrecision)) %>% 
  filter(coordinateUncertaintyInMeters < 10000 | is.na(coordinateUncertaintyInMeters)) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301,3036,999,9999)) %>% 
  filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>%
  filter(!is.na(month)) %>% # remove those where month of the record is unknown
  cc_cen(buffer = 2000, lon = "decimalLongitude", lat = "decimalLatitude") %>% # remove country centroids within 2km
  cc_cap(buffer = 2000, lon = "decimalLongitude", lat = "decimalLatitude") %>% # remove capitals centroids within 2km
  cc_inst(buffer = 2000, lon = "decimalLongitude", lat = "decimalLatitude") %>% # remove zoo and herbaria within 2km 
  cc_sea(lon = "decimalLongitude", lat = "decimalLatitude") %>% # remove from ocean 
  distinct(decimalLongitude,decimalLatitude,speciesKey,datasetKey, .keep_all = TRUE) # remove location duplicates

# Removed 52778 records.
# 791729 rows remaining

# check number of species
table(d_sub$species)
length(unique(d_sub$species)) # 48 species

# correcting some name inconsistencies
d_sub[d_sub$species == "Calvia quatuordecimguttata", "species"] <- "Calvia quattuordecimguttata" # missing t
d_sub[d_sub$species == "Propylaea quatuordecimpunctata", "species"] <- "Propylea quattuordecimpunctata" # spelling
d_sub[d_sub$species == "Brumus quadripustulatus", "species"] <- "Exochomus quadripustulatus" # synonym
d_sub[d_sub$species == "Parexochomus nigromaculatus", "species"] <- "Exochomus nigromaculatus" # synonym
d_sub[d_sub$species == "Subcoccinella vigintiquatuorpunctata", "species"] <- "Subcoccinella vigintiquattuorpunctata" # missing t


# take a look at the distribution of data for each species

# load world map
wrld <- map_data("world")

# sp <-  unique(d_sub$species)[31]
for(sp in unique(d_sub$species)){
  
  # select data for the species
  sp_sub <- d_sub[d_sub$species == sp, ]
  
  # plot on a map of the world
  ggplot() + 
    geom_map( data = wrld, map = wrld, aes(long, lat, map_id = region), 
              fill = "grey") +
    geom_point(data = sp_sub, aes(x = decimalLongitude, y = decimalLatitude)) +
    theme_bw() + 
    ggtitle(paste0(sp, ": distribution of GBIF records")) + 
    theme(axis.title = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text = element_blank())
  
  ggsave(filename = paste0(plotdir, sp, ".png"))
  
  
}


# Subset to records in Europe only

# Europe country codes
EU <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT",
        "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", 
        "SK", "FI", "SE", "NO", "IS", "LI", "CH", "UK", "BA", "ME", "MD", "MK", 
        "AL", "RS", "TR", "UA", "AM", "BY", "AZ", "GB", "AD", "MC", "RU", "SM")

# subset records, keep those for Europe only (not counting native range for the non-natives)
d_EU <- d_sub[d_sub$countryCode %in% EU, ]
# 631059 records


# plot points to check from Europe only, map for each species

# sp <- unique(d_sub$species)[52]
for(sp in unique(d_sub$species)){
  
  # select data for the species
  sp_sub <- d_EU[d_EU$species == sp, ]
  
  # plot on a map of the world
  ggplot() + 
    geom_map( data = wrld, map = wrld, aes(long, lat, map_id = region), 
              fill = "grey") +
    geom_point(data = sp_sub, aes(x = decimalLongitude, y = decimalLatitude)) +
    theme_bw() + 
    ggtitle(paste0(sp, ": European distribution of GBIF records")) + 
    theme(axis.title = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text = element_blank())
  
  ggsave(filename = paste0(plotdir, sp, "_Europe.png"))
  
  
}

# save dataset for European records from GBIF
save(d_EU, file = paste0(datadir, "Ladybirds_Europe_GBIF_processed.rdata"))
#load(file = paste0(datadir, "Ladybirds_Europe_GBIF_processed.rdata"))




## add peak of European records to the species table

# load in the table
sp_tab <- read.csv("1_Species_record_summaries/UK/Species_record_Summaries.csv")

# create empty column
sp_tab$Max_month_EU <- NA

# loop through each species to get the month with the highest number of records
# to compare to that calculated from the UK data
for(i in 1:nrow(sp_tab)){
  # i <- 1
  
  # subset to each species 
  sp_recs <- d_EU[d_EU$species == sp_tab[i, 1], ]
  
  # determine peaks
  
  # create empty dataframe to ensure all months have data even if no records
  months <- data.frame(month  = 1:12)
  
  # frequency table of months
  months_freq <- sp_recs %>% 
    group_by(month) %>% tally()
  
  # ensure all months have data by merging above with empty data frame
  months_freq <- merge(months, months_freq, by = "month", all.x = T)
  
  # replace NAs with 0s
  months_freq[is.na(months_freq$n), "n"] <- 0
  
  # If there are fewer than 20 records for the species. Set the month with the 
  # max records and the peak months to NA.
  if(nrow(sp_recs) < 20){
    
    sp_tab[sp_tab$species == sp_tab[i, 1], "Max_month_EU"] <- NA

    
  }else{
    
    # month with the most records
    sp_tab[sp_tab$species == sp_tab[i, 1], "Max_month_EU"] <- months_freq[months_freq$n == max(months_freq$n, na.rm = T), "month"]
    
  }}

# save the table
write.csv(sp_tab, "1_Species_record_summaries/UK/Species_record_Summaries.csv", row.names = F)


