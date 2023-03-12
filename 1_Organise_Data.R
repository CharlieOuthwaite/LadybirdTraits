############################################################
#                                                          #
#              Organise Trait and Trend Data               #
#                                                          #
############################################################

# In this script, the ladybird trait data and trend data are read in and organised. 

# Trait data is from XXX Add paper details XXX

# Trend data is from Outhwaite et al 2019 Scientific Data


# clear environment
rm(list = ls())

# load libraries


# Set working directories
datadir <- "0_Data/"
outdir <- "1_Organise_Data/"
if(!dir.exists(outdir)) dir.create(outdir)

# load in species trends
trends <- read.csv(paste0(datadir, "Species_Trends.csv"))

# subset to ladybirds only
trends <- trends[trends$Group == "Ladybirds", ]

# take a look at the trend data
head(trends)

# number of species
nrow(trends) # 37 species

# keep columns of interest
trends <- trends[, c(2, 6:10)]

# load in ladybird trait database
traits <- read.csv(paste0(datadir, "Ladybird_Traits_Database.csv"))






