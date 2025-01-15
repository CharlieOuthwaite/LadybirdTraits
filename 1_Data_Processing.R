##%######################################################%##
#                                                          #
####                 UK data processing                 ####
#                                                          #
##%######################################################%##

# This script carries out the processing required on the raw occurence record datasets. 
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