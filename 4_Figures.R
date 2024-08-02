##%######################################################%##
#                                                          #
####             Figures for the manuscript             ####
#                                                          #
##%######################################################%##

# Here I generate figures summarising some of the key ladybird traits.

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)

# set directories
datadir <- "1_Species_record_summaries/"
outdir <- "4_Figures/"
if(!dir.exists(outdir)) dir.create(outdir)
