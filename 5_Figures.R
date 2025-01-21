##%######################################################%##
#                                                          #
####             Figures for the manuscript             ####
#                                                          #
##%######################################################%##

# Here I generate figures summarising some of the key ladybird data and traits.

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(cowplot)

# set directories
datadir <- "0_Data/"
outdir <- "4_Figures/"
if(!dir.exists(outdir)) dir.create(outdir)

# load in the edited trait database
db <- read.csv(paste0(datadir, "Ladybird_Traits_DB_draft_UK_V2_CURRENT.csv"))

# load in the ladybird data - UK
d_UK <- read.csv("1_Species_record_summaries/UK/Ladybird_occurrences_processed_UK_Allvars.csv")

# load in the ladybird data - Europe
load(paste0(datadir, "Ladybirds_Europe_GBIF_processed.rdata")) # d_EU


##%######################################################%##
#                                                          #
####                  Maps of records                   ####
#                                                          #
##%######################################################%##

# get world map
world <- ne_countries(scale = "medium", returnclass = "sf")

### Europe ###
#Europe <- world[which(world$continent == "Europe") & which(world$name == "Turkey"),]

p1 <- ggplot(world) +
  geom_hex(data = d_EU, aes(x = decimalLongitude, y = decimalLatitude), bins = 100) + 
  scale_fill_continuous(high = "black", low = "lightgreen") + 
  geom_sf(fill = NA, col = "#575757") +
  coord_sf(xlim = c(-30,170), ylim = c(30,80), expand = FALSE) + 
  theme_bw() + 
  theme(axis.title = element_blank(), 
        legend.title = element_blank(), 
        text = element_text(size = 8))

# save
ggsave(paste0(outdir, "Europe_records_map.png"), dpi = 250)


# basic map for schematic
ggplot(world) +
  geom_hex(data = d_EU, aes(x = decimalLongitude, y = decimalLatitude), bins = 100) + 
  scale_fill_continuous(high = "black", low = "lightgreen") + 
  geom_sf(fill = NA, col = "black") +
  coord_sf(xlim = c(-30,170), ylim = c(30,80), expand = FALSE) + 
  theme_minimal() + 
  theme(axis.title = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "none", 
        panel.grid = element_blank(), 
        axis.text = element_blank())

# save
ggsave(paste0(outdir, "Europe_records_map_basic.png"), dpi = 250)



### UK ###
  
# create the UK map
UK <- world[which(world$geounit == "United Kingdom"),]

p2 <- ggplot(UK) +
  geom_hex(data = d_UK, aes(x = decimalLongitude.processed, y = decimalLatitude.processed)) + 
  scale_fill_continuous(high = "black", low = "lightblue3") + 
  geom_sf(fill = NA, col = c("#575757")) +
  coord_sf(xlim = c(-10,3), ylim = c(48,61), expand = FALSE) + 
  theme_bw() + 
  theme(axis.title = element_blank(), 
        legend.title = element_blank(), 
        text = element_text(size = 8))

# save
ggsave(paste0(outdir, "UK_records_map.png"), dpi = 250)


# basic map for schematic
ggplot(UK) +
  geom_hex(data = d_UK, aes(x = decimalLongitude.processed, y = decimalLatitude.processed)) + 
  scale_fill_continuous(high = "black", low = "lightblue3") + 
  geom_sf(fill = NA, col = "black") +
  coord_sf(xlim = c(-10,3), ylim = c(48,61), expand = FALSE) + 
  theme_minimal() + 
  theme(axis.title = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "none", 
        panel.grid = element_blank(), 
        axis.text = element_blank())

# save
ggsave(paste0(outdir, "UK_records_map_basic.png"), dpi = 250)


# combine figures for paper
plot_grid(p2, p1, labels = c("a", "b"), nrow = 2, rel_heights = c(1, 0.8))

ggsave(paste0(outdir, "FIGURE_combined_maps.png"), dpi = 250, width = 6, height = 4)



##%######################################################%##
#                                                          #
####    Bar chart of long-term and short-term trends    ####
#                                                          #
##%######################################################%##

# subset out the trend data
trends <- db[, c("Shortterm_trend", "ST_trend_lower", "ST_trend_upper", "Longterm_trend", "LT_trend_lower", "LT_trend_upper")]

# determine categories
trends$ST_status <- NA
trends$LT_status <- NA

# declining ST
trends[trends$ST_trend_lower < 0 & trends$ST_trend_upper < 0 & !is.na(trends$Shortterm_trend), "ST_status"] <- "Declining"

# increasing ST
trends[trends$ST_trend_lower > 0 & trends$ST_trend_upper > 0 & !is.na(trends$Shortterm_trend), "ST_status"] <- "Increasing"

# stable ST
trends[is.na(trends$ST_status) & !is.na(trends$Shortterm_trend), "ST_status"] <- "Stable"

# how many species have trends?
length(trends$Shortterm_trend[!is.na(trends$Shortterm_trend)]) # short-term trends for 22 species


# declining LT
trends[trends$LT_trend_lower < 0 & trends$LT_trend_upper < 0 & !is.na(trends$Longterm_trend), "LT_status"] <- "Declining"

# increasing LT
trends[trends$LT_trend_lower > 0 & trends$LT_trend_upper > 0 & !is.na(trends$Longterm_trend), "LT_status"] <- "Increasing"

# stable LT
trends[is.na(trends$LT_status) & !is.na(trends$Longterm_trend), "LT_status"] <- "Stable"

# how many species have trends?
length(trends$Longterm_trend[!is.na(trends$Longterm_trend)]) # long-term trends for 29 species


# create stacked bar plot

# organise the data
term <- c(rep("Long-term", 3), rep("Short-term", 3))
trend <-rep(c("Increasing", "Stable", "Declining"), 2)
n <- c(5, 17, 7, 5, 14, 3)

table(trends$LT_status)
table(trends$ST_status)

plot_dat <- data.frame(term, trend, n)
plot_dat$trend <- factor(plot_dat$trend, levels = c("Increasing", "Stable", "Declining"))

# stacked bar plot
ggplot(plot_dat, aes(fill=trend, y=n, x=term)) + 
  geom_bar(position="stack", stat="identity", width = 0.5) + 
  scale_fill_manual(values = c(c("#008B00", "#838B8B", "#8B2323"))) + 
  theme_bw() + 
  theme(legend.title = element_blank(), 
        axis.title.x = element_blank(), 
        text = element_text(size = 8)) +
  ylab("Number of species")

# save
ggsave(paste0(outdir, "FIGURE_trends_barplot.png"), width = 4, height = 2)


