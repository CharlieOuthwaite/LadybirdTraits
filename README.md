# LadybirdTraits

The scripts in this repository are associated with the production of a traits database for UK ladybirds. We bring together information from the literature, the recent field guide by Roy & Brown and occurrence records to collate information on traits, ecological preferences and record-based metrics for the 48 species of ladybird that are currently considered resident in the UK.

The dataset is available via the EIDC [here](https://doi.org/10.5285/18cdeee4-38cf-4d15-a141-a99a53e17095).

Please use the citation below if you use the data. 

Outhwaite, C.L.; Cocker, L.; Comont, R.; White, H.J. ; Powney, G.D. ; Turvey, K. ; Roy, H.E.; Brown, P.M.J. (2025). Species traits, ecological preferences and distribution metrics for 48 species of ladybird considered resident in the United Kingdom. NERC EDS Environmental Information Data Centre. https://doi.org/10.5285/18cdeee4-38cf-4d15-a141-a99a53e17095


The associated data paper is available [here] (https://doi.org/10.1038/s41597-025-05985-8).

Outhwaite, C.L., Cocker, L., Comont, R.F. et al. A database of traits for the ladybird species of the United Kingdom. Sci Data 12, 1708 (2025). https://doi.org/10.1038/s41597-025-05985-8

The following scripts are included here:

*1_Data_Processing.R* - this script takes occurrence records from the UK Ladybird Survey (supplied by the Biological Records Centre) and iRecord (downloaded from the NBN Atlas) and carried out the processing required to combine these records into one datasets. This included extracting country information, generating latitude and longitudes from grid references and harmonising column names. 

*2_Summarise_records.R* - this script takes the processed dataset generated in the previous script and carried out a number of summaries of the dataset that make up the record-based metrics within the trait database. For example, presence and number of records in each country, the month with the maximum number of records and the number of vice counties with records for each species. This script makes up the bulk of the record-based variables in the dataset.

*3_Organise_GBIF_data.R* - this script downloads and processes records for the 48 ladybirds from across Europe from GBIF. These records are later used to determine the European level species temperature indicies. Note, you will need to add your own GBIF log-in information for this script to work. 

*4_Calculate_species_temp_index*.R - this script takes the records in the UK and those from Europe and uses climate data to determine the mean and peak species temperature indices. This indicates the temperatures that species are likely to experience across their range. These values are then added to the table which adds to the trait database. 

*5_Figures.R* - this script takes the trait database and generates the figures presented in the data paper. 
