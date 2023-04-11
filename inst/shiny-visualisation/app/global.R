library(dplyr)
library(sf)
library(lubridate)
library(stars)

# load the data table and format it for the dashboard
data('dungfauna_aus')
table <- dungfauna_aus
# get variables we want
table <- table %>% select(
  id = eventID,
  state=stateProvince,
  site=parentLocationID,
  trap=locationID,
  latitude=decimalLatitude,
  longitude=decimalLongitude,
  year=year,
  month=month,
  day=day,
  species=scientificName,
  abundance=individualCount,
  biomass=individualBiomass,
  datasetName=datasetName
)
table$date <- ymd(paste(table$year, table$month, table$day, sep='-'))
table <- table %>% subset(select=-c(year, month, day))

# remove duplicated data
table <- table[!duplicated(table[!names(table) %in% c('abundance', 'biomass')]),]

# Jake B FIX THE DATA!
# remove nas
table <- table %>% tidyr::drop_na(site, trap, abundance)
table$biomass[is.na(table$biomass)] <- 0

alldatas <- table %>%
  tidyr::pivot_wider(names_from=species, values_from = c(abundance, biomass), values_fill=0)


include_predictions <- FALSE
include_data_table <- TRUE


alldatas$abundance_total <- rowSums(alldatas[ , grepl( "abundance" , names( alldatas ) ) ])
alldatas$biomass_total <- rowSums(alldatas[ , grepl( "biomass" , names( alldatas ) ) ])
alldatas$date <- as.Date(alldatas$date, format="%d/%m/%Y")
alldatas$latitude <- jitter(alldatas$latitude)
alldatas$longitude <- jitter(alldatas$longitude)
alldatas <- alldatas %>% mutate(
  datacode=row_number()
)

row.names(alldatas) <- alldatas$datacode

foo <- alldatas[ , grepl( "abundance" , names( alldatas ))]
original_names <- colnames(foo)
foo <- subset(foo, select=-abundance_total)
species <- gsub("abundance_", "", colnames(foo))
names(species) <- species
species_choices <- species
species_choices <- c('All'='total', species)

cleantable <- alldatas %>%
  select(
    Site = site,
    State = state,
    datacode = datacode,
    Abundance = abundance_total,
    Biomass = biomass_total,
    Lat = latitude,
    Long = longitude,
    datasetName = datasetName
  )

dataset_sources <- c(
  'All',
  unique(cleantable$datasetName)
)

# get predicted_data_sf
# load("./data/predicted_data_sf.RData")
# predictions <- st_as_sf(predicted_data_sf)
# predictions_range <- range(predictions$fit)

# create fake data for testing
# file_locs <- rep(
#   c('./data/solarrad.20210421.grid', './data/solarrad.20210422.grid', './data/solarrad.20210423.grid'),
#   4
# )
# # stars way
# r_stack <- stars::read_stars(file_locs)
# names(r_stack) <- 1:12
# saveRDS(r_stack, './data/example_predictions.rds')
# r_stack <- readRDS('./data/example_predictions.rds')

# r_stack <- stars::read_stars('./data/predictions_for_dashboard.nc')
# names(r_stack) <- 1:12

if (!file.exists('./data/month_name_custom.rds')) {
  stop('create ./data/month_name_custom.rds by running server.R!')
} else {
  month.name.custom <- readRDS('./data/month_name_custom.rds')
}
