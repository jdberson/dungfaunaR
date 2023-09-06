# library(dplyr)
# library(sf)
# library(lubridate)
# library(stars)

# load the data table and format it for the dashboard
table <-
  dungfauna_occurrence |>

  dplyr::mutate(
    date = lubridate::as_date(eventDate_setup),
    trap = dplyr::if_else(base::is.na(locationID_trap),
                          locationID_site,
                          locationID_trap)
    ) |>

  dplyr::select(
  id = eventID,
  state = stateProvince,
  site = locationID_site,
  trap,
  latitude = decimalLatitude,
  longitude = decimalLongitude,
  date,
  species = scientificName,
  abundance = individualCount,
  #biomass=individualBiomass,
  datasetName,
  occurrenceStatus
) |>

  # Temporary hack to include a biomass variable - JB to fix
  dplyr::mutate(biomass = stats::rnorm(base::length(datasetName), 100)) |>

  # There are data that come from surveys / museum records etc. These will have
  # an occurrenceStatus of "present" but a count of NA. Need to decide how to
  # incorporate these data. For now, assign these values 1.
  dplyr::mutate(abundance = dplyr::case_when(
    base::is.na(abundance) & occurrenceStatus == "present" ~ 1,
    base::is.na(abundance) & occurrenceStatus == "absent" ~ 0,
    TRUE ~ abundance
  ))

dungfauna_occurrence <- dungfauna_occurrence |>
  dplyr::mutate(
    datacode = base::paste0(locationID_site, '_', stateProvince)
  )

alldatas <-
  table |>
  tidyr::pivot_wider(
    names_from=species,
    values_from = c(abundance, biomass),
    values_fill=0) |>
  dplyr::mutate(
    abundance_total = base::rowSums(dplyr::across(dplyr::contains("abundance"))),
    biomass_total = base::rowSums(dplyr::across(dplyr::contains("biomass"))),
    datacode = dplyr::row_number()
  )
#row.names(alldatas) <- alldatas$datacode

include_predictions <- FALSE
include_data_table <- TRUE



#alldatas$date <- as.Date(alldatas$date, format="%d/%m/%Y")




foo <- alldatas[ , base::grepl( "abundance" , base::names( alldatas ))]
original_names <- base::colnames(foo)
foo <- base::subset(foo, select = -abundance_total)
species <- base::gsub("abundance_", "", base::colnames(foo))
base::names(species) <- species
species_choices <- species
species_choices <- c('All'='total', species)

cleantable <-
  alldatas |>
  dplyr::select(
    Site = site,
    State = state,
    datacode = datacode,
    date = date,
    Abundance = abundance_total,
    Biomass = biomass_total,
    Lat = latitude,
    Long = longitude,
    datasetName = datasetName
  )

dataset_sources <- c(
  'All',
  base::unique(cleantable$datasetName)
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

if (!base::file.exists('./data/month_name_custom.rds')) {
  stop('create ./data/month_name_custom.rds by running server.R!')
} else {
  month.name.custom <- base::readRDS('./data/month_name_custom.rds')
}

# Clear the objects created while running the shiny app
# Note that we are not clearing the dungfauna_occurrence object as this
# may have been loaded prior to loading the app - not sure if this is the
# best practice
shiny::onStop(function() {
  base::rm(list = c("alldatas", "cleantable", "foo", "table", "dataset_sources",
              "include_data_table", "include_predictions", "month.name.custom",
              "original_names", "species", "species_choices"),
     envir = .GlobalEnv)
  detach("package:shiny", unload = FALSE) # Gives an error if TRUE
})
