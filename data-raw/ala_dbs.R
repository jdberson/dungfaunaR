# Can use ALA data for presence data - will require some cleaning up

# Penny's data from 2007 report much more complete in comparison to ALA




# RUN AT THE SAME TIME AS prediction_production.R

# TO CLEAN UP


library(galah)
library(tidyverse)
library(leaflet)


show_all(atlases)
galah_config(email = "jacob.berson@uwa.edu.au", atlas = "Australia") # default

species <-
  c("Bubas bison",
    "Copris elphenor",
    "Copris hispanus",
    "Euoniticellus africanus",
    "Euoniticellus fulvus",
    "Euoniticellus intermedius",
    "Euoniticellus pallipes",
    "Digitonthophagus gazella",
    "Geotrupes spiniger",
    "Liatongus militaris",
    "Onitis alexis",
    "Onitis aygulus",
    "Onitis caffer",
    "Onitis pecuarius",
    "Onitis vanderkelleni",
    "Onitis viridulus",
    "Onthophagus binodis",
    "Onthophagus nigriventris",
    "Onthophagus sagittarius",
    "Onthophagus taurus",
    "Sisyphus rubrus",
    "Sisyphus spinipes")

length(species)

species_info <-
  search_taxa(species)

# UPDATE THIS TO INCUDE COLUMNS IN COMMON WITH dungfaunaR

dbs_ala <-

  # For some reason including "Sisyphus spinipes" with other species results in
  # an error, hence the bind_rows
  bind_rows(

    galah_call() |>
      galah_identify(species[1:21]) |>
      #galah_group_by(species) |>
      galah_apply_profile(CSDM) |>
      galah_select(individualCount, group = c("basic", "event")) |>
      atlas_occurrences(),

    galah_call() |>
      galah_identify(species[22]) |>
      #galah_group_by(species) |>
      galah_apply_profile(CSDM) |>
      galah_select(individualCount, group = c("basic", "event")) |>
      atlas_occurrences()
  )

# Explore data
dbs_ala |>
  distinct(dataResourceName)

abundance_data_providers <-
  dbs_ala |>
  filter(individualCount > 1) |> # Some presence only show counts of 1
  distinct(dataResourceName) |>
  pull()

dbs_ala |>
  filter(individualCount > 1) |>
  distinct(dataResourceName)
dbs_ala |>
  filter(dataResourceName %in% c(abundance_data_providers)) |>
  group_by(dataResourceName) |>
  count()

# Remove data from outside of Australia
dbs_ala_filtered <-
  dbs_ala |>
  filter(dataResourceName != "Scarab Beetles of Hawai'i") |>
  filter(is.na(decimalLatitude) | decimalLatitude < 0) |>
  filter(is.na(decimalLongitude) | decimalLongitude > 0)

# Visualise data ----------------------------------------------------------

map_df <-
  dbs_ala_filtered |>
  mutate(occurrenceStatus_01 = if_else(occurrenceStatus == "PRESENT", 1, 0)) |>
  group_by(dataResourceName, decimalLongitude, decimalLatitude, scientificName) |>
  summarise(total = sum(occurrenceStatus_01)) |>
  mutate(occurrenceStatus = if_else(total == 0, "absent", "present")) |>
  ungroup() %>%
  split(f = .$scientificName)


l_map <-
  leaflet() |>
  addProviderTiles("Esri.WorldImagery", group = "Satellite")

factpal <- colorFactor(palette = "viridis",
                       unique(map_df$occurrenceStatus))

names(map_df) |>
  purrr::walk( function(df) {
    l_map <<- l_map |>
      addCircleMarkers(data = map_df[[df]],
                       lng = ~decimalLongitude,
                       lat = ~decimalLatitude,
                       label = ~str_c(dataResourceName, occurrenceStatus, sep = " "),
                       radius = ~log(total),
                       group = df,
                       color = ~factpal(occurrenceStatus),
                       opacity = 1)
  })

l_map |>
  addLayersControl(
    baseGroups = names(map_df),
    #baseGroups = c("Satellite","Boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  )




# North East Dung Beetle Project ------------------------------------------

# North East Dung Beetle Project is the only substantial contributor with
# abundance data

dbs_ne <-
  dbs_ala |>
  filter(dataResourceName == "North East Dung Beetle Project")
# 11,724 records

dbs_ne |>
  drop_na(decimalLatitude)
# 4,470 records

dbs_ne |>
  filter(eventID != "")
# 4,470 records

# Records mising lon/lat also missing event id
dbs_ne <-
  dbs_ne |>
  drop_na(decimalLatitude)

dbs_ne_wide <-
  dbs_ne |>
  select(-c(taxonConceptID:occurrenceStatus, eventTime, samplingEffort, samplingProtocol)) |>
  pivot_wider(
    id_cols = c(eventID, decimalLatitude, decimalLongitude, eventDate),
    names_from = scientificName,
    values_from = individualCount)
# 452 records

dbs_ne_wide |>
  drop_na()
# 443 complete records

# Visualise seasonality - seems reasonable
dbs_ne_summary <-
dbs_ne |>
  mutate(date = lubridate::as_date(eventDate),
         month = lubridate::month(date),
         year = lubridate::year(date)) |>
  group_by(scientificName, month, year) |>
  summarise(count = mean(individualCount, na.rm = TRUE)) |>
  mutate(date = dmy(str_c(14, month, year, sep = "/")), .after = year)

library(plotly)

plot_ly(dbs_ne_summary |> ungroup(),
        x = ~date, y = ~count, hoverinfo='text',
        text= ~str_c(scientificName, date, sep = " ")) |>
  add_lines(color=~scientificName)


# usethis::use_data(dbs_ala, overwrite = TRUE)
