## code to prepare `dafwa_2012_2014` dataset goes here

library(tidyverse)
library(dwcPrepare)
library(leaflet)
library(plotly)
library(sf)

# Note: the DAFWA 2012-2014 project comprised both a trapping and a survey
# component. Here we prepare both separately before combining into one dataset


# Trapping data -----------------------------------------------------------


# Import raw data ---------------------------------------------------------

## Load and initial formatting of raw data
dafwa_wa_2012_2014_trap_raw <-
  read_csv("data-raw/data-files/dafwa_wa_2012_2014_trap.csv") |>

  # Assign unique trap identifier
  mutate(trap = str_c(site, trap, sep = " "))

# Darwin Core Event Details -----------------------------------------------

dafwa_wa_2012_2014_trap_event <-
  dafwa_wa_2012_2014_trap_raw |>

  # Generate event fields with dwc_Event()
  mutate(
    dwc_Event(
      start = date_setup,
      end = date_collect,
      tzone = "Australia/Perth",
      habitat = "pasture",
      samplingProtocol = "CSIRO pitfall trap baited with 1L (+/- 0.1L) cattle dung wrapped in gauze (ethanol preservative)",
      samplingEffort = "1 trap"
    )
  ) |>

  # Change site to locationID_site and trap to locationID_trap to reflect nested
  # spatial locations
  mutate(locationID_site = site,
         locationID_trap = trap) |>

  # Assign each sample a unique id (eventID) and each site visit a parentEventID
  # At the same time give a name to the dataset
  group_by(locationID_site, date_setup) |>
  mutate(datasetName = "South-Western Australian Dung Beetle Survey and Monitoring Project",
         parentEventID =  str_c(locationID_site, as.Date(date_setup),
                                sep = "_"),
         eventID = str_c(parentEventID, letters[1:n()], sep = "_")) |>
  ungroup()


# Darwin Core Location Details --------------------------------------------

# Two traps placed close to each other at each site. Check if both are available:

# Check unique lon/lat for each site
dafwa_wa_2012_2014_trap_event |>
  group_by(locationID_site) |>
  distinct(latitude, longitude) |>
  arrange(locationID_site) |>
  print(n = 23)

dafwa_wa_2012_2014_trap_event |>
  group_by(latitude, longitude) |>
  distinct(locationID_site) |>
  arrange(locationID_site) |>
  print(n = 23)


# Yes, except for Narrikup

# Check the distance between the two traps

dafwa_wa_2012_2014_trap_sf <-
dafwa_wa_2012_2014_trap_event |>
  group_by(locationID_site, longitude, latitude) |>
  slice(1) |>
  group_by(locationID_site) |>
  mutate(n = n()) |>
  filter(n > 1) |>
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE)

wa_nearest_trap <-
  dafwa_wa_2012_2014_trap_sf |>
  st_nearest_feature()

wa_trap_distances <-
  bind_cols(
    dafwa_wa_2012_2014_trap_sf |>
      select(locationID_site, locationID_trap) |>
      st_drop_geometry(),

    dafwa_wa_2012_2014_trap_sf[wa_nearest_trap, "locationID_trap"] |>
      st_drop_geometry(),

    distance = st_distance(dafwa_wa_2012_2014_trap_sf,
                           dafwa_wa_2012_2014_trap_sf[wa_nearest_trap, ],
                           by_element = TRUE)
  )

# Summarise distances
wa_trap_distances |>
  ungroup() |>
  summarise(dist_min = min(distance),
            dist_max = max(distance),
            dist_mean = mean(distance))


# Keep point location but note that uncertainty of ~30m will mean that both
# traps captured in the same 'location'.


data("county_stateProvince_aus")
data("locality_data_aus")

dafwa_wa_2012_2014_trap_location <-

  # Make a tibble with one row per trap
  dafwa_wa_2012_2014_trap_event |>
  group_by(locationID_site, locationID_trap) |>
  summarise(verbatimLongitude = longitude[1],
            verbatimLatitude = latitude[1]) |>

  # Use dwc_Location() to generation Darwin Core location terms
  mutate(dwc_Location(
    longitude = verbatimLongitude,
    latitude = verbatimLatitude,
    verbatimCoordinateSystem = "decimal degrees",
    verbatimSRS = "EPSG:4326",
    gps_uncertainty = 30,
    localities_sf = locality_data_aus,
    localities_names = "locality_name",
    county_sf = county_stateProvince_aus
  )) |>

  # Relocate columns
  relocate(verbatimLatitude, verbatimLongitude, .after = verbatimSRS)


## Look at trap polygons
dafwa_trap <-
  dafwa_wa_2012_2014_trap_location |>

  # Create the sf object
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE) |>

  # Need to account for uncertainty around points by adding buffer
  # Note the crs transform as coordinateUncertaintyInMeters is in m
  st_transform(crs = st_crs("EPSG:7844")) %>%
  st_buffer(dist = .$coordinateUncertaintyInMeters) |>

  # Summarise to convex hull
  group_by(locationID_site, locationID_trap) |>
  summarise() |>
  st_convex_hull() |>
  st_transform(crs = st_crs("EPSG:4326")) |>
  rename(footprintWKT = geometry) |>
  mutate(footprintSRS  = "EPSG:4326", .after = footprintWKT)

# View trap polygons
leaflet(data = dafwa_trap) |>
  addProviderTiles("Esri.WorldImagery") |>
  addPolygons(label = ~locationID_trap)

# Check if any trap locations intersect
dafwa_trap |>
  dwc_polygon_cluster() |>
  group_by(.cluster) |>
  mutate(n = length(.cluster)) |>
  filter(n > 1) # 0

# Both traps at each site intersect due to large uncertainty and close distance
# of traps. Keep coordinates unique to each trap as traps were permanently
# kept in the same location

## Look at site polygons

dafwa_site <-
  dafwa_trap |>
  group_by(locationID_site, footprintSRS) |>
  summarise() |>
  st_convex_hull()

# View site polygons
leaflet(data = dafwa_site) |>
  addProviderTiles("Esri.WorldImagery") |>
  addPolygons(label = ~locationID_site)

# Combine event and location details --------------------------------------

# Combine event with location details
dafwa_wa_2012_2014_trap_location_event <-
  left_join(dafwa_wa_2012_2014_trap_event,
            dafwa_wa_2012_2014_trap_location,
            by = c("locationID_site", "locationID_trap")) |>


  # Include additional fields
  mutate(

    # Add in additional fields
    basisOfRecord = "PreservedSpecimen",
    recordedBy = "DAFWA",
    identifiedBy = "DAFWA"
    ) |>

  # Include georeferenceProtocol and georeferenceSources column
  mutate(
    georeferenceProtocol =
      "coordinates from GPS device, coordinateUncertaintyInMeters includes an assumed GPS uncertainty of 30 m",
    georeferenceSources = "GPS"

  ) |>

  # Include placeholder for eventRemarks
  mutate(
    eventRemarks = NA_character_,
    date_collect = as_date(date_collect)
  ) |>

  # Select and order columns to keep
  select(

    # Record-level
    datasetName,
    basisOfRecord,

    # Occurrence fields
    recordedBy,

    # Event fields
    eventID,
    parentEventID,
    eventDate_setup = date_setup,
    eventDate_collect = date_collect,
    eventDate,
    startDayOfYear,
    endDayOfYear,
    year,
    month,
    day,
    verbatimEventDate,
    habitat,
    samplingProtocol,
    sampleSizeValue,
    sampleSizeUnit,
    samplingEffort,
    eventRemarks,

    # Location fields
    locationID_site,
    locationID_trap,
    country,
    countryCode,
    stateProvince,
    county,
    locality,
    decimalLatitude,
    decimalLongitude,
    geodeticDatum,
    coordinateUncertaintyInMeters,
    coordinatePrecision,
    verbatimLatitude,
    verbatimLongitude,
    verbatimCoordinateSystem,
    verbatimSRS,
    georeferenceProtocol,
    georeferenceSources,

    # Identification fields
    identifiedBy,

    # Taxon fields
    `Onthophagus taurus`:`Copris hispanus`

  )



# Survey data -------------------------------------------------------------


# Import raw data ---------------------------------------------------------

## Load and initial formatting of raw data
dafwa_wa_2012_2014_survey_raw <-
  read_csv("data-raw/data-files/dafwa_wa_2012_2014_survey.csv")

# Darwin Core Event Details -----------------------------------------------

dafwa_wa_2012_2014_survey_event <-
  dafwa_wa_2012_2014_survey_raw |>

  # Generate event fields with dwc_Event()
  mutate(
    dwc_Event(
      start = date,
      tzone = "Australia/Perth",
      habitat = "pasture",
      samplingProtocol = "Dung pads searched for dung beetles",
      samplingEffort = "Approximately five person hours spent searching",
      eventRemarks = "Species counts are visual activity ratings"
    )
  ) |>

  # Change site to locationID_site to match other data
  mutate(
    locationID_site = site
  )|>

  # Assign a unique id (eventID) to each site visit
  group_by(locationID_site, date) |>
  mutate(datasetName = "South-Western Australian Dung Beetle Survey and Monitoring Project",
         parentEventID =  str_c(locationID_site, as.Date(date), sep = "_"),
         eventID = str_c(parentEventID, letters[1:n()], sep = "_")) |>
  ungroup() #|>


# Darwin Core Location Details --------------------------------------------

# Check unique lon/lat for each site
dafwa_wa_2012_2014_survey_event |>
  group_by(locationID_site) |>
  distinct(latitude, longitude) |>
  arrange(locationID_site) |>
  print(n = 57)


dafwa_wa_2012_2014_survey_location <-

  # Make a tibble with one row per survey
  dafwa_wa_2012_2014_survey_event |>
  group_by(locationID_site) |>
  summarise(verbatimLongitude = longitude[1],
            verbatimLatitude = latitude[1]) |>

  # Use dwc_Location() to generation Darwin Core location terms
  mutate(dwc_Location(
    longitude = verbatimLongitude,
    latitude = verbatimLatitude,
    verbatimCoordinateSystem = "decimal degrees",
    verbatimSRS = "EPSG:4326",
    gps_uncertainty = 500,
    localities_sf = locality_data_aus,
    localities_names = "locality_name",
    county_sf = county_stateProvince_aus
  )) |>

  # Relocate columns
  relocate(verbatimLatitude, verbatimLongitude, .after = verbatimSRS)


## Site information
dafwa_survey_site <-
  dafwa_wa_2012_2014_survey_location |>

  # Create the sf object
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE) |>

  # Need to account for uncertainty around points by adding buffer
  # Note the crs transform as coordinateUncertaintyInMeters is in m
  st_transform(crs = st_crs("EPSG:7844")) %>%
  st_buffer(dist = .$coordinateUncertaintyInMeters) |>

  # Summarise to convex hull
  group_by(locationID_site) |>
  summarise() |>
  st_convex_hull() |>
  st_transform(crs = st_crs("EPSG:4326")) |>
  rename(footprintWKT = geometry) |>
  mutate(footprintSRS  = "EPSG:4326", .after = footprintWKT)

# Check if any site locations intersect
dafwa_survey_site |>
  dwc_polygon_cluster() |>
  group_by(.cluster) |>
  mutate(n = length(.cluster)) |>
  filter(n > 1) # 0

leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  addPolygons(data = dafwa_survey_site,
              label = ~locationID_site, color = "blue")


# Combine event and location details --------------------------------------

dafwa_wa_2012_2014_survey_location_event <-
  left_join(dafwa_wa_2012_2014_survey_event,
            dafwa_wa_2012_2014_survey_location, by = c("locationID_site")) |>

  # Add in additional fields
  mutate(
    basisOfRecord = "HumanObservation",
    recordedBy = "DAFWA",
    identifiedBy = "DAFWA"
  ) |>

  # Include georeferenceProtocol and georeferenceSources columns
  mutate(
    georeferenceProtocol =
      "coordinates from GPS device, coordinateUncertaintyInMeters includes an assumed GPS uncertainty of 500 m to indicate multiple dung pad sampling locations at the site",
    georeferenceSources = "GPS",
  ) |>

   # Select and order columns to keep
  select(

    # Record-level
    datasetName,
    basisOfRecord,

    # Occurrence fields
    recordedBy,

    # Event fields
    eventID,
    parentEventID,
    eventDate_setup = date,
    eventDate_collect = date,
    eventDate,
    startDayOfYear,
    endDayOfYear,
    year,
    month,
    day,
    verbatimEventDate,
    habitat,
    samplingProtocol,
    samplingEffort,
    eventRemarks,

    # Location fields
    locationID_site,
    country,
    countryCode,
    stateProvince,
    county,
    locality,
    decimalLatitude,
    decimalLongitude,
    geodeticDatum,
    coordinateUncertaintyInMeters,
    coordinatePrecision,
    verbatimLatitude,
    verbatimLongitude,
    verbatimCoordinateSystem,
    verbatimSRS,
    georeferenceProtocol,
    georeferenceSources,

    # Identification fields
    identifiedBy,

    # Taxon fields
    `Bubas bison`:`Euoniticellus pallipes`
  )


# Combine data ------------------------------------------------------------


# Check trap and survey locations -----------------------------------------

bind_rows(dafwa_survey_site,
          dafwa_site) |>
  dwc_polygon_cluster() |>
  group_by(.cluster) |>
  mutate(n = length(.cluster)) |>
  filter(n > 1) # 2

# One site - Williams, already given the same locationID_site name across
# datasets so no need to change here


# Combine locationID_site polygons, retain the larger survey location for
# Willams
dafwa_site <-
  bind_rows(
    dafwa_survey_site,

            dafwa_site |>
              filter(locationID_site != "Williams")
    )




# Combine datasets --------------------------------------------------------

dafwa_wa_2012_2014 <-

  bind_rows(

    dafwa_wa_2012_2014_trap_location_event,

    dafwa_wa_2012_2014_survey_location_event |>
      mutate(verbatimEventDate = as.character(verbatimEventDate))
  ) |>

  # Onthophagus vermiculatus not recorded in survey data - change NAs to 0s
  # that is, it would have been recorded if found
  replace_na(list(`Onthophagus vermiculatus` = 0)) |>

  # inlcude dung_type
  mutate(
    dung_type = "cattle",
    .after = identifiedBy
  )


# Check data --------------------------------------------------------------

# Check site locations
leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(data = dafwa_wa_2012_2014 ,
                   lng = ~decimalLongitude, lat = ~decimalLatitude,
                   label = ~locationID_trap,
                   labelOptions = labelOptions(noHide = FALSE),
                   color='red')

# View species locations
map_df <-
  dafwa_wa_2012_2014 |>
  pivot_longer(cols = `Onthophagus taurus`:`Copris hispanus`,
               names_to = "scientificName", values_to = "individualCount") |>
  filter(individualCount != 0) |>
  group_by(locationID_site, scientificName) |>
  summarise(decimalLongitude = decimalLongitude[1],
            decimalLatitude = decimalLatitude[1]) |>
  ungroup() %>%
  split(f = .$scientificName)

l_map <-
  leaflet() |>
  addProviderTiles("Esri.WorldImagery", group = "Satellite")

names(map_df) |>
  purrr::walk( function(df) {
    l_map <<- l_map |>
      addCircleMarkers(data = map_df[[df]],
                       lng = ~decimalLongitude,
                       lat = ~decimalLatitude,
                       label = ~str_c(locationID_site, scientificName, sep = " "),
                       group = df,
                       color = "red",
                       opacity = 1)
  })

l_map |>
  addLayersControl(
    baseGroups = names(map_df),
    options = layersControlOptions(collapsed = FALSE)
  )


## Time series

dafwa_wa_2012_2014_summary <-
  dafwa_wa_2012_2014 |>
  pivot_longer(cols = `Onthophagus taurus`:`Copris hispanus`,
               names_to = "scientificName", values_to = "individualCount") |>
  filter(basisOfRecord == "PreservedSpecimen") |>

  # Some gymnastics to calculate monthly means of each species after
  # removing sites where species not found
  group_by(locationID_site, scientificName, month, year) |>
  mutate(total = sum(individualCount)) |>
  group_by(locationID_site, scientificName) |>
  mutate(max = max(total)) |>
  filter(max != 0) |>
  group_by(scientificName, month, year) |>
  summarise(count = mean(individualCount, na.rm = TRUE)) |>
  mutate(date = dmy(str_c(14, month, year, sep = "/")), .after = year)


plot_ly(dafwa_wa_2012_2014_summary |> ungroup(),
        x = ~date, y = ~count, hoverinfo='text',
        text= ~str_c(scientificName, date, sep = " ")) |>
  add_lines(color=~scientificName)



# Save data ---------------------------------------------------------------

# Save data for combining with that from other projects
write.csv(dafwa_wa_2012_2014,
          "data-raw/dafwa_wa_2012_2014-prepared.csv",
          row.names = FALSE,
          na = "")


# Also save trap and site polygon data for comparing with data from other
# projects
saveRDS(object = dafwa_trap,
        file = "data-raw/dafwa_trap.rds")

saveRDS(object = dafwa_site,
        file = "data-raw/dafwa_site.rds")

# Session info ------------------------------------------------------------

sessionInfo()

# R version 4.3.1 (2023-06-16 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 22621)
#
# Matrix products: default
#
#
# locale:
# [1] LC_COLLATE=English_Australia.utf8  LC_CTYPE=English_Australia.utf8
# [3] LC_MONETARY=English_Australia.utf8 LC_NUMERIC=C
# [5] LC_TIME=English_Australia.utf8
#
# time zone: Australia/Perth
# tzcode source: internal
#
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
# [1] sf_1.0-14             plotly_4.10.2         leaflet_2.2.0
# [4] dwcPrepare_0.0.0.9000 lubridate_1.9.2       forcats_1.0.0
# [7] stringr_1.5.0         dplyr_1.1.2           purrr_1.0.2
# [10] readr_2.1.4           tidyr_1.3.0           tibble_3.2.1
# [13] ggplot2_3.4.3         tidyverse_2.0.0
#
# loaded via a namespace (and not attached):
# [1] utf8_1.2.3         generics_0.1.3     class_7.3-22       KernSmooth_2.23-21
# [5] stringi_1.7.12     hms_1.1.3          digest_0.6.33      magrittr_2.0.3
# [9] grid_4.3.1         timechange_0.2.0   fastmap_1.1.1      jsonlite_1.8.7
# [13] e1071_1.7-13       DBI_1.1.3          httr_1.4.7         fansi_1.0.4
# [17] crosstalk_1.2.0    viridisLite_0.4.2  scales_1.2.1       lazyeval_0.2.2
# [21] cli_3.6.1          rlang_1.1.1        units_0.8-3        munsell_0.5.0
# [25] withr_2.5.0        tools_4.3.1        tzdb_0.4.0         colorspace_2.1-0
# [29] vctrs_0.6.3        R6_2.5.1           proxy_0.4-27       classInt_0.4-9
# [33] lifecycle_1.0.3    fs_1.6.3           htmlwidgets_1.6.2  usethis_2.2.2
# [37] pkgconfig_2.0.3    pillar_1.9.0       gtable_0.3.4       Rcpp_1.0.11
# [41] glue_1.6.2         data.table_1.14.8  tidyselect_1.2.0   rstudioapi_0.15.0
# [45] htmltools_0.5.6    compiler_4.3.1
