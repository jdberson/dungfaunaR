## code to prepare `qld_2001-2003_2009-2010` dataset goes here

library(tidyverse)
library(dwcPrepare)
library(leaflet)
library(plotly)
library(sf)


# Trapping data -----------------------------------------------------------


# Import raw data ---------------------------------------------------------

## Load and initial formatting of raw data
qld_2001_2003_trap_raw <-
  read_csv("data-raw/data-files/qld_2001_2003_trap.csv") |>

  # Assign unique trap identifier
  mutate(trap = str_c(site, trap, sep = " ")) |>

  # Change names
  rename(latitude = verbatimLatitude,
         longitude = verbatimLongitude)


# Darwin Core Event Details -----------------------------------------------

qld_2001_2003_trap_event <-
  qld_2001_2003_trap_raw |>

  # Generate event fields with dwc_Event()
  mutate(
    dwc_Event(
      start = date_setup,
      end = date_collect,
      tzone = "Australia/Brisbane",
      habitat = "pasture",
      samplingProtocol = "QLD project pitfall trap baited with 1 L (+/- 0.1 L) cattle dung wrapped in gauze (no preservative)",
      samplingEffort = "1 trap"
    )
  ) |>

  # Change site to locationID_site and trap to locationID_trap to reflect nested
  # spatial locations
  rename(locationID_site = site,
         locationID_trap = trap) |>

  # Assign each sample a unique id (eventID) and each site visit a parentEventID
  group_by(locationID_site, date_setup) |>
  mutate(datasetName = "Queensland Dung Beetle Project",
         parentEventID =  str_c(locationID_site, as.Date(date_setup), sep = "_"),
         eventID = str_c(parentEventID, letters[1:n()], sep = "_")) |>
  ungroup()


# Darwin Core Location Details --------------------------------------------

# For QLD trapping data we know the location of the majority of traps, but for
# some sites, we know the location of one trap only, or the site location but
# not the trap locations. We can use the distance between traps at sites where
# both trap locations are known to assign an uncertainty to unknown trap
# locations. We will supplement this with visualising the locations to modify
# the uncertainty where appropriate.


# Look at distance between traps where both traps are known

qld_2001_2003_trap_sf <-
  qld_2001_2003_trap_event |>
  filter(coordinate_level == "trap") |>
  group_by(locationID_trap) |>
  slice(1) |> group_by(locationID_site) |>
  mutate(n = n()) |>
  filter(n > 1) |>
  mutate(dwc_coordinates(longitude = longitude,
                         latitude = latitude,
                         verbatimCoordinateSystem = verbatimCoordinateSystem,
                         verbatimSRS = "EPSG:4326")) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE)

qld_nearest_trap <-
  qld_2001_2003_trap_sf |>
  st_nearest_feature()

qld_trap_distances <-
  bind_cols(
    qld_2001_2003_trap_sf |>
      select(region, locationID_site, locationID_trap,
             decimalLatitude, decimalLongitude) |>
      st_drop_geometry(),

    qld_2001_2003_trap_sf[qld_nearest_trap, "locationID_trap"] |>
      st_drop_geometry(),

    distance = st_distance(qld_2001_2003_trap_sf,
                           qld_2001_2003_trap_sf[qld_nearest_trap, ],
                           by_element = TRUE)
  )

# Summarise distances
qld_trap_distances_summary <-
qld_trap_distances |>
   group_by(
    region,
      ) |>
  summarise(dist_min = as.integer(round(min(distance),0)),
            dist_max = as.integer(round(max(distance),0)),
            dist_mean = as.integer(round(mean(distance),0)),
            dist_q1 = as.integer(round(quantile(distance, probs = 0.25),0)),
            dist_median = as.integer(round(median(distance),0)),
            dist_q3 = as.integer(round(quantile(distance, probs = 0.75),0)))
qld_trap_distances_summary

qld_trap_distances |>
  ungroup() |>
  mutate(distance = as.integer(distance)) |>
  ggplot(aes(x = region, y = distance)) +
  geom_boxplot()

qld_trap_distances |>
  ungroup() |>
  mutate(distance = as.integer(distance)) |>
  ggplot(aes(x = distance)) +
  geom_histogram() +
  facet_wrap(~region)

# Where the site is known but the trap location within a site is unknown, assign
# the gps uncertainty as the 75th percentile distance between traps for sites in
# the same region, otherwise assign is as 100m. Using the 75th percentile
# removes the influence of some outlier sites with very large distances between
# traps.

# Assign 500m for Northern Territory Site and 2000m for Cooktown 2 trap (taken
# from Google Earth when investigating surrounding land uses)

qld_2001_2003_trap_event <-
left_join(
  qld_2001_2003_trap_event,
  qld_trap_distances_summary,
  by = "region"
) |>
  mutate(gps_uncertainty = case_when(
    region == "NT" ~ 500,
    locationID_trap  == "Cooktown 2" ~ 2000,
    coordinate_level == "site" ~ dist_q3,
    coordinate_level == "trap" ~ 30
  ), .after = coordinate_source) |>
  select(-c(dist_min, dist_max, dist_mean, dist_q1, dist_median, dist_q3))




# Now we can use dwc_Location to get the location information for the QLD traps

data("county_stateProvince_aus")
data("locality_data_aus")


qld_2001_2003_trap_location <-

  # Make a tibble with one row per trap
  qld_2001_2003_trap_event |>
  group_by(locationID_site, locationID_trap) |>
  summarise(verbatimLongitude = longitude[1],
            verbatimLatitude = latitude[1],
            gps_uncertainty = gps_uncertainty[1],
            verbatimCoordinateSystem = verbatimCoordinateSystem[1]) |>

  # Use dwc_Location() to generation Darwin Core location terms
  mutate(dwc_Location(
    longitude = verbatimLongitude,
    latitude = verbatimLatitude,
    verbatimCoordinateSystem = verbatimCoordinateSystem,
    verbatimSRS = "EPSG:4326",
    gps_uncertainty = gps_uncertainty,
    localities_sf = locality_data_aus,
    localities_names = "locality_name",
    county_sf = county_stateProvince_aus
  ))


## Look at trap polygons
qld_trap <-
  qld_2001_2003_trap_location |>

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
factpal <- colorFactor(palette = "viridis",
                       unique(qld_2001_2003_trap_event$region))

qld_trap_locations_uncertainty <-
leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  addPolygons(
    data = qld_trap |>
      left_join(qld_2001_2003_trap_event |>
                  select(locationID_trap,
                         coordinate_level,
                         coordinate_source,
                         region,
                         site_code)) |>
      filter(coordinate_level == "trap"),
    label = ~str_c(site_code, "Trap =", locationID_trap,
                   "Coordinate source =", coordinate_source, sep = " "),
    color = ~factpal(region),
    group = "Trap level certainty",
    fillOpacity  = 0.02
    ) |>
  addPolygons(
    data = qld_trap |>
      left_join(qld_2001_2003_trap_event |>
                  select(locationID_trap,
                         coordinate_level,
                         coordinate_source,
                         region,
                         site_code)) |>
      filter(coordinate_level == "site"),
    label = ~str_c(site_code, "Trap =", locationID_trap,
                   "Coordinate source =", coordinate_source, sep = " "),
    color = ~factpal(region),
    group = "Site level certainty",
    fillOpacity  = 0.02
  ) |>
  addLayersControl(
    overlayGroups = c("Trap level certainty", "Site level certainty"),
    options = layersControlOptions(collapsed = FALSE)
  )

qld_trap_locations_uncertainty

# Check if any trap locations intersect
qld_trap |>
  dwc_polygon_cluster() |>
  group_by(.cluster) |>
  mutate(n = length(.cluster)) |>
  filter(n > 1) #

# Lots of intersections due to uncertainty of second trap at a number of
# locations. In small number of locations traps were also placed close
# also resulting in intersections.

## Look at site polygons

qld_site <-
  qld_trap |>
  group_by(locationID_site, footprintSRS) |>
  summarise() |>
  st_convex_hull()

# Check if site locations intersect
# Check if any trap locations intersect
qld_site |>
  dwc_polygon_cluster() |>
  group_by(.cluster) |>
  mutate(n = length(.cluster)) |>
  filter(n > 1) # 7

qld_2001_2003_trap_location |>
  filter(locationID_site %in% c("Dululu_a", "Dululu_b", "Wowan"))# |> View()

# Some intersecting sites due to large uncertainty for some trap locations.
# Retain separate site identifiers as this may reflect differences in management
# stock etc.

# View site polygons
leaflet(data = qld_site) |>
  addProviderTiles("Esri.WorldImagery") |>
  addPolygons(label = ~locationID_site)


# Combine event and location details --------------------------------------

# Combine event with location details
qld_2001_2003_trap_location_event <-
  left_join(qld_2001_2003_trap_event,
            qld_2001_2003_trap_location,
            by = c("locationID_site", "locationID_trap")) |>


  # Include additional fields
  mutate(

    # Add in additional fields
    basisOfRecord = "PreservedSpecimen",
    recordedBy = "QLD_DPI",
    identifiedBy = "QLD_DPI"
  ) |>

  # Include a georeferenceProtocol column
  mutate(
    georeferenceProtocol = case_when(
      coordinate_level == "site" ~
        "coordinates taken from the other trap at the same site, coordinateUncertaintyInMeters includes additional uncertainty associated with the unknown trap location within the site",
      coordinate_level == "trap" & coordinate_source == "GPS" ~ "coordinates from GPS device, coordinateUncertaintyInMeters includes an assumed GPS uncertainty of 30 m",
      coordinate_level == "trap" & coordinate_source == "Google Earth" ~ "coordinates from Google Earth, coordinateUncertaintyInMeters includes an assumed GPS uncertainty of 30 m",
      coordinate_level == "trap" & coordinate_source == "eGaz" ~ "coordinates from eGaz, coordinateUncertaintyInMeters includes an assumed GPS uncertainty of 30 m",
    )
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
    verbatimCoordinateSystem = verbatimCoordinateSystem.x,
    verbatimSRS,
    georeferenceProtocol,
    georeferenceSources = coordinate_source,

    # Identification fields
    identifiedBy,

    # Taxon fields
    gazella:vanderkelleni

  ) |>
  rename_with(
    .fn = \(x) case_when(
      x == "gazella" ~ "Digitonthophagus gazella",
      x %in% c("sagittarius", "nigriventris", "binodis") ~
        str_c("Onthophagus", x, sep = " "),
      x == "militaris" ~ "Liatongus militaris",
      x %in% c("alexis", "viridulus", "caffer", "pecuarius", "vanderkelleni") ~
        str_c("Onitis", x, sep = " "),
      x %in% c("intermedius", "africanus") ~
        str_c("Euoniticellus", x, sep = " "),
      x %in% c("spinipes", "rubrus") ~
        str_c("Sisyphus", x, sep = " "),
      x == "elphenor" ~ "Copris elphenor",
      TRUE ~ x
    )
  )


# Survey data -------------------------------------------------------------


# 2001-2003 survey --------------------------------------------------------

# Import raw data ---------------------------------------------------------

## Load and initial formatting of raw data
qld_2001_2003_survey_raw <-
  read_csv("data-raw/data-files/qld_2001_2003_survey.csv") |>

  # Change names
  rename(latitude = verbatimLatitude,
         longitude = verbatimLongitude)


# Darwin Core Event Details -----------------------------------------------

qld_2001_2003_survey_event <-


  # Generate event fields with dwc_Event()
  # Do this separately for the samples where there is a date range vs where
  # there is a single date

  bind_rows(
    qld_2001_2003_survey_raw |>
      filter(date_setup == date_collect) |>
      mutate(
        dwc_Event(
          start = date_setup,
          habitat = "pasture",
          samplingProtocol = samplingProtocol
        ),

        verbatimEventDate = as.character(verbatimEventDate)

      ),

    qld_2001_2003_survey_raw |>
      filter(date_setup != date_collect) |>
      mutate(
        dwc_Event(
          start = date_setup,
          end = date_collect,
          habitat = "pasture",
          samplingProtocol = samplingProtocol
        ),

        sampleSizeValue = NA,
        sampleSizeUnit = NA
      )
  )|>

  # Change site to locationID_site to match other data
  mutate(
    locationID_site = site
  )|>

  # Assign a unique id (eventID) to each site visit
  group_by(locationID_site, date_setup) |>
  mutate(datasetName = "Queensland Dung Beetle Project",
         parentEventID = str_c(locationID_site, as.Date(date_setup), sep = "_"),
         eventID = str_c(parentEventID, sample_id, sep = "_")) |>
  ungroup()




# Darwin Core Location Details --------------------------------------------

# Check unique lon/lat for each site
qld_2001_2003_survey_event |>
  group_by(locationID_site) |>
  summarise(n = n_distinct(latitude, longitude)) |>
  filter(n > 1)

qld_2001_2003_survey_event |>
  group_by(latitude, longitude) |>
  summarise(n = n_distinct(locationID_site)) |>
  filter(n > 1)


qld_2001_2003_survey_location <-

  # Make a tibble with one row per survey
  qld_2001_2003_survey_event |>

  mutate(gps_uncertainty = if_else(coordinate_level == "property",
                                   2000,
                                   10000)) |>

  group_by(locationID_site) |>
  summarise(verbatimLongitude = longitude[1],
            verbatimLatitude = latitude[1],
            verbatimCoordinateSystem = verbatimCoordinateSystem[1],
            gps_uncertainty = gps_uncertainty[1]) |>

  # Use dwc_Location() to generation Darwin Core location terms
  mutate(dwc_Location(
    longitude = verbatimLongitude,
    latitude = verbatimLatitude,
    verbatimCoordinateSystem = verbatimCoordinateSystem,
    verbatimSRS = "EPSG:4326",
    gps_uncertainty = gps_uncertainty,
    localities_sf = locality_data_aus,
    localities_names = "locality_name",
    county_sf = county_stateProvince_aus
  ))

## Site information
qld_survey_site_a <-
  qld_2001_2003_survey_location |>

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
qld_survey_site_a |>
  dwc_polygon_cluster() |>
  group_by(.cluster) |>
  mutate(n = length(.cluster)) |>
  filter(n > 1) # 35

# Lots of overlapping sites due to large uncertainties, keep site names unique
# as these may indicate variation in farm management etc

leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  addPolygons(data = qld_survey_site_a,
              label = ~locationID_site, color = "blue")


# Combine event and location details --------------------------------------

# Combine event with location details
qld_2001_2003_survey_location_event <-
  left_join(qld_2001_2003_survey_event,
            qld_2001_2003_survey_location,
            by = c("locationID_site")) |>


  # Include additional fields
  mutate(

    # Add in additional fields
    basisOfRecord = "PreservedSpecimen",
    recordedBy = "QLD_DPI",
    identifiedBy = "PE",
    samplingEffort = NA_character_
  ) |>

  # Include a georeferenceProtocol column
  mutate(
    georeferenceProtocol = case_when(
      coordinate_level == "property" ~ "coordinates derived from site location description, coordinateUncertaintyInMeters includes an assumed GPS uncertainty of 2,000 m to indicate unknown survey locations within the site",
      coordinate_level == "nearest_town" ~ "coordinates are of the nearest town given in the site location description, coordinateUncertaintyInMeters includes an assumed GPS uncertainty of 10,000 m to indicate the unknown site location within the region",
    )
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
    samplingEffort,

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
    verbatimCoordinateSystem = verbatimCoordinateSystem.x,
    verbatimSRS,
    georeferenceProtocol,
    georeferenceSources = coordinate_source,

    # Identification fields
    identifiedBy,

    # Taxon fields
    gazella:caffer

  ) |>

  # Change NAs to 0s
  mutate(across(gazella:caffer, \(x) replace_na(x, 0))) |>

  rename_with(
    .fn = \(x) case_when(
      x == "gazella" ~ "Digitonthophagus gazella",
      x %in% c("obliquus", "sagittarius", "nigriventris", "binodis") ~
        str_c("Onthophagus", x, sep = " "),
      x == "militaris" ~ "Liatongus militaris",
      x %in% c("alexis", "viridulus", "caffer", "pecuarius", "vanderkelleni") ~
        str_c("Onitis", x, sep = " "),
      x %in% c("intermedius", "africanus") ~
        str_c("Euoniticellus", x, sep = " "),
      x %in% c("spinipes", "rubrus") ~
        str_c("Sisyphus", x, sep = " "),
      x == "elphenor" ~ "Copris elphenor",
      TRUE ~ x
    )
  )



# 2009-2010 survey --------------------------------------------------------


# Import raw data ---------------------------------------------------------

## Load and initial formatting of raw data
qld_2009_2010_survey_raw <-
  read_csv("data-raw/data-files/qld_2009_2010_survey.csv") |>

  # Change names
  rename(latitude = verbatimLatitude,
         longitude = verbatimLongitude)


# Darwin Core Event Details -----------------------------------------------

qld_2009_2010_survey_event <-


  # Generate event fields with dwc_Event()
  # Do this separately for the overnight samples vs those done on the same day

  bind_rows(
  qld_2009_2010_survey_raw |>
  filter(date_setup == date_collect) |>
  mutate(
    dwc_Event(
      start = date_setup,
      habitat = "pasture",
      samplingProtocol = samplingProtocol,
      samplingEffort = samplingEffort
    ),

    verbatimEventDate = as.character(verbatimEventDate)

  ),

  qld_2009_2010_survey_raw |>
    filter(date_setup != date_collect) |>
    mutate(
      dwc_Event(
        start = date_setup,
        end = date_collect,
        habitat = "pasture",
        samplingProtocol = samplingProtocol,
        samplingEffort = samplingEffort
      ),

      sampleSizeValue = NA,
      sampleSizeUnit = NA
    )
  )|>

  # Change site to locationID_site to match other data
  mutate(
    locationID_site = site
  )|>

  # Assign a unique id (eventID) to each site visit
  group_by(locationID_site, date_setup) |>
  mutate(datasetName = "Queensland Dung Beetle Project",
         parentEventID = str_c(locationID_site, as.Date(date_setup), sep = "_"),
         eventID = str_c(parentEventID, letters[1:n()], sep = "_")) |>
  ungroup()




# Darwin Core Location Details --------------------------------------------

# Check unique lon/lat for each site
qld_2009_2010_survey_event |>
  group_by(locationID_site) |>
  distinct(latitude, longitude) |>
  arrange(locationID_site) |>
  print(n = 12)


qld_2009_2010_survey_location <-

  # Make a tibble with one row per survey
  qld_2009_2010_survey_event |>
  group_by(locationID_site) |>
  summarise(verbatimLongitude = longitude[1],
            verbatimLatitude = latitude[1],
            verbatimCoordinateSystem = verbatimCoordinateSystem[1]) |>

  # Use dwc_Location() to generation Darwin Core location terms
  mutate(dwc_Location(
    longitude = verbatimLongitude,
    latitude = verbatimLatitude,
    verbatimCoordinateSystem = verbatimCoordinateSystem,
    verbatimSRS = "EPSG:4326",
    gps_uncertainty = 300,
    localities_sf = locality_data_aus,
    localities_names = "locality_name",
    county_sf = county_stateProvince_aus
  ))

## Site information
qld_survey_site_b <-
  qld_2009_2010_survey_location |>

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
qld_survey_site_b |>
  dwc_polygon_cluster() |>
  group_by(.cluster) |>
  mutate(n = length(.cluster)) |>
  filter(n > 1) # 0

leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  addPolygons(data = qld_survey_site_b,
              label = ~locationID_site, color = "blue")


# Combine event and location details --------------------------------------

# Combine event with location details
qld_2009_2010_survey_location_event <-
  left_join(qld_2009_2010_survey_event,
            qld_2009_2010_survey_location,
            by = c("locationID_site")) |>


  # Include additional fields
  mutate(

    # Add in additional fields
    basisOfRecord = "PreservedSpecimen",
    recordedBy = "WF",
    identifiedBy = "PE"
  ) |>

  # Include a georeferenceProtocol column
  mutate(
    georeferenceProtocol = case_when(
      coordinate_level == "property" ~ "coordinates from GPS device, coordinateUncertaintyInMeters includes an assumed GPS uncertainty of 300 m to indicate multiple dung pad sampling locations at the site"
    )
  ) |>

  # Update samplingEffort field
  mutate(
    samplingEffort = case_when(
      samplingEffort == "6 dung pads" ~ "Six dung pads left overnight and searched",
      samplingProtocol == "flotation" ~ NA_character_,
      TRUE ~ samplingEffort
    )
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
    samplingEffort,

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
    verbatimCoordinateSystem = verbatimCoordinateSystem.x,
    verbatimSRS,
    georeferenceProtocol,
    georeferenceSources = coordinate_source,

    # Identification fields
    identifiedBy,

    # Taxon fields
    obliquus:viridulus

  ) |>
  rename_with(
    .fn = \(x) case_when(
      x == "gazella" ~ "Digitonthophagus gazella",
      x %in% c("obliquus", "sagittarius", "nigriventris", "binodis") ~
        str_c("Onthophagus", x, sep = " "),
      x == "militaris" ~ "Liatongus militaris",
      x %in% c("alexis", "viridulus", "caffer", "pecuarius", "vanderkelleni") ~
        str_c("Onitis", x, sep = " "),
      x %in% c("intermedius", "africanus") ~
        str_c("Euoniticellus", x, sep = " "),
      x %in% c("spinipes", "rubrus") ~
        str_c("Sisyphus", x, sep = " "),
      x == "elphenor" ~ "Copris elphenor",
      TRUE ~ x
    )
  )


# Combine data ------------------------------------------------------------


# Check trap and survey locations -----------------------------------------

# Check for common site names between surveys
qld_survey_site_a |>
  filter(locationID_site %in% c(qld_survey_site_b |> pull(locationID_site)))

# Combine survey sites to single sf object
qld_survey_site <-
  bind_rows(
    qld_survey_site_a,
    qld_survey_site_b
  )

# Check for common site names between surveys and trapping
qld_survey_site |>
  filter(locationID_site %in% c(qld_site |> pull(locationID_site))) |>
  distinct(locationID_site)

qld_site |>
  filter(locationID_site %in% c(qld_survey_site |> pull(locationID_site))) |>
  distinct(locationID_site)

# Check for intersecting sites
bind_rows(qld_survey_site,
          qld_site) |>
  dwc_polygon_cluster() |>
  group_by(.cluster) |>
  mutate(n = length(.cluster)) |>
  filter(n > 1) # 2

# Helenvale_c, Helenvale_j and Cooktown all intersect. These locations all
# appear to be within what was once Helenvale Station. Change to one site

qld_site_a <-
bind_rows(qld_survey_site,
          qld_site) |>
  mutate(locationID_site = case_when(
    locationID_site %in% c("Helenvale_c", "Helenvale_j") ~ "Cooktown",
    TRUE ~ locationID_site
  )) |>
  group_by(locationID_site) |>
  summarise() |>
  st_convex_hull()

leaflet() |>
  #addProviderTiles("Esri.WorldImagery") |>
  addProviderTiles("OpenStreetMap.Mapnik") |>
  addPolygons(data = qld_site_a,
              label = ~locationID_site, color = "blue")





# Combine datasets --------------------------------------------------------

qld_trap_survey <-

  bind_rows(

    qld_2001_2003_trap_location_event,

    qld_2001_2003_survey_location_event |>
      mutate(across(verbatimLatitude:verbatimLongitude, \(x) as.character(x))),

    qld_2009_2010_survey_location_event |>
      mutate(verbatimEventDate = as.character(verbatimEventDate))
  ) |>

  # Onthophagus obliquus not recorded in 2001-2003 data - change NAs to 0s
  # that is, it would have been recorded if found
  # Some species not recorded in survey - these are all 0s
  mutate(across(`Digitonthophagus gazella`:`Onthophagus obliquus`,
                \(x) replace_na(x, 0))) |>

  # inlcude dung_type
  mutate(
    dung_type = "cattle",
    .after = identifiedBy
  )



# Check data --------------------------------------------------------------

# View species locations
map_df <-
  qld_trap_survey |>
  pivot_longer(cols = `Digitonthophagus gazella`:`Onthophagus obliquus`,
               names_to = "scientificName", values_to = "individualCount") |>
  group_by(locationID_site, scientificName) |>
  summarise(
    total = sum(individualCount),
    decimalLongitude = decimalLongitude[1],
    decimalLatitude = decimalLatitude[1]) |>
  mutate(occurrenceStatus =
           if_else(total == 0, "absent", "present")) |>
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
                       label = ~str_c(locationID_site, scientificName, sep = " "),
                       group = df,
                       color = ~factpal(occurrenceStatus),
                       opacity = 1)
  })

l_map |>
  addLayersControl(
    baseGroups = names(map_df),
    options = layersControlOptions(collapsed = FALSE)
  )


## Time series

qld_trap_survey_summary <-
  qld_trap_survey |>
  pivot_longer(cols = `Digitonthophagus gazella`:`Onthophagus obliquus`,
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


plot_ly(qld_trap_survey_summary |> ungroup(),
        x = ~date, y = ~count, hoverinfo='text',
        text= ~str_c(scientificName, date, sep = " ")) |>
  add_lines(color=~scientificName)


# Save data ---------------------------------------------------------------

# Save data for combining with that from other projects
write.csv(qld_trap_survey,
          "data-raw/qld_2001-2003_2009-2010-prepared.csv",
          row.names = FALSE,
          na = "")


# Also save trap and site polygon data for comparing with data from other
# projects
saveRDS(object = qld_trap,
        file = "data-raw/qld_trap.rds")

saveRDS(object = qld_site_a,
        file = "data-raw/qld_site.rds")
