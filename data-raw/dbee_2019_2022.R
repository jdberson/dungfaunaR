## code to prepare `dbee_2019_2022` dataset goes here

library(tidyverse)
library(dwcPrepare)
library(leaflet)
library(plotly)
library(sf)



# DBEE data managed by UWA ------------------------------------------------

# We prepare the data from the Dung Beetle Ecosystem Engineers project
# that was managed by UWA (identification and counting of samples collected by
# project partners).

# Import raw data ---------------------------------------------------------

## Load and initial formatting of raw data
dbee_raw <-
  read_csv("data-raw/data-files/dbee_2019_2022_UWA.csv") |>

  # Assign timezones
  mutate(
    date_setup = force_tz(time = date_setup, tzone = "Australia/Perth"),
    date_collect = force_tz(time = date_collect, tzone = "Australia/Perth")) |>

  # Feedback was that the term 'gauze' would cover both 'mesh bag' and 'muslin
  # cloth' in the samplingProtocol field. Update this here.
  # Also, updated with space between number and unit of measurement
  mutate(samplingProtocol =
           str_replace_all(samplingProtocol,
                           c("a meshBag|muslinCloth|meshCloth" = "gauze",
                             "1L" = "1 L",
                             "5L" = "5 L",
                             "1kg" = "1 kg",
                             "3kg" = "3 kg",
                             "5kg" = "5 kg")))

# Darwin Core Event Details -----------------------------------------------

dbee_event <-
  dbee_raw |>

  # Generate event fields with dwc_Event()
  mutate(
    dwc_Event(
      start = date_setup,
      end = date_collect,
      tzone = "Australia/Perth",
      sampleSizeUnit = "day",
      fieldNumber = fieldNumber,
      habitat = "pasture",
      samplingProtocol = samplingProtocol,
      samplingEffort = "1 trap",
      eventRemarks = eventRemarks
    )
  ) |>

  # Assign each site visit a parentEventID
  group_by(site, date_setup) |>
  mutate(parentEventID =  str_c(site, as.Date(date_setup, tz = "Australia/Perth"),
                                sep = "_")) |>
  ungroup() |>

  # Need to correct for where date not known
  mutate(
    eventDate = case_when(
      eventRemarks ==
        "Trapping dates unknown. Month inferred from sample code sequence." ~
      str_c(year(date_setup), sprintf("%02d", month(date_setup)), sep = "-"),
      TRUE ~ eventDate),

    startDayOfYear = case_when(
      eventRemarks ==
        "Trapping dates unknown. Month inferred from sample code sequence." ~
        NA_real_,
      TRUE ~ startDayOfYear),

    endDayOfYear = case_when(
      eventRemarks ==
        "Trapping dates unknown. Month inferred from sample code sequence." ~
        NA_real_,
      TRUE ~ endDayOfYear),

    day = case_when(
      eventRemarks ==
        "Trapping dates unknown. Month inferred from sample code sequence." ~
        NA_real_,
      TRUE ~ day),

    sampleSizeValue = case_when(
      eventRemarks ==
        "Trapping dates unknown. Month inferred from sample code sequence." ~
        NA_real_,
      TRUE ~ sampleSizeValue),

    sampleSizeUnit = case_when(
      eventRemarks ==
        "Trapping dates unknown. Month inferred from sample code sequence." ~
        NA_character_,
      TRUE ~ sampleSizeUnit)
  )



# Darwin Core Location Details --------------------------------------------

#data("county_stateProvince_aus")
#data("locality_data_aus")

dbee_location <-
  dbee_event |>

  # Calculate location information
  mutate(dwc_Location(
    longitude = longitude,
    latitude = latitude,
    verbatimCoordinateSystem = verbatimCoordinateSystem,
    verbatimSRS = verbatimSRS,
    gps_uncertainty = gps_uncertainty,
  ))


# Four traps at a site ~500m apart were intended to be set at the same locations
# ~ monthly. Traps could be moved ~100m to account for changing farm management
# activities, eg, to move to a nearby paddock that didn't have livestock.

# Some 'one-off' traps were also set.

# During the project coordinate and trap identifier information was assigned to
# traps with the best available information. With a complete dataset, the
# coordinate data for traps where it was not recorded by a device can now be
# improved. The below code was used to used to update coordinates where
# needed.

# Here we first cluster traps by distance to assign each to a 'cluster'. Note
# that we use only those records where we have trap level lon/lat coordinates:
# georeferenceProtocol %in% c("coordinatesFromMobileDevice")

dbee_location1 <-
  dbee_location |>

  # Remove records that had GPS coordinates assigned manually
  filter(
    georeferenceProtocol %in%
      c("coordinatesFromMobileDevice",
        "coordinatesFromMobileDevice_siteLevelUncertainty")
  ) |>
  group_by(site) |>

  # Cluster traps within a site based on those within 100m of each other
  group_modify(~dwc_point_cluster(.x, "decimalLongitude", "decimalLatitude",
                                  hclust_method = "single",
                                  h = 100))

# Check that all have been assigned a cluster
dbee_location1 |>
  filter(is.na(cluster))

# View trap/cluster locations
factpal <- colorFactor(palette.colors(17, palette = "Polychrome 36"),
                       unique(dbee_location1$cluster))

leaflet(data = dbee_location1) |>
  addProviderTiles("Esri.WorldImagery") |>
  addCircleMarkers(lng = ~decimalLongitude, lat = ~decimalLatitude,
                   label = ~stringr::str_c(fieldNumber, site, trap,
                                           cluster, sep = " "),
                   color = ~factpal(cluster))

# Check number of clusters per site
dbee_location1 |>
  group_by(site) |>
  summarise(n_clusters = n_distinct(cluster)) |>
  group_by(n_clusters) |>
  count()


# Assign cluster to traps where location information is from standard trap
# location.
dbee_location2 <-
  left_join(
    dbee_location |>
      filter(georeferenceProtocol %in%
               c("coordinatesFromTrapCentroidCollectorVerified",
                 "coordinatesFromTrapCentroidPhotoVerified")),

    dbee_location1 |>
      group_by(site, cluster) |>
      mutate(n = n_distinct(trap)) |>
      select(site, trap, cluster, n) |>
      group_by(site, trap, cluster) |>
      slice(1) |>
      ungroup()

  )

# 23 samples are from traps that have been split over multiple clusters
# This has occurred where later trapping or a low number of traps have shared
# the same name but have been placed too far to be included in the same cluster

dbee_location2 |>
  group_by(fieldNumber) |>
  mutate(n = length(fieldNumber)) |>
  filter(n > 1) |>
  select(fieldNumber, SC, eventDate, site, trap,
         cluster, georeferenceProtocol, recordedBy) #|> View()

# Remove duplicates assigned to incorrect cluster
dbee_location2 <-
  dbee_location2 |>

  # Perup
  filter(!(fieldNumber %in% c("TC-00275", "TC-02622", "TC-02650") &
             cluster == 5)) |>
  filter(!(fieldNumber == "TC-02651" & cluster == 6)) |>
  filter(!(fieldNumber == "TC-03440" & cluster == 6)) |>

  # Heywood
  filter(!(fieldNumber == "TC-00420" & cluster == 2)) |>

  # Coree
  filter(!(fieldNumber %in% c("TC-00603", "TC-00611", "TC-00615") &
             cluster == 5)) |>

  # Reedy_Creek
  filter(!(fieldNumber %in% c("TC-01812", "TC-04816") &
             cluster %in% c(5, 8))) |>
  filter(!(fieldNumber == "TC-07236" & cluster %in% c(4, 8))) |>
  filter(!(fieldNumber %in% c("TC-04813", "TC-07233") & cluster == 6)) |>

  # Wongoondy
  filter(!(fieldNumber == "TC-02772" & cluster == 5)) |>

  # Northern_Gully
  filter(!(fieldNumber == "TC-02802" & cluster == 5)) |>

  # Avenue_Range
  filter(!(fieldNumber %in% c("TC-04746", "TC-04810", "TC-07502") &
             cluster == 5)) |>

  # Woolumbool
  filter(!(fieldNumber == "TC-07192" & cluster == 4)) |>

  # Carlotta
  filter(!(fieldNumber == "TC-05855" & cluster == 3)) |>

  # Gairdner
  filter(!(fieldNumber == "TC-06494" & cluster == 4)) |>

  # Parryville.A
  filter(!(fieldNumber == "TC-08978" & cluster == 1))


# Check that all have a cluster
dbee_location2 |>
  filter(is.na(cluster)) |>
  select(fieldNumber, site, trap, cluster)

# 4 samples missing a cluster, these traps are all missing from
# dat_raw_location1:
dbee_location1 |>
  filter(
    trap %in% c(dbee_location2 |>
  filter(is.na(cluster)) |>
  pull(trap))
  )

# And there is only a single sample for each site:
dbee_event |>
  filter(
    trap %in% c(dbee_location2 |>
                  filter(is.na(cluster)) |>
                  pull(trap))
  )

# Check the clusters assigned to each site
dbee_location1 |>
  filter(
    site %in% c(dbee_location2 |>
                  filter(is.na(cluster)) |>
                  pull(site))
  ) |>
  select(fieldNumber, site, trap, cluster)

# Assign missing clusters
dbee_location2 <-
  dbee_location2 |>
  mutate(
    cluster = case_when(
      fieldNumber %in% c("TC-00281") ~ 4, # Cherry_Tree Pool site
      fieldNumber %in% c("TC-07859") ~ 2, # Cann_River site
      fieldNumber %in% c("TC-HHILL", "TC-MONG") ~ 1, # Yellow star sites
      TRUE ~ cluster
    )
  )

# Look at all traps (all events with trap level information)
leaflet(data = bind_rows(dbee_location1, dbee_location2)) |>
  addProviderTiles("Esri.WorldImagery") |>
  addCircleMarkers(lng = ~decimalLongitude, lat = ~decimalLatitude,
                   label = ~stringr::str_c(site, cluster, sep = " "),
                   color = ~factpal(cluster))

# Generate polygons for each individual trap/cluster using data with coordinates
# recorded with the app
dbee_location_cluster1 <-

  dbee_location1 |>

  # Next create the sf object
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE) |>

  # Need to account for uncertainty around points by adding buffer
  # Note the crs transform as coordinateUncertaintyInMeters is in m
  st_transform(crs = st_crs("EPSG:7844")) %>%
  st_buffer(dist = .$coordinateUncertaintyInMeters) |>

  # Need to make valid due to issue with one row of data at Newholme (TC-00313a)
  st_make_valid() |>

  # Summarise to convex hull
  group_by(site, cluster) |>
  summarise() |>
  st_convex_hull() |>
  mutate(area = st_area(geometry)) |>
  st_transform(crs = st_crs("EPSG:4326"))



# Extract centroids to assign to trapping events with coordinates to be assigned
# from trap centroid
dbee_location2 <-
  left_join(dbee_location2,

            dbee_location_cluster1 |>
              mutate(centroid = st_centroid(geometry),
                     decimalLongitude_std = st_coordinates(centroid)[, 1],
                     decimalLatitude_std = st_coordinates(centroid)[, 2],
                     .before = geometry) |>
              select(-centroid) |>
              st_drop_geometry(),

            by = c("site", "cluster"))


# Lons / Lats should be similar to original - check in case an error was made
ggplot(data = dbee_location2,
       aes(x = verbatimLongitude, y = decimalLongitude_std)) +
  geom_point() +
  geom_smooth()

# Lons / Lats should be similar to original - check in case an error was made
ggplot(data = dbee_location2,
       aes(x = verbatimLatitude, y = decimalLatitude_std)) +
  geom_point() +
  geom_smooth()


# Calculate distance from centroid to furthermost trap to estimate the
# uncertainty for traps assigned the trap centroid coordinates - min 31 m
distance_centroid_to_edge <- function(polygon){
  as.numeric(max(st_distance(st_centroid(polygon), st_cast(polygon, "POINT"))))
}


trap_uncertainty <- data.frame(site = NA_character_,
                               cluster = NA_real_,
                               uncertainty = NA_real_)
for(i in 1:length(dbee_location_cluster1$site)){
  trap_uncertainty[i, "site"] <- dbee_location_cluster1[i, "site"]
  trap_uncertainty[i, "cluster"] <- dbee_location_cluster1[i, "cluster"]
  trap_uncertainty[i, "uncertainty"] <- distance_centroid_to_edge(dbee_location_cluster1[i,])
}

# Assign centroid coordinates and uncertainty to traps in dbee_location2
dbee_location2 <-
  dbee_location2 |>

  # if_else required due to four traps that are single traps at a site and were
  # not included in dbee_location1
  # (i.e. do not have a polygon with centroid from dbee_location1 data)
  mutate(
    decimalLatitude = if_else(!is.na(decimalLatitude_std),
                              decimalLatitude_std, decimalLatitude),
    decimalLongitude = if_else(!is.na(decimalLongitude_std),
                               decimalLongitude_std, decimalLongitude)) |>
  select(-c(decimalLatitude_std, decimalLongitude_std, area)) |>

  # assign uncertainty
  left_join(
    trap_uncertainty,
    by = c("site", "cluster")
  ) |>

  mutate(coordinateUncertaintyInMeters = case_when(
    # Feedback from partners indicated that TC-MONG trap location may be
    # inaccurate. Assign greater GPS uncertainty.
    fieldNumber == "TC-MONG" ~ 1001,
    is.na(uncertainty) ~ 31,
    uncertainty < 31 ~ 31,
    TRUE ~ round(uncertainty, 0)
  )) |>
  select(-uncertainty)


# Recalculate trap polygons to incorporate additional uncertainty associated
# with traps assigned centroid
dbee_location_cluster2 <-

  bind_rows(dbee_location1,
            dbee_location2) |>

  # Next create the sf object
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE) |>

  # Need to account for uncertainty around points by adding buffer
  # Note the crs transform as coordinateUncertaintyInMeters is in m
  st_transform(crs = st_crs("EPSG:7844")) %>%
  st_buffer(dist = .$coordinateUncertaintyInMeters) |>

  # Need to make valid due to issue with one row of data at Newholme (TC-00313a)
  st_make_valid() |>

  # Summarise to convex hull
  group_by(site, cluster) |>
  summarise() |>
  st_convex_hull() |>
  mutate(area = st_area(geometry)) |>
  st_transform(crs = st_crs("EPSG:4326"))

# View trap polygons
leaflet(data = dbee_location_cluster2) |>
  addProviderTiles("Esri.WorldImagery") |>
  addPolygons(label = ~str_c(site, cluster, sep = " "))

# Check if any trap locations intersect
dbee_location_cluster2 |>
  dwc_polygon_cluster() |>
  group_by(.cluster) |>
  mutate(n = length(.cluster)) |>
  filter(n > 1) # 0

# Check distance between trap centroids
nearest <-
  dbee_location_cluster2 |>
  st_centroid() |>
  st_nearest_feature()

trap_distances <- st_distance(dbee_location_cluster2 |>
                                st_centroid(),
                              dbee_location_cluster2[nearest, ] |>
                                st_centroid(),
                              by_element = TRUE)

dbee_location_cluster2 <-
  bind_cols(dbee_location_cluster2,
            distance = trap_distances)

trap_distances_by_site <-
  dbee_location_cluster2 |>
  mutate(trap = str_c(site, cluster)) |>
  group_by(site) |>
  summarise(min(distance), mean(distance), max(distance)) |>
  st_drop_geometry()

# View(trap_distances_by_site)

# All distances between centroids > 100m


## Finalise trap information
dbee_trap <-
  dbee_location_cluster2 |>
  rename(locationID_site = site, footprintWKT = geometry) |>

  # Assign the cluster ID as the trap identifier. Where there is only one trap
  # at a site, gives this a 1 identifier
  mutate(locationID_trap =
           if_else(!is.na(cluster), str_c(locationID_site, cluster, sep = " "),
                   str_c(locationID_site, "1", sep = " ")),
         .after = locationID_site) |>
  mutate(footprintSRS  = "EPSG:4326", .after = footprintWKT) |>
  mutate(centroid = st_centroid(footprintWKT),
         decimalLatitude = st_coordinates(centroid)[, 2],
         decimalLongitude = st_coordinates(centroid)[, 1],
         .before = footprintWKT) |>
  mutate(geodeticDatum = "EPSG:4326", .after = decimalLongitude) |>
  select(-centroid, -area, -distance, -cluster)


dbee_trap_event <-
  bind_rows(dbee_location1,
            dbee_location2)|>
  rename(locationID_site = site) |>
  # Assign the cluster ID as the trap identifier. Where there is only one trap
  # at a site, gives this a 1 identifier
  mutate(locationID_trap =
           if_else(!is.na(cluster), str_c(locationID_site, cluster, sep = " "),
                                   str_c(locationID_site, "1", sep = " ")),
         .after = locationID_site) |>
  ungroup()


# Check for duplicated cluster IDs within site / date
dbee_trap_event |>
  mutate(date = lubridate::date(date_setup)) |>
  #select(fieldNumber, locationID_site, locationID_trap, date,
  #       georeferenceProtocol, decimalLongitude, decimalLatitude) |>
  group_by(locationID_site, locationID_trap, cluster, date) |>
  mutate(n = n()) |>
  filter(n > 1) |>  #View() #83 samples

  leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  addCircleMarkers(
    lng = ~decimalLongitude, lat = ~decimalLatitude,
    label = ~stringr::str_c(fieldNumber, locationID_site,
                            locationID_trap, date, sep = " "),
    color = ~factpal(cluster)) |>
  addPolygons(data = dbee_trap,
              label = ~locationID_trap)

# Some 'trap ids' duplicated within a site visit. This occurs when traps have
# have been placed close together, and/or there has been movement of trap
# locations over time. Retain trap id identifier but make users aware that
# DBEE trap identifier based on clustering rather than permanent location of
# trap


# Some checks
st_join(dbee_trap_event |>
          st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
                   crs = "EPSG:4326"),
        dbee_trap) |>
  filter(locationID_trap.x != locationID_trap.y)

dbee_trap_event |>
  group_by(fieldNumber) |>
  mutate(n = length(fieldNumber)) |>
  filter(n>1)

dbee_trap_event |>
  #filter(is.na(fieldNumber)) #0
  #filter(is.na(locationID_site)) #0
  #filter(is.na(locationID_trap)) #0
  #filter(is.na(decimalLatitude)) #0
  filter(is.na(decimalLongitude)) #0


# Remaining data are those where traps can be located within the precision of a
# site
dbee_location3 <-
  dbee_location |>

  # Remove records that had GPS coordinates assigned manually
  filter(
    !georeferenceProtocol %in% c(
      "coordinatesFromMobileDevice",
      "coordinatesFromMobileDevice_siteLevelUncertainty",
      "coordinatesFromTrapCentroidCollectorVerified",
      "coordinatesFromTrapCentroidPhotoVerified"
    )
  )

dbee_location3 |> distinct(georeferenceProtocol)
# coordinatesFromSiteCentroid

## Check for sites in dbee_location3 that are not in dat_trap
dbee_location3 |>
  filter(!site %in% c(dbee_trap |> pull(locationID_site))) |>
  distinct(site)
# Two sites:
# Strathkellar
# Larnook

# Strathkellar has coordinates for four trap locations, data were not available
# to link these coordinates to each sample, but the coordinates can be used to
# specify a site location
# Calculate site polygon from these
Strathkellar_polygon <-
  dbee_location3 |>
  filter(site == "Strathkellar") |>

  # Next create the sf object
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE) |>

  # Need to account for uncertainty around points by adding buffer
  # Note the crs transform as coordinateUncertaintyInMeters is in m
  st_transform(crs = st_crs("EPSG:7844")) |>
  # Buffer 100m around each trap location
  st_buffer(dist = 100) |>

  # Summarise to convex hull
  summarise() |>
  st_convex_hull() |>
  mutate(
    locationID_site = "Strathkellar", .before = geometry) |>
  st_transform(crs = st_crs("EPSG:4326")) |>
  rename(footprintWKT = geometry)

# Larnook is a yellow star site, with coordinates sent from house
Larnook_polygon <-
  dbee_location3 |>
  filter(site == "Larnook") |>

  # Next create the sf object
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE) |>

  # Need to account for uncertainty around points by adding buffer
  # Note the crs transform as coordinateUncertaintyInMeters is in m
  st_transform(crs = st_crs("EPSG:7844")) |>
  # Buffer 1000m around each trap location
  st_buffer(dist = 1000) |>

  # Summarise to convex hull
  summarise() |>
  st_convex_hull() |>
  mutate(
    locationID_site = "Larnook", .before = geometry) |>
  st_transform(crs = st_crs("EPSG:4326")) |>
  rename(footprintWKT = geometry)


# Calculate the remaining Site level polygons from trap polygons
dbee_site <-
  bind_rows(
    Strathkellar_polygon,
    Larnook_polygon,

    dbee_trap |>
      group_by(locationID_site) |>
      summarise() |>
      st_convex_hull()
  ) |>
  mutate(footprintSRS  = "EPSG:4326", .after = footprintWKT) |>
  mutate(centroid = st_centroid(footprintWKT),
         decimalLatitude = st_coordinates(centroid)[, 2],
         decimalLongitude = st_coordinates(centroid)[, 1],
         .before = footprintWKT) |>
  mutate(geodeticDatum = "EPSG:4326", .after = decimalLongitude) |>
  select(-centroid)

# Look a the site data
leaflet(data = dbee_site) |>
  addProviderTiles("Esri.WorldImagery") |>
  addPolygons(label = ~locationID_site)


# Calculate site level uncertainty as distance from site centroid to furthest
# edge of site polygon. Assign this value to coordinateUncertaintyInMeters for
# those events where georeferenceProtocol = coordinatesFromSiteCentroid

distance_centroid_to_edge <- function(polygon){
  as.numeric(max(st_distance(st_centroid(polygon), st_cast(polygon, "POINT"))))
}


site_uncertainty = data.frame(locationID_site = NA_character_,
                              uncertainty = NA_real_)
for(i in 1:length(dbee_site$locationID_site)){
  site_uncertainty[i, "locationID_site"] <- dbee_site[i, "locationID_site"]
  site_uncertainty[i, "uncertainty"] <- distance_centroid_to_edge(dbee_site[i,])
}


dbee_site_event <-
  left_join(
    dbee_location3 |> rename(locationID_site = site),
    dbee_site |> select(-geodeticDatum, -footprintSRS) |> st_drop_geometry(),
    by = "locationID_site") |>
  left_join(site_uncertainty, by = "locationID_site") |>
  mutate(decimalLatitude = decimalLatitude.y,
         decimalLongitude = decimalLongitude.y) |>
  mutate(coordinateUncertaintyInMeters = round(uncertainty, 0)) |>
  select(-ends_with(c(".x", ".y")), -uncertainty)


# 26 samples could not be confidently assigned the correct trap and sample
# codes from within a site visit. Assign these site centroid and uncertainty.
dbee_trap_event |>
  filter(
    georeferenceProtocol == "coordinatesFromMobileDevice_siteLevelUncertainty"
    )


dbee_trap_event <-
  left_join(
    dbee_trap_event |> ungroup(),
    dbee_site |> select(-geodeticDatum, -footprintSRS) |> st_drop_geometry(),
    by = "locationID_site") |>
  left_join(
    site_uncertainty, by = "locationID_site"
  ) |>
  mutate(
    decimalLatitude = if_else(
      georeferenceProtocol ==
        "coordinatesFromMobileDevice_siteLevelUncertainty",
      decimalLatitude.y, decimalLatitude.x
    ),

    decimalLongitude = if_else(
      georeferenceProtocol ==
        "coordinatesFromMobileDevice_siteLevelUncertainty",
      decimalLongitude.y, decimalLongitude.x
    ),

    coordinateUncertaintyInMeters = if_else(
      georeferenceProtocol ==
        "coordinatesFromMobileDevice_siteLevelUncertainty",
      round(uncertainty, 0), coordinateUncertaintyInMeters,
    )

  ) |>
  select(-ends_with(c(".x", ".y")), -uncertainty)

## Combine event level data and assign locality information

data("locality_data_aus")

dbee_location_event <-
  bind_rows(
    dbee_trap_event,
    dbee_site_event
  ) |>
  select(-n) |>
  # Assign locality information using dwc_Prepare
  mutate(locality = dwc_locality(decimalLongitude = decimalLongitude,
                                 decimalLatitude = decimalLatitude,
                                 localities_sf = locality_data_aus,
                                 localities_names = "locality_name"),
         .after = locationID_trap)

## Correct georeferencing information
dbee_location_event_a <-
  dbee_location_event |>
  mutate(

    georeferenceSources =
      case_when(
        georeferenceProtocol == "coordinatesFromMobileDevice" ~ "GPS",
        georeferenceProtocol %in%
          c("coordinatesFromTrapCentroidCollectorVerified",
            "coordinatesFromTrapCentroidPhotoVerified",
            "coordinatesFromSiteCentroid",
            "coordinatesFromMobileDevice_siteLevelUncertainty") ~  "collector"
      ),


    georeferenceProtocol = case_when(
      georeferenceProtocol == "coordinatesFromMobileDevice" ~
        "coordinates and GPS uncertainty from mobile phone device",
      georeferenceProtocol %in% c(
        "coordinatesFromMobileDevice_siteLevelUncertainty",
        "coordinatesFromSiteCentroid") ~
        "coordinates taken from the site centroid, coordinateUncertaintyInMeters includes additional uncertainty associated with the unknown trap location within the site",
      georeferenceProtocol %in%
        c("coordinatesFromTrapCentroidCollectorVerified",
          "coordinatesFromTrapCentroidPhotoVerified") ~
        "coordinates taken from the trap centroid, coordinateUncertaintyInMeters includes additional uncertainty associated with deriving the sampling location from non-GPS sources"
    )

  )


## Assign country to county information

data("county_stateProvince_aus")
dbee_location_event_complete <-
  dwc_country_to_county(
    dbee_location_event_a,
    decimalLongitude = "decimalLongitude",
    decimalLatitude = "decimalLatitude",
    county_sf = county_stateProvince_aus,
    country_column_name = "country",
    countryCode_column_name = "countryCode",
    stateProvince_column_name = "stateProvince",
    county_column_name = "county"
  )




## Summary of where we are at

# We now have three potentially useful datasets:
dbee_site # site polygons
dbee_trap # trap polygons
dbee_location_event_complete # event and location information for each sample



# Finalise data -----------------------------------------------------------

# The DBEE data includes event level information for samples that have not been
# processed. Here we filter the data to include only that which includes sample
# counts. We also select those columns we wish to keep.

dbee_location_event_processed <-
  dbee_location_event_complete |>

  # Final update to samplingProtocol field to keep consistent with other samples
  # Move information to event remarks.
  mutate(
    eventRemarks =
      if_else(str_detect(samplingProtocol, "mesh with 2.5mm openings"),
              "gauze with 2.5 mm openings used to wrap dung", eventRemarks),
    samplingProtocol =
           str_replace(samplingProtocol, "mesh with 2.5mm openings", "gauze"))|>

  # update locationID_trap field
  # some trap location identifiers included where the trap location is
  # unknown within a site - change these to NA
  mutate(
    locationID_trap = case_when(
      georeferenceProtocol == "coordinates taken from the site centroid, coordinateUncertaintyInMeters includes additional uncertainty associated with unknown trap location within the site" ~
        NA_character_,
      TRUE ~ locationID_trap
    )
    )|>

  # Correction to occurrence remarks
  mutate(
    occurrenceRemarks = case_when(
      occurrenceRemarks ==
        "Zero Scarabaeinae inferred from the trap collection report." ~
        "Zero deliberately introduced dung beetles inferred from the trap collection report.",
      occurrenceRemarks == "Onthophagus taurus counted using the image method." ~
        "Onthophagus taurus counted using the area method.",
      TRUE ~ occurrenceRemarks
    )
  ) |>

  # flotation samples assigned sampleSizeValue
  # of 0, change these to NA
  mutate(
    sampleSizeValue = if_else(
      samplingProtocol == "flotation", NA_real_, sampleSizeValue
    )
  ) |>

  # Filter to where there are count / occurrence data
  filter(sample_processed == "Y") |>

  # Select and order columns to keep
  select(

    # Record-level
    datasetName,
    basisOfRecord,
    fieldNumber,
    SC, #  NEEDED FOR catalogNumber

    # Occurrence fields
    recordedBy,
    occurrenceRemarks,

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

    # Additional fields
    dung_type,

    # Taxon fields
    `Bubas bison`:`Sisyphus spinipes`

  )



# Check data --------------------------------------------------------------

# View values for descriptive fields
distinct(dbee_location_event_processed, datasetName)
distinct(dbee_location_event_processed, basisOfRecord)
distinct(dbee_location_event_processed, recordedBy)
distinct(dbee_location_event_processed, samplingProtocol)
distinct(dbee_location_event_processed, dung_type)
distinct(dbee_location_event_processed, identifiedBy)
distinct(dbee_location_event_processed, stateProvince)
distinct(dbee_location_event_processed, georeferenceProtocol)
distinct(dbee_location_event_processed, eventRemarks)
distinct(dbee_location_event_processed, occurrenceRemarks)
distinct(dbee_location_event_processed, verbatimCoordinateSystem)
distinct(dbee_location_event_processed, verbatimSRS)

# View species locations
map_df <-
  dbee_location_event_processed |>
  pivot_longer(cols = `Bubas bison`:`Sisyphus spinipes`,
               names_to = "scientificName", values_to = "individualCount") |>
  filter(individualCount != 0) |>
  group_by(locationID_site, scientificName) |>
  summarise(decimalLongitude = decimalLongitude[1],
            decimalLatitude = decimalLatitude[1],
            total = sum(individualCount, na.rm = TRUE)) |>
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
                       label = ~str_c(locationID_site, total, scientificName, sep = " "),
                       radius = ~log(total),
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

dbee_occurrence_summary <-
  dbee_location_event_processed |>
  pivot_longer(cols = `Bubas bison`:`Sisyphus spinipes`,
               names_to = "scientificName", values_to = "individualCount") |>

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


plot_ly(dbee_occurrence_summary |> ungroup(),
        x = ~date, y = ~count, hoverinfo='text',
        text= ~str_c(scientificName, date, sep = " ")) |>
  add_lines(color=~scientificName)



# Save data ---------------------------------------------------------------

# Save data for combining with that from other projects
write.csv(dbee_location_event_processed,
          "data-raw/dbee_2019_2022-prepared.csv",
          row.names = FALSE,
          na = "")

# Also save site and trap polygon data for comparing with data from other
# projects

saveRDS(object = dbee_site,
        file = "data-raw/dbee_site.rds")

saveRDS(object = dbee_trap,
        file = "data-raw/dbee_trap.rds")



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
# [1] gtable_0.3.4             htmlwidgets_1.6.2        lattice_0.21-8
# [4] tzdb_0.4.0               leaflet.providers_1.13.0 vctrs_0.6.3
# [7] tools_4.3.1              crosstalk_1.2.0          generics_0.1.3
# [10] parallel_4.3.1           proxy_0.4-27             fansi_1.0.4
# [13] pkgconfig_2.0.3          Matrix_1.5-4.1           KernSmooth_2.23-21
# [16] data.table_1.14.8        RColorBrewer_1.1-3       lifecycle_1.0.3
# [19] farver_2.1.1             compiler_4.3.1           munsell_0.5.0
# [22] htmltools_0.5.6          usethis_2.2.2            class_7.3-22
# [25] yaml_2.3.7               lazyeval_0.2.2           jquerylib_0.1.4
# [28] pillar_1.9.0             crayon_1.5.2             ellipsis_0.3.2
# [31] classInt_0.4-9           wk_0.8.0                 nlme_3.1-162
# [34] tidyselect_1.2.0         digest_0.6.33            stringi_1.7.12
# [37] labeling_0.4.3           splines_4.3.1            fastmap_1.1.1
# [40] grid_4.3.1               colorspace_2.1-0         cli_3.6.1
# [43] magrittr_2.0.3           utf8_1.2.3               e1071_1.7-13
# [46] withr_2.5.0              scales_1.2.1             sp_2.0-0
# [49] bit64_4.0.5              timechange_0.2.0         httr_1.4.7
# [52] bit_4.0.5                hms_1.1.3                viridisLite_0.4.2
# [55] mgcv_1.8-42              s2_1.1.4                 rlang_1.1.1
# [58] Rcpp_1.0.11              glue_1.6.2               geosphere_1.5-18
# [61] DBI_1.1.3                rstudioapi_0.15.0        vroom_1.6.3
# [64] jsonlite_1.8.7           R6_2.5.1                 fs_1.6.3
# [67] units_0.8-3
