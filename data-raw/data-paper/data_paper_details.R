# Code used for data paper ------------------------------------------------

# Note: some parts of this code rely on files contained within the dungfaunaR
# R project available at https://github.com/jdberson/dungfaunaR

# Load data and packages --------------------------------------------------

library(dungfaunaR)
library(tidyverse)
library(sf)
library(leaflet)
library(ggspatial)
library(patchwork)

library(geodata)
library(terra)
library(exactextractr)

library(rgee)
rgee::ee_Authenticate()
rgee::ee_Initialize()

# Trap sf objects
dbee_trap <-
  readRDS("data-raw/dbee_trap.rds")

qld_trap <-
  readRDS("data-raw/qld_trap.rds")


# Assign site type variable for use throughout ----------------------------

# In the paper we classify sites as either 'Ad hoc sampling sites' or
# 'Monitoring sites'. Ad hoc sampling sites are those where traps were deployed
# once or twice, or where a non-trapping survey took place, the remaining sites
# are Monitoring sites.

# We also broadly classify sampling events into those where traps were deployed
# vs where surveys were undertaken.

# The following code assigns sites as either ad hoc or monitoring sites
# (site_type) and sampling events as either trap or survey (survey_trap).

dungfauna_event_updated <-
  dungfauna_event |>
  group_by(locationID_site) |>
  mutate(
    n_visits = n_distinct(parentEventID),
    site_type = case_when(
      str_detect(samplingProtocol, "trap") & n_visits > 2 ~ "Monitoring sites",
      TRUE ~ "Ad hoc sampling sites",
    ),
    survey_trap = if_else(str_detect(samplingProtocol, "trap"),
                          "trap", "survey")) |>
  # Two sites are both (i.e. were trapped and surveyed) - change to monitoring
  mutate(n_types = n_distinct(site_type)) |>
  mutate(site_type = if_else(n_types ==  2, "Monitoring sites", site_type)) |>
  mutate(n_types = n_distinct(site_type)) |>
  select(-n_types)

# The following code calculates the mean coordinates for a site and transforms
# this to an sf object. This can be useful for extracting environmental
# variables for a site, as well as calculating the distances between sites.
dungfauna_event_sites <-
  dungfauna_event_updated |>
  group_by(locationID_site) |>
  summarise(decmalLongitude = mean(decimalLongitude),
            decimalLatitude = mean(decimalLatitude)) |>
  st_as_sf(coords = c("decmalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE)


# Set plotting theme ------------------------------------------------------

# Set ggplot theme
theme_set(theme_bw())
theme_update(
  strip.text = element_text(size=10),
  strip.background = element_rect(fill = NA),
  panel.border = element_rect(colour="black", fill=NA),
  legend.position="none",
  axis.text.y = element_text(size=10, colour="black"),
  axis.text.x = element_text(size=10, colour="black"),
  axis.title.y = element_text(size=12, colour="black"),
  axis.title.x = element_text(size=12, colour="black"),
  text=element_text(family="serif"))




# Information for abstract ------------------------------------------------

# N presence and records
dungfauna_occurrence |>
  count(occurrenceStatus)
# 22,718 presence records
# 213,538 absence records

# N monitoring events
dungfauna_event |>
  count()
# 10,272 sampling events

# N sites
dungfauna_event |>
  summarise(n_distinct(locationID_site))
# 546 sites

# N records with abundance data
dungfauna_occurrence |>
  filter(occurrenceStatus == "present" & !is.na(individualCount)) |>
  count()
# 21,958

# % presence records with abundance data
dungfauna_occurrence |>
  filter(occurrenceStatus == "present" & !is.na(individualCount)) |>
  count() |>
  pull() /

  dungfauna_occurrence |>
  filter(occurrenceStatus == "present") |>
  count() |>
  pull()
# 0.9665464

# Number of dung beetles identified
dungfauna_occurrence |>
  summarise(sum(individualCount, na.rm = TRUE))
# 1,752,047 # DOES NOT INCLUDE SURVEY BEETLES

# To include survey beetles (i.e. at least 1 beetle identified for presence)
dungfauna_occurrence |>
  summarise(sum(individualCount, na.rm = TRUE)) |>
  pull() +

  dungfauna_occurrence |>
  filter(occurrenceStatus == "present" & is.na(individualCount)) |>
  count() |> # 760
  pull()
# 1,752,807 # INCLUDES SURVEY BEETLES - USED IN ABSTRACT


# Class II A.3 ------------------------------------------------------------

# Period of study
dungfauna_occurrence |>
  group_by(datasetName) |>
  distinct(year) |>
  arrange(year)


# Class II B.1 ------------------------------------------------------------

## Site descriptions

# Here we use publicly available datasets to provide some descriptions of the
# sites where dung beetle monitoring activities took place.

# d. Geology, landform

# We will use Google Earth Engine to extract the elevation data for each site

dem_s <- ee$Image("AU/GA/DEM_1SEC/v10/DEM-S")

elevation_data <-
  dungfauna_event_sites %>%

  # Extract elevation data
  mutate(ee_extract(x = dem_s, y = ., scale = 5))

# Summarise the elevation data
elevation_data |>
  st_drop_geometry() |>
  summarise(mean(elevation), min(elevation), max(elevation))


# f. Site history

# Download land use data for Australia
clum_path <- tempdir()
clum_file_path <- str_c(clum_path, "geotiff_clum_50m1220m.zip", sep = "\\" )


utils::download.file(
  url = "https://www.agriculture.gov.au/sites/default/files/documents/geotiff_clum_50m1220m.zip",
  destfile =  clum_file_path)
utils::unzip(clum_file_path,
             exdir=stringr::str_trim(tools::file_path_sans_ext(clum_file_path)))
base::file.remove(clum_file_path)

# Read in land use raster data
clum <-
  terra::rast(
    paste0(clum_path,
           "\\geotiff_clum_50m1220m\\geotiff_clum_50m1220m\\clum_50m1220m.tif")
  )

# Read in the data that assigns codes in the raster file to land uses
info <-
  st_read(
    paste0(clum_path,
           "\\geotiff_clum_50m1220m\\geotiff_clum_50m1220m\\clum_50m1220m.tif.vat.dbf")
  )

# Include a 5 km buffer around sites
all_sites_buffer <-
  dungfauna_event_sites |>
  st_buffer(dist = units::set_units(5, "km"))

# The following summarises the proportion of each land use within the 5 km
# buffer zone around each site, and then assigns the land use with
# the highest proportion to each site
all_sites_landuse <-

  bind_cols(
    dungfauna_event_sites,

    # The following extracts the fraction of each land used for each site
    exactextractr::exact_extract(x = clum,
                                 y = st_transform(all_sites_buffer,
                                                  crs = st_crs(clum)),
                                 fun = "frac")|>

      # The following code summarises the extracted land use to the CL18
      # levels - see the info object for details.
      mutate(id = 1:length(frac_111)) |>
      relocate(id) |>
      pivot_longer(
        cols = !id,
        names_to = "VALUE",
        names_prefix = "frac_",
        values_to = "frac") |>
      left_join(
        info |>
          select(VALUE, AGRI_INDUS, CL18) |>
          mutate(VALUE = as.character(VALUE)),
        by = "VALUE"
      ) |>
      mutate(landuse = case_when(
        CL18 %in% c("Grazing modified pastures",
                    "Grazing native vegetation",
                    "Dryland cropping") ~ CL18,
        CL18 == "Irrigated pastures" ~ "Grazing modified pastures",
        TRUE ~ "other_landuse"
      )) |>
      filter(landuse != "other_landuse") |>
      group_by(id, landuse) |>
      summarise(frac = sum(frac)) |>
      slice_max(frac, with_ties = FALSE) |>
      select(-id)
  )


# The following gives the proportion of sites for each land use
all_sites_landuse |>
  st_drop_geometry() |>
  filter(frac != 0) |>
  count(landuse) |>
  mutate(prop = n / sum(n))

# The following gives the proportion of sampling events for each land use
left_join(
  dungfauna_event_updated |>
    ungroup(),
  all_sites_landuse |>
    st_drop_geometry() |>
    select(locationID_site, landuse, frac),
  by = c("locationID_site")
) |>
  filter(frac != 0) |>
  count(landuse) |>
  mutate(prop = n / sum(n))

# g. Climate

# Download worldclim data

# Change tempdir() to whichever directory the data should be saved to
wc_path <- tempdir()

# Download the bioclim variables for Australia
aus_bioclim <-
  worldclim_country("Australia", var = "bio", path = wc_path)

# Extract the data for each site
all_sites_bioclim <-

  dungfauna_event_sites %>%

  # Extract the bioclim variables
  terra::extract(aus_bioclim, .)

# wc2.1_30s_bio_1 is the annual mean temperature
# wc2.1_30s_bio_12 is the annual precipitation
# See: https://www.worldclim.org/data/bioclim.html

# Summarise the annual mean temperature and annual precipitation
all_sites_bioclim |>
  summarise(
    across(c(wc2.1_30s_bio_1, wc2.1_30s_bio_12),
           list(mean = mean, min = min, max = max))
  )

# Class II B.2 ------------------------------------------------------------


# How often were ad hoc sampling events at a site undertaken
dungfauna_event_updated |>
  mutate(date = date(eventDate_setup)) |>
  filter(site_type == "Ad hoc sampling sites" | survey_trap == "survey")  |>
  group_by(datasetName, survey_trap, locationID_site) |>
  summarise(
    n_sampling_events = n(),
    n_occasions = n_distinct(date)) |>
  summarise(
    sum(n_occasions),
    min(n_occasions),
    max(n_occasions)
    ) |>
  ungroup() |>
  mutate(
    #total_events = sum(`sum(n_sampling_events)`),
    total_occasions = sum(`sum(n_occasions)`))


# Class II B.2.b ----------------------------------------------------------

# Number of traps per visit
dungfauna_occurrence |>
  group_by(eventID) |>
  slice(1) |>
  group_by(datasetName, parentEventID) |>
  summarise(N = n()) |>
  ggplot(aes(x = N)) +
  geom_histogram() +
  facet_wrap(~datasetName)

# Total monitoring and ad hoc sites
dungfauna_event_updated |>
  group_by(site_type) |>
  summarise(n_sites = n_distinct(locationID_site))

# Figure 1 ----------------------------------------------------------------


# Load map of Australia
aus_map <- rnaturalearth::ne_states(country="Australia", returnclass="sf")

fig_1_data <-
  dungfauna_event |>
  group_by(datasetName, locationID_site) |>

  # Assign ad hoc sites to those surveyed or trapped
  mutate(
    survey_trap = if_else(str_detect(samplingProtocol, "trap"),
                          "trap", "survey")) |>
  summarise(
    decimalLongitude = mean(decimalLongitude),
    decimalLatitude = mean(decimalLatitude),
    n_visits = n_distinct(parentEventID),
    survey_trap = survey_trap[1]) |>
  arrange(n_visits) |>
  mutate(
    site_type = if_else(
      n_visits < 3, "Ad hoc sampling sites", "Monitoring sites"
    ),
    survey_trap = if_else(
      site_type == "Monitoring sites", "trap", survey_trap
    ),
    datasetName =
      factor(if_else(
        datasetName == "South-Western Australian Dung Beetle Survey and Monitoring Project",
        "South-Western Australian Project",
        datasetName),
        levels = c("Queensland Dung Beetle Project",
                   "South-Western Australian Project",
                   "Dung Beetle Ecosystem Engineers"))
  )

# Updated figure following reviewer feedback:
ggplot(aus_map) +
  geom_sf(fill = "white") +
  coord_sf(xlim=c(112, 155), ylim=c(-43, -10)) +
  geom_point(data = fig_1_data |>
               ungroup() |>
               select(decimalLongitude, decimalLatitude, survey_trap),
             aes(x = decimalLongitude,
                 y = decimalLatitude,
                 shape = survey_trap),
             size = 1, colour = "grey") +
  geom_point(
    data = fig_1_data,
    aes(x = decimalLongitude ,
        y = decimalLatitude,
        colour = n_visits,
        shape = survey_trap),
    size = 1) +
  scale_colour_viridis_c(
    limits = c(1, 33),
    begin = 0, end = 0.8, option = "plasma", alpha = 1) +
  scale_shape_manual(values = c(3, 20)) +

  geom_text(
    data = tibble(
      decimalLongitude = 112,
      decimalLatitude = -12,
      datasetName = factor(rep(c("Queensland Dung Beetle Project",
                          "South-Western Australian Project",
                          "Dung Beetle Ecosystem Engineers"), 2),
                          levels = c("Queensland Dung Beetle Project",
                                     "South-Western Australian Project",
                                     "Dung Beetle Ecosystem Engineers")),
      site_type = c(rep("Ad hoc sampling sites", 3), rep("Monitoring sites", 3)),
      text = c("A", "C", "E", "B", "D", "F")
    ),
    aes(x = decimalLongitude, y = decimalLatitude, label = text), size = 6, family="serif") +
  ylab("Latitude") +
  xlab("Longitude") +
  ggtitle("") +
  annotation_north_arrow(
    data = tibble(datasetName = factor("Dung Beetle Ecosystem Engineers"),
                  site_type = "Monitoring sites"),
    location = "br",
    width = unit(0.5, "cm"),
    height = unit(0.5, "cm"),
    style = north_arrow_orienteering(text_family = "serif", text_size = 6)) +
  guides(colour = guide_colourbar(title = "No. site visits",
                                  title.position = "top"),
         shape = guide_legend(title = "Sampling type",
                              override.aes = list(size = 5),
                              title.position = "top")) +
  facet_grid(datasetName ~ site_type) +
  theme(legend.position = "bottom")

ggsave(
  "data-raw/data-paper/paper_figure_1.png",
  width = 18,
  height = 23,
  units = "cm",
  dpi = 600
)



# Table 1 -----------------------------------------------------------------



# The following calculates the distance data to be used in Table 1

# Distance between regular monitoring sites (i.e. sites with traps visited
# more than twice).

dungfauna_event_location_qld_sf <-
  dungfauna_event_updated |>
  filter(datasetName == "Queensland Dung Beetle Project",
         site_type == "Monitoring sites") |>
  group_by(datasetName, locationID_site) |>
  summarise(decmalLongitude = mean(decimalLongitude),
            decimalLatitude = mean(decimalLatitude)) |>
   st_as_sf(coords = c("decmalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE)

dungfauna_event_location_dafwa_sf <-
  dungfauna_event_updated |>
  filter(datasetName == "South-Western Australian Dung Beetle Survey and Monitoring Project",
         site_type == "Monitoring sites") |>
  group_by(datasetName, locationID_site) |>
  summarise(decmalLongitude = mean(decimalLongitude),
            decimalLatitude = mean(decimalLatitude)) |>
  st_as_sf(coords = c("decmalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE)

dungfauna_event_location_dbee_sf <-
  dungfauna_event_updated |>
  filter(datasetName == "Dung Beetle Ecosystem Engineers",
         site_type == "Monitoring sites") |>
  group_by(datasetName, locationID_site) |>
  summarise(decmalLongitude = mean(decimalLongitude),
            decimalLatitude = mean(decimalLatitude)) |>
  st_as_sf(coords = c("decmalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE)


nearest_site_qld <-
  dungfauna_event_location_qld_sf |>
  st_nearest_feature()

nearest_site_dafwa <-
  dungfauna_event_location_dafwa_sf |>
  st_nearest_feature()


nearest_site_dbee <-
  dungfauna_event_location_dbee_sf |>
  st_nearest_feature()


site_distances <-
  bind_rows(
    bind_cols(
      dungfauna_event_location_qld_sf |>
        select(datasetName, locationID_site) |>
        st_drop_geometry(),

      dungfauna_event_location_qld_sf[nearest_site_qld, c("locationID_site")] |>
        st_drop_geometry(),

      distance = st_distance(dungfauna_event_location_qld_sf,
                             dungfauna_event_location_qld_sf[nearest_site_qld, ],
                             by_element = TRUE)
    ),

    bind_cols(
      dungfauna_event_location_dafwa_sf |>
        select(datasetName, locationID_site) |>
        st_drop_geometry(),

      dungfauna_event_location_dafwa_sf[nearest_site_dafwa, c("locationID_site")] |>
        st_drop_geometry(),

      distance = st_distance(dungfauna_event_location_dafwa_sf,
                             dungfauna_event_location_dafwa_sf[nearest_site_dafwa, ],
                             by_element = TRUE)
    ),

    bind_cols(
      dungfauna_event_location_dbee_sf |>
        select(datasetName, locationID_site) |>
        st_drop_geometry(),

      dungfauna_event_location_dbee_sf[nearest_site_dbee, c("locationID_site")] |>
        st_drop_geometry(),

      distance = st_distance(dungfauna_event_location_dbee_sf,
                             dungfauna_event_location_dbee_sf[nearest_site_dbee, ],
                             by_element = TRUE)
    )
  ) |>
  mutate(distance_km = as.numeric(distance) / 1000)



## Distance between traps at monitoring sites

dungfauna_event_trap_qld_sf <-
  dungfauna_event |>
  filter(datasetName == "Queensland Dung Beetle Project",
         str_detect(georeferenceProtocol, "coordinates taken from other trap at the same site", negate = TRUE),
         locationID_site %in% c(dungfauna_event_location_qld_sf |> pull(locationID_site))) |>
  group_by(datasetName, locationID_site, locationID_trap) |>
  summarise(decimalLongitude = mean(decimalLongitude),
            decimalLatitude = mean(decimalLatitude)) |>
  group_by(locationID_site) |>
  mutate(n = n()) |>
  filter(n > 1) |>
  select(-n) |>
  group_by(datasetName) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE)


dungfauna_event_trap_dafwa_sf <-
  dungfauna_event |>
  filter(datasetName == "South-Western Australian Dung Beetle Survey and Monitoring Project",
         locationID_site != "Narrikup",
         basisOfRecord != "HumanObservation",
         locationID_site %in% c(dungfauna_event_location_dafwa_sf |> pull(locationID_site))) |>
  group_by(datasetName, locationID_site, locationID_trap) |>
  summarise(decmalLongitude = mean(decimalLongitude),
            decimalLatitude = mean(decimalLatitude)) |>
  group_by(locationID_site) |>
  mutate(n = n()) |>
  filter(n > 1) |>
  select(-n) |>
  group_by(datasetName) |>
  st_as_sf(coords = c("decmalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE)

# Use dbee_trap due to dbee traps moving around

dungfauna_event_trap_dbee_sf <-
  dbee_trap |>
  st_drop_geometry() |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs("EPSG:4326"),
           remove = FALSE) |>
  mutate(datasetName = "Dung Beetle Ecosystem Engineers",
         .before = locationID_site) |>
  filter(locationID_site %in% c(dungfauna_event_location_dbee_sf |> pull(locationID_site))) |>
  group_by(locationID_site) |>
  mutate(n = n()) |>
  filter(n > 1) |>
  select(-n)


nearest_trap_qld <-
  dungfauna_event_trap_qld_sf |>
  st_nearest_feature()

nearest_trap_dafwa <-
  dungfauna_event_trap_dafwa_sf |>
  st_nearest_feature()


nearest_trap_dbee <-
  dungfauna_event_trap_dbee_sf |>
  st_nearest_feature()


trap_distances <-
  bind_rows(
    bind_cols(
      dungfauna_event_trap_qld_sf |>
        select(datasetName, locationID_trap) |>
        st_drop_geometry(),

      dungfauna_event_trap_qld_sf[nearest_trap_qld, c("locationID_trap")] |>
        st_drop_geometry(),

      distance = st_distance(dungfauna_event_trap_qld_sf,
                             dungfauna_event_trap_qld_sf[nearest_trap_qld, ],
                             by_element = TRUE)
    ),

    bind_cols(
      dungfauna_event_trap_dafwa_sf |>
        select(datasetName, locationID_trap) |>
        st_drop_geometry(),

      dungfauna_event_trap_dafwa_sf[nearest_trap_dafwa, c("locationID_trap")] |>
        st_drop_geometry(),

      distance = st_distance(dungfauna_event_trap_dafwa_sf,
                             dungfauna_event_trap_dafwa_sf[nearest_trap_dafwa, ],
                             by_element = TRUE)
    ),

    bind_cols(
      dungfauna_event_trap_dbee_sf |>
        select(datasetName, locationID_trap) |>
        st_drop_geometry(),

      dungfauna_event_trap_dbee_sf[nearest_trap_dbee, c("locationID_trap")] |>
        st_drop_geometry(),

      distance = st_distance(dungfauna_event_trap_dbee_sf,
                             dungfauna_event_trap_dbee_sf[nearest_trap_dbee, ],
                             by_element = TRUE)
    )
  )




# Table 1
table_1 <-
left_join(

  # Summarise total number of sites and the number of monitoring sites
  left_join(
    dungfauna_event_updated |>
      group_by(datasetName) |>
      summarise(`Total No. sites` = n_distinct(locationID_site)),

    dungfauna_event_updated |>
      filter(site_type == "Monitoring sites") |>
      group_by(datasetName) |>
      summarise(`No. monitoring sites` = n_distinct(locationID_site))
  ),

  # Summarise the distances between sites and between traps within a site
  left_join(

    site_distances |>
      summarise(
        dist_mean = mean(distance_km),
        dist_min = min(distance_km),
        dist_max = max(distance_km)
      ) |>
      mutate(`Distance between monitoring sites, km` = str_c(round(dist_mean, 0), " (", round(dist_min, 0), "-", round(dist_max, 0), ")")) |>
      dplyr::select(-c(dist_mean, dist_min, dist_max)),


    trap_distances |>
      group_by(datasetName) |>
      # Note we need to summarise while excluding 0 values due to some QLD
      # sites having traps with the same coordinates
      mutate(distance = as.numeric(distance)) |>
      summarise(
        dist_mean = round(mean(distance[distance > 0]), 0),
        dist_min = round(min(distance[distance > 0]), 0),
        dist_max = round(max(distance), 0)
      ) |>

      mutate(`Distance between traps within a monitoring site, m` =
               case_when(
                 str_detect(datasetName, "South-Western") ~ "10*",
                 TRUE ~ str_c(dist_mean, " (", dist_min, "-", dist_max, ")"))
      )|>
      dplyr::select(-c(dist_mean, dist_min, dist_max))

  )
)

table_1


# Include time information for monitoring sites (Class II B.2.c)
table_1_updated <-
table_1 |>

  left_join(
dungfauna_event |>
  group_by(datasetName, locationID_site) |>
  summarise(min_date = min(eventDate_setup),
            max_date = max(eventDate_setup),
            n_visits = n_distinct(parentEventID)) |>
  filter(n_visits > 2) |>
  mutate(period_sampled = max_date - min_date) |>
  summarise(
    mean_period = round(mean(period_sampled),0),
    min_period = round(min(period_sampled),0),
    max_period = round(max(period_sampled),0),
    mean_visits = round(mean(n_visits),0),
    min_visits = min(n_visits),
    max_visits = max(n_visits)

  ) |>
  mutate(`Length of monitoring period at each site, days` =
           str_c(mean_period, " (", min_period, "-", max_period, ")"),

         `No. visits during monitoring period to each site` =
           str_c(mean_visits, " (", min_visits, "-", max_visits, ")")

  ) |>
  dplyr::select(
    -c(mean_period, min_period, max_period, mean_visits, min_visits, max_visits)
    )
) |>
  select(-`Total No. sites`)

table_1_updated

# Export the table
write.csv(table_1_updated, "data-raw/data-paper/table_1.csv", row.names = FALSE)




# Class II B.2.c ----------------------------------------------------------

dungfauna_event |>
  group_by(datasetName, locationID_site) |>
  summarise(min_date = min(eventDate_setup),
            max_date = max(eventDate_setup),
            n_visits = n_distinct(parentEventID)) |>
  filter(n_visits > 2) |>
  mutate(period_sampled = max_date - min_date) |>
  ungroup() |>
  summarise(max(period_sampled)) |>
  mutate(years_sampled = `max(period_sampled)` / 365)


# Figure 2 ----------------------------------------------------------------

fig_2_data <-
  dungfauna_event |>
  group_by(locationID_site) |>
  mutate(
    n_visits = n_distinct(parentEventID),
    site_type = case_when(
      str_detect(samplingProtocol, "trap") & n_visits > 2 ~ "Monitoring sites",
      TRUE ~ "Ad hoc sampling sites",
    )) |>
  group_by(datasetName, site_type, year, month) |>
  summarise(n_visits = n_distinct(locationID_site)) |>
  mutate(
    yearmonth = lubridate::as_date(str_c(year,month, sep = "-"),
                                   format = "%Y-%m"))


## Updated figure following reviewer feedback

# Queensland
fig2a <-
  ggplot(data = fig_2_data |>
           filter(datasetName == "Queensland Dung Beetle Project") |>
           mutate(
             period = if_else(
               yearmonth %in% c(as.Date("2009-12-01"), as.Date("2010-01-01"), as.Date("2010-05-01")),
               "2009-2010",
               "2001-2003"
             )) |>
           # Add in some additional months with 0 site visits - a bit of a hack
           # to help align x-axes across plots
           bind_rows(
             tibble(
               site_type = "Ad hoc sampling sites",
               n_visits = 0,
               yearmonth = c(seq(as.Date("2003-07-01"), as.Date("2004-03-01"), by = "1 month"), as.Date("2009-11-01")),
               period = c(rep("2001-2003", 9), "2009-2010")
             )

           ),
         aes(x = yearmonth, y = n_visits, fill = site_type)) +
  geom_col() +

  # Include label for 2009-2010 survey
  geom_text(
    data = tibble(
      yearmonth = as.Date("2010-01-15"),
      n_visits = 100,
      period = "2009-2010",
      label = "*2009-2010\nsurvey"
    ),
    inherit.aes = FALSE,
    aes(x = yearmonth, y = n_visits, label = label), size = 3, family="serif"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_x_date(date_breaks = "6 months", date_minor_breaks = "2 month", date_labels = "%b %Y") +
  xlab("Date") +
  ylab("Total number of sites visited each month") +
  ggtitle("A. Queensland Dung Beetle Project") +
  guides(fill = guide_legend(title = "Type of site")) +
  theme(legend.position = c(0.71, 0.764))+
  facet_grid(~period, scales = "free_x", space = "free") +
  # Format facets - note the panel.spacing was altered to help align the
  # x-axes across plots
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(0.5, "lines"))


# SW-WA
fig2b <-
  ggplot(data = fig_2_data |>
           filter(datasetName == "South-Western Australian Dung Beetle Survey and Monitoring Project"),
         aes(x = yearmonth, y = n_visits, fill = site_type)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  scale_x_date(date_breaks = "6 months", date_minor_breaks = "2 month", date_labels = "%b %Y", limits = c(as_date("2012-01-01"), as_date("2015-11-1"))) +
  xlab("Date") +
  ylab("Total number of sites visited each month") +
  ggtitle("B. South-Western Australian Dung Beetle Survey and Monitoring Project")

# DBEE
fig2c <-
  ggplot(data = fig_2_data |>
           filter(datasetName == "Dung Beetle Ecosystem Engineers"),
         aes(x = yearmonth, y = n_visits, fill = site_type)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  scale_x_date(date_breaks = "6 months", date_minor_breaks = "2 month", date_labels = "%b %Y", limits = c(as_date("2019-01-01"), as_date("2022-11-1"))) +
  xlab("Date") +
  ylab("Total number of sites visited each month") +
  ggtitle("C. Dung Beetle Ecosystem Engineers Project")

fig2a / fig2b / fig2c + plot_layout(axis_titles = "collect")

ggsave(
  "data-raw/data-paper/paper_figure_2.png",
  width = 18,
  height = 20,
  units = "cm",
  dpi = 600
)


# Class II B.3.a ----------------------------------------------------------

## Methods section

## Queensland Project

# QLD trapping method summary
dungfauna_occurrence |>
  filter(datasetName == "Queensland Dung Beetle Project") |>
  distinct(samplingProtocol)

# N monitoring sites
# recall 112 monitoring sites in Table 1

# Moving trap locations
qld_trap |>
  mutate(n_trap = n_distinct(locationID_trap)) |>
  filter(n_trap > 2) |>
  summarise(locationID_site[1],
            n_trap[1])
# 3 sites

# georeferenceSources
dungfauna_occurrence |>
  filter(datasetName == "Queensland Dung Beetle Project") |>
  distinct(georeferenceSources)

# georeferenceProtocol
dungfauna_occurrence |>
  filter(datasetName == "Queensland Dung Beetle Project") |>
  distinct(georeferenceProtocol)

# Absence qualifeer
dungfauna_occurrence |>
  filter(datasetName == "Queensland Dung Beetle Project" &
           occurrenceStatus == "absent") |>
  group_by(samplingProtocol) |>
  distinct(absence_qualifier) |>
  relocate(absence_qualifier, .before = samplingProtocol)

# Nearest town coordinates
dungfauna_occurrence |>
  filter(datasetName == "Queensland Dung Beetle Project" &
           samplingProtocol == "QLD 2001-2003 survey using the flotation method" &
           str_detect(georeferenceProtocol, "town")) |>
  summarise(n_distinct(eventID))
# 8

# QLD 2009-2010 survey method summaries
dungfauna_occurrence |>
  filter(datasetName == "Queensland Dung Beetle Project" &
           year %in% c(2009, 2010)) |>
  distinct(samplingProtocol)


## South-Western Australian Dung Beetle Survey and Monitoring Project

# Trapping and survey protocols
dungfauna_occurrence |>
  filter(datasetName == "South-Western Australian Dung Beetle Survey and Monitoring Project") |>
  distinct(samplingProtocol)

# georeferenceProtocol
dungfauna_occurrence |>
  filter(datasetName == "South-Western Australian Dung Beetle Survey and Monitoring Project") |>
  distinct(georeferenceProtocol)

# Survey protocol
dungfauna_occurrence |>
  filter(datasetName == "South-Western Australian Dung Beetle Survey and Monitoring Project",
         samplingProtocol == "Dung pads searched for dung beetles") |>
  distinct(occurrenceRemarks)


## Dung Beetle Ecosystem Engineers (DBEE) Project

# Sampling protocols
dungfauna_occurrence |>
  filter(datasetName == "Dung Beetle Ecosystem Engineers") |>
  distinct(samplingProtocol)

# dung types
dungfauna_event |>
  filter(datasetName == "Dung Beetle Ecosystem Engineers") |>
  count(dung_type)

# length of trapping
dungfauna_event |>
  filter(datasetName == "Dung Beetle Ecosystem Engineers") |>
  ggplot(aes(x = sampleSizeValue)) +
  geom_histogram()

# georeferenceSources
dungfauna_occurrence |>
  filter(datasetName == "Dung Beetle Ecosystem Engineers") |>
  distinct(georeferenceSources)

# georeferenceProtocol
dungfauna_occurrence |>
  filter(datasetName == "Dung Beetle Ecosystem Engineers") |>
  distinct(georeferenceProtocol)

# Samples with 0 counts from trap collector
dungfauna_occurrence |>
  filter(datasetName == "Dung Beetle Ecosystem Engineers") |>
  group_by(eventID) |>
  slice(1) |>
  filter(str_detect(occurrenceRemarks, "Zero")) |>
  ungroup() |>
  count()

# N taurus photo counts
dungfauna_occurrence |>
  filter(
    datasetName == "Dung Beetle Ecosystem Engineers",
    scientificName == "Onthophagus taurus",
    occurrenceStatus == "present",
    occurrenceRemarks == "Onthophagus taurus counted using the area method.") |>
  count()
104

# N samples with taurus
dungfauna_occurrence |>
  filter(
    datasetName == "Dung Beetle Ecosystem Engineers",
    scientificName == "Onthophagus taurus",
    occurrenceStatus == "present"
    ) |>
  count()
3793

dungfauna_occurrence |>
  distinct(occurrenceRemarks)

# Table 2 -----------------------------------------------------------------


# To include survey beetles (i.e. at least 1 beetle identified for presence)
dungfauna_occurrence |>
  summarise(sum(individualCount, na.rm = TRUE)) |>
  pull() +

  dungfauna_occurrence |>
  filter(occurrenceStatus == "present" & is.na(individualCount)) |>
  count() |> # 760
  pull()

db_species <-
  read_csv("data-raw/data-files/db_species.csv") |>
  select(-identificationReferences )

table_2 <-

  # Create column to make it easier summing occurrence records and abundance
  # counts
dungfauna_occurrence |>
  mutate(occurrenceStatus_numeric = case_when(
    occurrenceStatus == "present" ~ 1,
    occurrenceStatus == "absent" ~ 0,
    TRUE ~ NA_integer_), .after = occurrenceStatus
  ) |>

  # For each species calculate the number of presence records and the number
  # of specimens recorded
  group_by(scientificName) |>
  summarise(
    N_presence_records = sum(occurrenceStatus_numeric, na.rm = TRUE),
    N_counts = sum(individualCount, na.rm = TRUE) + sum(occurrenceStatus_numeric[is.na(individualCount)])
  ) |>

  # Use the db_species data to extract the scientificNameAuthorship
  left_join(db_species |>
              select(scientificName, scientificNameAuthorship)) |>

  # Include the origin and year first released
  mutate(
    Species = str_c(scientificName, scientificNameAuthorship, sep = " "),
    Origin = case_when(
      scientificName == "Bubas bison" ~ "France and Spain",
      scientificName == "Copris elphenor" ~ "Southern Africa",
      scientificName == "Copris hispanus" ~ "Spain",
      scientificName == "Digitonthophagus gazella" ~ "Zimbabwe (via Hawaii) and South Africa",
      scientificName == "Euoniticellus africanus" ~ "South Africa",
      scientificName == "Euoniticellus fulvus" ~ "France and Turkey",
      scientificName == "Euoniticellus intermedius" ~ "South Africa",
      scientificName == "Euoniticellus pallipes" ~ "Iran and Turkey",
      scientificName == "Geotrupes spiniger" ~ "France",
      scientificName == "Liatongus militaris" ~ "South Africa (via Hawaii)",
      scientificName == "Onitis alexis" ~ "Malawi, Mozambique, South Africa, and Zimbabwe",
      scientificName == "Onitis aygulus" ~ "South Africa",
      scientificName == "Onitis caffer" ~ "South Africa",
      scientificName == "Onitis pecuarius" ~ "South Africa",
      scientificName == "Onitis vanderkelleni" ~ "Kenya",
      scientificName == "Onitis viridulus" ~ "South Africa",
      scientificName == "Onthophagus binodis" ~ "South Africa",
      scientificName == "Onthophagus nigriventris" ~ "Kenya",
      scientificName == "Onthophagus obliquus" ~ "Nigeria",
      scientificName == "Onthophagus sagittarius" ~ "Sri Lanka (via Hawaii)",
      scientificName == "Onthophagus taurus" ~ "Greece, Italy, Spain, and Turkey",
      scientificName == "Sisyphus rubrus" ~ "South Africa",
      scientificName == "Sisyphus spinipes" ~ "South Africa"
    ),
    year_released = case_when(
      scientificName == "Bubas bison" ~ "1983",
      scientificName == "Copris elphenor" ~ "1977",
      scientificName == "Copris hispanus" ~ "1983",
      scientificName == "Digitonthophagus gazella" ~ "1968",
      scientificName == "Euoniticellus africanus" ~ "1971",
      scientificName == "Euoniticellus fulvus" ~ "1978",
      scientificName == "Euoniticellus intermedius" ~ "1971",
      scientificName == "Euoniticellus pallipes" ~ "1977",
      scientificName == "Geotrupes spiniger" ~ "1979",
      scientificName == "Liatongus militaris" ~ "1968",
      scientificName == "Onitis alexis" ~ "1972",
      scientificName == "Onitis aygulus" ~ "1977",
      scientificName == "Onitis caffer" ~ "1979",
      scientificName == "Onitis pecuarius" ~ "1976",
      scientificName == "Onitis vanderkelleni" ~ "1974",
      scientificName == "Onitis viridulus" ~ "1976",
      scientificName == "Onthophagus binodis" ~ "1971",
      scientificName == "Onthophagus nigriventris" ~ "1975",
      scientificName == "Onthophagus obliquus" ~ "1976",
      scientificName == "Onthophagus sagittarius" ~ "1968",
      scientificName == "Onthophagus taurus" ~ "1975",
      scientificName == "Sisyphus rubrus" ~ "1973",
      scientificName == "Sisyphus spinipes" ~ "1972"
    )) |>
  select(Species, Origin, year_released, N_presence_records, N_counts)

table_2


# Check that table matches totals given in abstract
table_2 |>
  summarise(across(c(N_presence_records, N_counts), \(x) sum(x)))
# Yes!

# Export the table
write.csv(table_2 |>
            # Format numerics to include commas every three digis
            mutate(across(N_presence_records:N_counts, scales::label_comma())),
          "data-raw/data-paper/table_2.csv", row.names = FALSE)



# Class IV.A --------------------------------------------------------------


# Size of data
nrow(dungfauna_occurrence)
ncol(dungfauna_occurrence)
file.size("data-raw/dungfauna_occurrence.csv")

dungfauna_occurrence |>
  summarise(n_distinct(eventID))


# Table 3 -----------------------------------------------------------------

# Variable names
colnames(dungfauna_occurrence)
str(dungfauna_occurrence)

# datasetName
dungfauna_occurrence |>
  distinct(datasetName)

# basisOfRecord
dungfauna_occurrence |>
  distinct(basisOfRecord)

# catalogNumber
dungfauna_occurrence |>
  summarise(n_distinct(catalogNumber, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(catalogNumber)

# scientificName
dungfauna_occurrence |>
  distinct(scientificName) |>
  print(n = 23)

# recordedBy
dungfauna_occurrence |>
  distinct(recordedBy) |>
  print(n = 14)

# individualCount
dungfauna_occurrence |>
  summarise(min(individualCount, na.rm = TRUE),
            max(individualCount, na.rm = TRUE))

# occurrenceStatus
dungfauna_occurrence |>
  distinct(occurrenceStatus)

# absence_qualifier
dungfauna_occurrence |>
  distinct(absence_qualifier)

# occurrenceRemarks
dungfauna_occurrence |>
  distinct(occurrenceRemarks)

# eventID
dungfauna_occurrence |>
  summarise(n_distinct(eventID, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(eventID)

# parentEventID
dungfauna_occurrence |>
  summarise(n_distinct(parentEventID, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(parentEventID)

# fieldNumber
dungfauna_occurrence |>
  summarise(n_distinct(fieldNumber, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(fieldNumber)

# eventDate_setup
dungfauna_occurrence |>
  summarise(min(eventDate_setup, na.rm = TRUE),
            max(eventDate_setup, na.rm = TRUE))

# eventDate_collect
dungfauna_occurrence |>
  summarise(min(eventDate_collect, na.rm = TRUE),
            max(eventDate_collect, na.rm = TRUE))

# eventDate
dungfauna_occurrence |>
  summarise(n_distinct(eventDate, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(eventDate)

# startDayOfYear
dungfauna_occurrence |>
  summarise(min(startDayOfYear, na.rm = TRUE),
            max(startDayOfYear, na.rm = TRUE))

# endDayOfYear
dungfauna_occurrence |>
  summarise(min(endDayOfYear, na.rm = TRUE),
            max(endDayOfYear, na.rm = TRUE))

# year
dungfauna_occurrence |>
  summarise(min(year, na.rm = TRUE),
            max(year, na.rm = TRUE))

# month
dungfauna_occurrence |>
  summarise(min(month, na.rm = TRUE),
            max(month, na.rm = TRUE))

# day
dungfauna_occurrence |>
  summarise(min(day, na.rm = TRUE),
            max(day, na.rm = TRUE))

# verbatimEventDate
dungfauna_occurrence |>
  summarise(n_distinct(verbatimEventDate, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(verbatimEventDate)

# samplingProtocol
dungfauna_occurrence |>
  summarise(n_distinct(samplingProtocol, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(samplingProtocol) |>
  arrange(samplingProtocol)

# sampleSizeValue
dungfauna_occurrence |>
  summarise(format(min(sampleSizeValue, na.rm = TRUE), nsmall = 5),
            format(max(sampleSizeValue, na.rm = TRUE), nsmall = 5))

# sampleSizeUnit
dungfauna_occurrence |>
  distinct(sampleSizeUnit)

# samplingEffort
dungfauna_occurrence |>
  summarise(n_distinct(samplingEffort, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(samplingEffort) |>
  arrange(samplingEffort)

# eventRemarks
dungfauna_occurrence |>
  summarise(n_distinct(eventRemarks, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(eventRemarks) |>
  arrange(eventRemarks)

# locationID_site
dungfauna_occurrence |>
  summarise(n_distinct(locationID_site, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(locationID_site)

# locationID_trap
dungfauna_occurrence |>
  summarise(n_distinct(locationID_trap, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(locationID_trap)

# country
dungfauna_occurrence |>
  distinct(country)

# countryCode
dungfauna_occurrence |>
  distinct(countryCode)

# stateProvince
dungfauna_occurrence |>
  distinct(stateProvince)

# county
dungfauna_occurrence |>
  summarise(n_distinct(county, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(county)

# locality
dungfauna_occurrence |>
  summarise(n_distinct(locality, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(locality)

# decimalLatitude
dungfauna_occurrence |>
  summarise(format(min(decimalLatitude, na.rm = TRUE), nsmall = 7),
            format(max(decimalLatitude, na.rm = TRUE), nsmall = 7))

# decimalLongitude
dungfauna_occurrence |>
  summarise(format(min(decimalLongitude, na.rm = TRUE), nsmall = 7),
            format(max(decimalLongitude, na.rm = TRUE), nsmall = 7))

# geodeticDatum
dungfauna_occurrence |>
  distinct(geodeticDatum)

# coordinateUncertaintyInMeters
dungfauna_occurrence |>
  summarise(min(coordinateUncertaintyInMeters, na.rm = TRUE),
            max(coordinateUncertaintyInMeters, na.rm = TRUE))

# coordinatePrecision
dungfauna_occurrence |>
  summarise(format(min(coordinatePrecision, na.rm = TRUE), nsmall = 5),
            format(max(coordinatePrecision, na.rm = TRUE), nsmall = 5))

# verbatimLatitude
dungfauna_occurrence |>
  summarise(n_distinct(verbatimLatitude, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(verbatimLatitude)

# verbatimLongitude
dungfauna_occurrence |>
  summarise(n_distinct(verbatimLongitude, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(verbatimLongitude)

# verbatimCoordinateSystem
dungfauna_occurrence |>
  distinct(verbatimCoordinateSystem)

# verbatimSRS
dungfauna_occurrence |>
  distinct(verbatimSRS)

# georeferenceProtocol
dungfauna_occurrence |>
  summarise(n_distinct(georeferenceProtocol, na.rm = TRUE))

dungfauna_occurrence |>
  distinct(georeferenceProtocol)

# georeferenceSources
dungfauna_occurrence |>
  distinct(georeferenceSources)

# identifiedBy
dungfauna_occurrence |>
  distinct(identifiedBy)

# dung_type
dungfauna_occurrence |>
  distinct(dung_type)

# Class IV.C --------------------------------------------------------------

# Data anomalies
dungfauna_event |>
  group_by(eventRemarks) |>
  count()


dungfauna_occurrence |>
  group_by(eventID) |>
  filter(is.na(day)) |>
  summarise(eventID[1])

# Session info ------------------------------------------------------------

sessionInfo()

# R version 4.3.2 (2023-10-31 ucrt)
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
# [1] rgee_1.1.7            exactextractr_0.9.1   geodata_0.5-9
# [4] terra_1.7-71          patchwork_1.2.0.9000  ggspatial_1.1.9
# [7] leaflet_2.2.1         sf_1.0-15             lubridate_1.9.3
# [10] forcats_1.0.0         stringr_1.5.1         dplyr_1.1.4
# [13] purrr_1.0.2           readr_2.1.4           tidyr_1.3.1
# [16] tibble_3.2.1          ggplot2_3.4.4         tidyverse_2.0.0
# [19] dungfaunaR_0.0.0.9000
#
# loaded via a namespace (and not attached):
# [1] gtable_0.3.4             raster_3.6-26            htmlwidgets_1.6.4
# [4] processx_3.8.2           lattice_0.22-5           tzdb_0.4.0
# [7] vctrs_0.6.5              tools_4.3.2              crosstalk_1.2.1
# [10] ps_1.7.5                 generics_0.1.3           proxy_0.4-27
# [13] fansi_1.0.6              pkgconfig_2.0.3          Matrix_1.6-5
# [16] KernSmooth_2.23-22       RColorBrewer_1.1-3       lifecycle_1.0.4
# [19] farver_2.1.1             compiler_4.3.2           munsell_0.5.0
# [22] codetools_0.2-19         htmltools_0.5.7          class_7.3-22
# [25] pillar_1.9.0             crayon_1.5.2             classInt_0.4-10
# [28] tidyselect_1.2.0         digest_0.6.34            stringi_1.8.3
# [31] labeling_0.4.3           fastmap_1.1.1            rnaturalearth_0.3.4
# [34] grid_4.3.2               colorspace_2.1-0         cli_3.6.2
# [37] magrittr_2.0.3           utf8_1.2.4               e1071_1.7-14
# [40] withr_3.0.0              scales_1.3.0             sp_2.1-3
# [43] timechange_0.3.0         httr_1.4.7               reticulate_1.32.0
# [46] rnaturalearthhires_0.2.1 png_0.1-8                hms_1.1.3
# [49] viridisLite_0.4.2        rlang_1.1.3              Rcpp_1.0.12
# [52] glue_1.7.0               DBI_1.2.1                rstudioapi_0.15.0
# [55] jsonlite_1.8.8           R6_2.5.1                 units_0.8-5
