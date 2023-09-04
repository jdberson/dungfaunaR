# To reload the data if starting from here --------------------------------

devtools::load_all()
library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(ggspatial)
library(patchwork)

dbee_trap <-
  readRDS("data-raw/dbee_trap.rds")

qld_trap <-
  readRDS("data-raw/qld_trap.rds")



# Assign site type variable for use throughout ----------------------------

dungfauna_event_updated <-
  dungfauna_event |>
  group_by(locationID_site) |>
  mutate(n_visits = n_distinct(parentEventID),
         site_type = case_when(
           str_detect(samplingProtocol, "trap") & n_visits > 2 ~ "Monitoring sites",
           TRUE ~ "Ad hoc sampling sites",
         )) |>
  # Two sites are both (i.e. were trapped and surveyed) - change to monitoring
  mutate(n_types = n_distinct(site_type)) |>
  mutate(site_type = if_else(n_types ==  2, "Monitoring sites", site_type)) |>
  mutate(n_types = n_distinct(site_type)) |>
  select(-n_types)




# Figures and other information for data paper ----------------------------

# Set ggplot theme
theme_set(theme_bw())
theme_update(
  strip.text = element_text(size=12),
  strip.background = element_blank(),
  panel.border = element_rect(colour="black", fill=NA),
  legend.position="none",
  axis.text.y = element_text(size=10, colour="black"),
  axis.text.x = element_text(size=10, colour="black"),
  axis.title.y = element_text(size=12, colour="black"),
  axis.title.x = element_text(size=12, colour="black"),
  text=element_text(family="serif"))




# Information for abstract ------------------------------------------------

# N presence records
dungfauna_occurrence |>
  filter(occurrenceStatus == "present") |>
  count()
# 22,718 presence records

# N absence records
dungfauna_occurrence |>
  filter(occurrenceStatus == "absent") |>
  count()
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
# 1752807 # DOES INCLUDE SURVEY BEETLES - USED IN ABSTRACT


# Class II A.3 ------------------------------------------------------------

# Period of study
dungfauna_occurrence |>
  group_by(datasetName) |>
  distinct(year) |>
  arrange(year)



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


# Figure
ggplot(aus_map) +
  geom_sf() +
  coord_sf(xlim=c(112, 155), ylim=c(-43, -10)) +
  geom_point(
    data =
      dungfauna_event |>
      group_by(locationID_site) |>
      summarise(
        decimalLongitude = mean(decimalLongitude),
        decimalLatitude = mean(decimalLatitude),
        n_visits = n_distinct(parentEventID)) |>
      arrange(n_visits) |>
      mutate(site_type = if_else(
        n_visits < 3, "(a) Ad hoc sampling sites", "(b) Monitoring sites"
      )),
    aes(x=decimalLongitude , y=decimalLatitude, colour = n_visits), size = 0.2) +
  scale_colour_viridis_c(
    limits = c(1, 33), na.value = "yellow",
    begin = 0, end = 0.99, option = "plasma", alpha = 1) +
  ylab("Latitude") +
  xlab("Longitude") +
  ggtitle("") +
  #annotation_scale(location = "bl", text_family = "serif") +
  annotation_north_arrow(location = "br",
                         width = unit(0.5, "cm"),
                         height = unit(0.5, "cm"),
                         style = north_arrow_orienteering(text_family = "serif", text_size = 6)) +
  guides(colour = guide_colourbar(title = "No. site visits")) +
  facet_wrap(~site_type, ncol = 2) +
  theme(legend.position = "right")


ggsave(
  "data-raw/data-paper/paper_figure_1.png",
  width = 18,
  height = 9,
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
  dplyr::select(-c(mean_period, min_period, max_period, mean_visits, min_visits, max_visits))
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
  mutate(n_visits = n_distinct(parentEventID),
         site_type = case_when(
           str_detect(samplingProtocol, "trap") & n_visits > 2 ~ "Monitoring sites",
           TRUE ~ "Ad hoc sampling sites",
         )) |>
  group_by(datasetName, site_type, year, month) |>
  summarise(n_visits = n_distinct(locationID_site)) |>
  mutate(yearmonth = lubridate::as_date(str_c(year,month, sep = "-"), format = "%Y-%m"))


# Queensland
fig2a <- ggplot(data = fig_2_data |>
                  filter(datasetName == "Queensland Dung Beetle Project" &
                           !year %in% c(2009, 2010)),
                aes(x = yearmonth, y = n_visits, fill = site_type)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  scale_x_date(date_breaks = "6 months", date_minor_breaks = "2 month", date_labels = "%b %Y", limits = c(as_date("2001-01-01"), as_date("2004-11-1"))) +
  xlab("Date") +
  ylab("Number of sites visited") +
  ggtitle("A. Queensland Dung Beetle Project") +
  guides(fill = guide_legend(title = "Type of site")) +
  theme(legend.position = c(0.875, 0.764))

# SW-WA
fig2b <- ggplot(data = fig_2_data |>
                  filter(datasetName == "South-Western Australian Dung Beetle Survey and Monitoring Project"),
                aes(x = yearmonth, y = n_visits, fill = site_type)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  scale_x_date(date_breaks = "6 months", date_minor_breaks = "2 month", date_labels = "%b %Y", limits = c(as_date("2012-01-01"), as_date("2015-11-1"))) +
  xlab("Date") +
  ylab("Number of sites visited") +
  ggtitle("B. South-Western Australian Dung Beetle Survey and Monitoring Project")

# DBEE
fig2c <- ggplot(data = fig_2_data |>
                  filter(datasetName == "Dung Beetle Ecosystem Engineers"),
                aes(x = yearmonth, y = n_visits, fill = site_type)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  scale_x_date(date_breaks = "6 months", date_minor_breaks = "2 month", date_labels = "%b %Y", limits = c(as_date("2019-01-01"), as_date("2022-11-1"))) +
  xlab("Date") +
  ylab("Number of sites visited") +
  ggtitle("C. Dung Beetle Ecosystem Engineers Project")




fig2a / fig2b / fig2c

ggsave(
  "data-raw/data-paper/paper_figure_2.png",
  width = 18,
  height = 20,
  units = "cm",
  dpi = 600
)

# Caption info
fig_2_data |>
  filter(datasetName == "Queensland Dung Beetle Project" & year %in% c(2009, 2010))



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
(18+8+6) - 6


# Alternative code for moving traps in qld
dungfauna_event |>
  filter(datasetName == "Queensland Dung Beetle Project") |>
  group_by(locationID_site) |>
  mutate(n = n_distinct(locationID_trap)) |>
  filter(n > 2) |>
  group_by(locationID_trap) |>
  mutate(n_events = n()) |>
  filter(n_events == 1) |>
  ungroup() |>
  summarise(
    n_sites = n_distinct(locationID_site),
    n = n()
  )


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
  filter(datasetName == "Queensland Dung Beetle Project" & year %in% c(2009, 2010)) |>
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

dungfauna_occurrence |>
  distinct(datasetName)

dungfauna_occurrence |>
  distinct(basisOfRecord)

dungfauna_occurrence |>
  distinct(scientificName) |>
  print(n = 23)

dungfauna_occurrence |>
  distinct(recordedBy) |>
  print(n = 14)

dungfauna_occurrence |>
  distinct(occurrenceStatus)

dungfauna_occurrence |>
  distinct(absence_qualifier)

dungfauna_occurrence |>
  distinct(occurrenceRemarks)

dungfauna_occurrence |>
  distinct(samplingProtocol)

dungfauna_occurrence |>
  distinct(eventRemarks)

dungfauna_occurrence |>
  distinct(geodeticDatum)

dungfauna_occurrence |>
  distinct(verbatimCoordinateSystem)

dungfauna_occurrence |>
  distinct(verbatimSRS)

dungfauna_occurrence |>
  distinct(georeferenceProtocol)

dungfauna_occurrence |>
  distinct(georeferenceSources)

dungfauna_occurrence |>
  distinct(identifiedBy)

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
# [1] patchwork_1.1.3       ggspatial_1.1.9       plotly_4.10.2
# [4] leaflet_2.2.0         sf_1.0-14             lubridate_1.9.2
# [7] forcats_1.0.0         stringr_1.5.0         dplyr_1.1.2
# [10] purrr_1.0.2           readr_2.1.4           tidyr_1.3.0
# [13] tibble_3.2.1          ggplot2_3.4.3         tidyverse_2.0.0
# [16] dungfaunaR_0.0.0.9000
#
# loaded via a namespace (and not attached):
# [1] DBI_1.1.3                s2_1.1.4                 remotes_2.4.2.1
# [4] rlang_1.1.1              magrittr_2.0.3           leaflet.extras_1.0.0
# [7] e1071_1.7-13             compiler_4.3.1           systemfonts_1.0.4
# [10] png_0.1-8                callr_3.7.3              vctrs_0.6.3
# [13] quadprog_1.5-8           profvis_0.3.8            wk_0.8.0
# [16] pkgconfig_2.0.3          crayon_1.5.2             fastmap_1.1.1
# [19] ellipsis_0.3.2           labeling_0.4.3           lwgeom_0.2-13
# [22] leafem_0.2.0             utf8_1.2.3               promises_1.2.1
# [25] sessioninfo_1.2.2        tzdb_0.4.0               ps_1.7.5
# [28] ragg_1.2.5               bit_4.0.5                rnaturalearthhires_0.2.1
# [31] cachem_1.0.8             jsonlite_1.8.7           later_1.3.1
# [34] terra_1.7-39             parallel_4.3.1           prettyunits_1.1.1
# [37] R6_2.5.1                 bslib_0.5.1              stringi_1.7.12
# [40] RColorBrewer_1.1-3       pkgload_1.3.2.1          jquerylib_0.1.4
# [43] stars_0.6-3              Rcpp_1.0.11              usethis_2.2.2
# [46] base64enc_0.1-3          directlabels_2023.8.25   httpuv_1.6.11
# [49] timechange_0.2.0         tidyselect_1.2.0         rnaturalearth_0.3.4
# [52] rstudioapi_0.15.0        abind_1.4-5              ggtext_0.1.2
# [55] codetools_0.2-19         miniUI_0.1.1.1           curl_5.0.2
# [58] processx_3.8.2           pkgbuild_1.4.2           lattice_0.21-8
# [61] shiny_1.7.5              withr_2.5.0              shinyalert_3.0.0
# [64] desc_1.4.2               units_0.8-3              proxy_0.4-27
# [67] urlchecker_1.0.1         xml2_1.3.5               pillar_1.9.0
# [70] KernSmooth_2.23-21       shinyjs_2.1.0            generics_0.1.3
# [73] vroom_1.6.3              rprojroot_2.0.3          sp_2.0-0
# [76] hms_1.1.3                munsell_0.5.0            scales_1.2.1
# [79] xtable_1.8-4             class_7.3-22             glue_1.6.2
# [82] lazyeval_0.2.2           tools_4.3.1              data.table_1.14.8
# [85] fs_1.6.3                 grid_4.3.1               crosstalk_1.2.0
# [88] devtools_2.4.5           colorspace_2.1-0         raster_3.6-23
# [91] cli_3.6.1                textshaping_0.3.6        fansi_1.0.4
# [94] viridisLite_0.4.2        gtable_0.3.4             sass_0.4.7
# [97] digest_0.6.33            classInt_0.4-9           farver_2.1.1
# [100] htmlwidgets_1.6.2        memoise_2.0.1            htmltools_0.5.6
# [103] lifecycle_1.0.3          httr_1.4.7               shinyWidgets_0.8.0
# [106] mime_0.12                bit64_4.0.5              gridtext_0.1.5
