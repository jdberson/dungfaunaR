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
  #strip.text = element_text(face="italic", size=9, hjust=0),
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
      summarise(
        dist_mean = round(mean(distance), 0),
        dist_min = round(min(distance), 0),
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
)

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

# ggsave(
#   "data-raw/data-paper/paper_figure_2.png",
#   width = 18,
#   height = 20,
#   units = "cm",
#   dpi = 600
# )

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
      scientificName == "Digitonthophagus gazella" ~ "Africa (via Hawaii) and South Africa",
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

