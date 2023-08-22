## code to prepare `county_stateProvince_aus` dataset goes here


library("sf")
library("tidyverse")


# Download shape file -----------------------------------------------------

# Download
download.file(
  url = "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/LGA_2022_AUST_GDA2020_SHP.zip",
  destfile = "data-raw/data-files/LGA_2022_AUST_GDA2020_SHP.zip",
  mode = "wb")

# Unzip
unzip("data-raw/data-files/LGA_2022_AUST_GDA2020_SHP.zip",
      exdir = "data-raw/data-files/LGA_2022_AUST_GDA2020_SHP")

# Remove .zip folder
file.remove("data-raw/data-files/LGA_2022_AUST_GDA2020_SHP.zip")


# Load and process data ---------------------------------------------------

# Load data
LGA_2022_AUST_GDA2020 <-
  st_read("data-raw/data-files/LGA_2022_AUST_GDA2020_SHP")

# Check for invalid geometries
LGA_2022_AUST_GDA2020[!st_is_valid(LGA_2022_AUST_GDA2020), ] # none

# Check for empty geometries
LGA_2022_AUST_GDA2020[st_is_empty(LGA_2022_AUST_GDA2020), ]

# Remove empty geometries
LGA_2022_AUST_GDA2020 <-
  LGA_2022_AUST_GDA2020 %>%
  filter(!st_is_empty(geometry))

# Rename columns to Darwin Core terms
county_stateProvince_aus <-
  LGA_2022_AUST_GDA2020 %>%
  select(county = LGA_NAME22,
         stateProvince = STE_NAME21,
         country = AUS_NAME21,
         countryCode = AUS_CODE21)


# Plot
ggplot(data = county_stateProvince_aus) +
  geom_sf(aes(fill = county)) +
  theme(legend.position = "none")


# Write data --------------------------------------------------------------

usethis::use_data(county_stateProvince_aus, overwrite = TRUE, compress = "xz")


