
## code to prepare `dungfauna_aus` dataset goes here

# Here we combine data from multiple projects to form a single dataset with
# imported dung beetles to Australia

# Over time we will update this dataset to include additional data where they
# become available, including data from other projects, and additional species
# (eg native dung beetles).

# The data are presented in both wide format (dungfauna_event) and long
# format with each species in a sample listed on a separate row
# (dungfauna_occurrence)

library(tidyverse)
library(dwcPrepare)
library(sf)
library(leaflet)
library(plotly)

# Import raw data ---------------------------------------------------------

## QLD 2001-2002 project data
qld_2001_2010 <-
  read_csv("data-raw/qld_2001-2003_2009-2010-prepared.csv")

qld_site <-
  readRDS("data-raw/qld_site.rds")

qld_trap <-
  readRDS("data-raw/qld_trap.rds")


## WA DAWFA 2012-2014 prject data
dafwa_2012_2014 <-
  read_csv("data-raw/dafwa_wa_2012_2014-prepared.csv")

dafwa_site <-
  readRDS("data-raw/dafwa_site.rds")

dafwa_trap <-
  readRDS("data-raw/dafwa_trap.rds")


## DBEE 2019 - 2022 project data
dbee_2019_2022 <-
  read_csv("data-raw/dbee_2019_2022-prepared.csv")

dbee_site <-
  readRDS("data-raw/dbee_site.rds")

dbee_trap <-
  readRDS("data-raw/dbee_trap.rds")



# Check for intersecting sites / traps ------------------------------------

## Check across DAFWA and DBEE data (both operated in Western Australia)

# Sites
bind_rows(dafwa_site,
          dbee_site) |>
  dwc_polygon_cluster() |>
  group_by(.cluster) |>
  mutate(n = length(.cluster)) |>
  filter(n > 1) # 2

# One regular trapping site - Bullsbrook, already given the same locationID_site
# name across datasets so no need to change here

# An irregular DBEE trapping site intersects with a DAFWA survey site. Change
# the DAFWA locationID_site to match that from the DBEE data
dafwa_2012_2014 <-
dafwa_2012_2014 |>
  mutate(across(c(locationID_site, locationID_trap, eventID), \(x)
                str_replace(x, "Northcliffe", "Meerup.A")))

# Changed the above for future data updates - note that there is no count data
# for Meerup.A currently:
dbee_2019_2022 |>
  filter(locationID_site == "Meerup.A")


# Check if traps intersect at Bullsbrook
bind_rows(dafwa_trap,
          dbee_trap) |>
  filter(locationID_site  == "Bullsbrook") |>
  dwc_polygon_cluster() |>
  group_by(.cluster) |>
  mutate(n = length(.cluster)) |>
  filter(n > 1) # Only the two DAFWA traps intersect

# Traps don't intersect, check if given unique locationID_trap values
dafwa_2012_2014 |>
  filter(locationID_site == "Bullsbrook") |>
  distinct(locationID_trap)

dbee_2019_2022 |>
  filter(locationID_site == "Bullsbrook") |>
  distinct(locationID_trap)

# Traps in different projects assigned unique identifiers

# Check that there are no intersecting site names, other than the ones
# identified above
dafwa_2012_2014 |>
  filter(locationID_site %in% c(dbee_2019_2022 |> pull(locationID_site))) |>
  distinct(locationID_site)
# locationID_site
# <chr>
# 1 Jingalup
# 2 Bullsbrook
# 3 Williams

# Bullsbrook is fine, need to adjust Williams and Jingalup (note Meerup.A does
# not appear in this list as count data not available for this site and it is
# therefore absent from dbee_2019_2022)

dbee_2019_2022 <-
dbee_2019_2022 |>
  mutate(across(c(locationID_site, locationID_trap, parentEventID), \(x)
                str_replace(x, "Williams", "Williams.A"))) |>
  mutate(across(c(locationID_site, locationID_trap, parentEventID), \(x)
                str_replace(x, "Jingalup", "Jingalup.A")))


dbee_2019_2022 |>
  filter(locationID_site %in% c(qld_2001_2010 |> pull(locationID_site))) |>
  distinct(locationID_site)
# locationID_site
# <chr>
# 1 Springdale

dafwa_2012_2014 |>
  filter(locationID_site %in% c(qld_2001_2010 |> pull(locationID_site))) |>
  distinct(locationID_site)

# One common site name between QLD and DBEE projects

# Change name in QLD project
qld_2001_2010 <-
  qld_2001_2010 |>
  mutate(locationID_site = str_replace(locationID_site, "Springdale", "Springdale_a"))


# View sites
leaflet() |>
  addProviderTiles("Esri.WorldImagery") |>
  addCircleMarkers(data = dafwa_2012_2014,
                   lng = ~decimalLongitude,
                   lat = ~decimalLatitude,
                   label = ~locationID_site, color = "lightblue") |>
  addPolygons(data = dafwa_site,
              label = ~locationID_site, color = "blue") |>
  addCircleMarkers(data = dbee_2019_2022,
                   lng = ~decimalLongitude,
                   lat = ~decimalLatitude,
                   label = ~locationID_site, color = "pink") |>
  addPolygons(data = dbee_site,
              label = ~locationID_site, color = "red") |>
  addCircleMarkers(data = qld_2001_2010,
                   lng = ~decimalLongitude,
                   lat = ~decimalLatitude,
                   label = ~locationID_site, color = "lightyellow") |>
  addPolygons(data = qld_site,
              label = ~locationID_site, color = "yellow")


# Combine data ------------------------------------------------------------

# Event data --------------------------------------------------------------

dungfauna_event <-
  bind_rows(
    dbee_2019_2022 |>
      mutate(across(c(verbatimLatitude, verbatimLongitude),
                    \(x) as.character(x))),
    dafwa_2012_2014 |>
      mutate(across(c(verbatimLatitude, verbatimLongitude),
                    \(x) as.character(x))),
    qld_2001_2010
  ) %>%

  # Order species names alphabetically
  select(datasetName:dung_type, sort(colnames(.))) |>

  # Where species not found at all in project it was not included in data,
  # need to correct for this here by assigning 0s to NAs
  mutate(across(`Bubas bison`:`Sisyphus spinipes`, \(x) replace_na(x, 0))) |>

  # DBEE lon/lat has > 7 decimal places where coordinates are from trap or site
  # centroid; DBEE sampleSizeValue has up to 15 decimal places; and QLD project
  # coordinatePrecision has up to 18 decimal places. Round these here:
  mutate(across(c(decimalLongitude, decimalLatitude), \(x) round(x, 7)),
         across(c(sampleSizeValue, coordinatePrecision), \(x) round(x, 5)))

# Check there aren't values with > 7 decimal places in the data
dungfauna_event |>
  mutate(across(where(is.numeric), \(x) nchar(gsub(".*\\.|^[^.]+$", "", as.character(x))))) |>
  summarise(across(where(is.numeric), \(x) max(x, na.rm = TRUE))) |>
  select(where(~ !all(. %in% 0)))

# Note that some columns removed below

# Occurrence data ---------------------------------------------------------

# Read in species data for species level information
db_species <-
  read_csv("data-raw/data-files/db_species.csv") |>
  select(-identificationReferences )

# Combine and prepare data
dungfauna_occurrence <-
  dungfauna_event |>

  pivot_longer(cols = `Bubas bison`:`Sisyphus spinipes`,
               names_to = "scientificName", values_to = "individualCount") |>

  # Following feedback no longer including species information in final data

  # Include species information for filtering further on
  left_join(
    db_species |> select(code, scientificName, establishmentMeans, pathway),
    by = "scientificName"
  ) |>

  # Include occurrenceStatus and taxonRank
  # Include additional fields
  mutate(

    # Include occurrence status
    occurrenceStatus = case_when(
      individualCount > 0 ~ "present",
      individualCount == 0 ~ "absent"),

    # Identify records where identification is to genus or above - here all
    # identified to species
    taxonRank =  "species") |>

  # identifiedBy variable has values where the individualCount = 0, these were
  # initially kept to distinguish instances in the DBEE project where 0 counts
  # were provided by UWA vs the trap collector. Remove these here as nonsensical
  # to identify something with a count of 0
  mutate(
    identifiedBy = if_else(individualCount == 0, NA_character_, identifiedBy)
  ) |>

  # Include catalogNumber for matching DBEE data to what is on ALA
  mutate(
    catalogNumber =
      if_else(datasetName == "Dung Beetle Ecosystem Engineers",
              str_c(SC, code, sep = "_"), NA_character_),
    .after = basisOfRecord
  ) |>
  select(-code, -SC) |>

  # DAFWA survey data used visual activity ratings, not counts of beetles. Put
  # these ratings into the occurrenceRemarks and remove from count, at the same
  # time remove taurus count photo method comment from occurrenceRemarks where
  # species is not taurus and correct other comments

  mutate(
    occurrenceRemarks = case_when(

      occurrenceRemarks ==
        "Onthophagus taurus counted using the image method." &
        scientificName != "Onthophagus taurus" ~ NA_character_,

      basisOfRecord == "HumanObservation" & datasetName ==
        "South-Western Australian Dung Beetle Survey and Monitoring Project" ~
        str_c("Visual activity rating of ", individualCount),

    TRUE ~ occurrenceRemarks
    )
  ) |>

  # Remove individualCount data from DAFWA survey data and remove comment from
  # eventRemarks. Also remove individualCount data from QLD 2001-2003 survey
  mutate(

    individualCount = case_when(
      basisOfRecord == "HumanObservation" & datasetName ==
        "South-Western Australian Dung Beetle Survey and Monitoring Project" ~
        NA_integer_,
      samplingProtocol == "QLD 2001-2003 survey using the flotation method" ~ NA_integer_,
      TRUE ~ individualCount
    ),

    eventRemarks = case_when(
      eventRemarks == "Species counts are visual activity ratings" ~
        NA_character_,
      TRUE ~ eventRemarks
    )

  ) |>


  # For now, data to include deliberately introduced dung beetles only
  filter(establishmentMeans == "introduced" & pathway == "releasedForUse") |>

  # Include absence qualifier column to give an indication of what 'absent'
  # means for each event
  mutate(
    absence_qualifier = case_when(
      occurrenceStatus == "absent" &
        str_detect(samplingProtocol, "Partial|partial") ~ "Species absent from partial trap sample",

      occurrenceStatus == "absent" &
        occurrenceRemarks == "Zero deliberately introduced dung beetles inferred from the trap collection report." ~ "Species absent from trap, no sample collected",

      occurrenceStatus == "absent" &
        str_detect(samplingProtocol, "trap") ~ "Species absent from trap sample",

      occurrenceStatus == "absent" &
        samplingProtocol == "flotation" ~ "Species absent from flotation sample",

      occurrenceStatus == "absent" &
        datasetName == "Queensland Dung Beetle Project" &
        year %in% c(2009, 2010) ~ "Species absent from survey sample",

      occurrenceStatus == "absent" &
        samplingProtocol == "QLD 2001-2003 survey using the flotation method" ~ "Species not detected by citizen scientists during survey",

      occurrenceStatus == "absent" &
        datasetName ==
        "South-Western Australian Dung Beetle Survey and Monitoring Project" &
        samplingProtocol == "Dung pads searched for dung beetles" ~ "Species not detected by research scientists during survey",

    )
  ) |>

  # Select and order columns to keep
  select(

    # Record-level
    datasetName,
    basisOfRecord,

    # Occurrence fields (note occurrence fields split by the species name)
    catalogNumber,

    # Taxon fields - removed all but scientificName for now
    scientificName,
    #kingdom,
    #phylum,
    #class,
    #order,
    #family,
    #subfamily,
    #genus,
    #specificEpithet,
    #taxonRank,
    #scientificNameAuthorship

    # Occurrence fields
    recordedBy,
    individualCount,
    #establishmentMeans,
    #degreeOfEstablishment,
    #pathway,
    occurrenceStatus,
    absence_qualifier,
    occurrenceRemarks,

    # Event fields
    eventID,
    parentEventID,
    fieldNumber,
    eventDate_setup,
    eventDate_collect,
    eventDate,
    startDayOfYear,
    endDayOfYear,
    year,
    month,
    day,
    verbatimEventDate,
    #habitat,
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

  )


# Update dungfauna_event --------------------------------------------------

dungfauna_event <-
  dungfauna_event |>
  select(
    -c(SC,
       `Hamonthophagus depressus`,
       `Onthophagus ferox`,
       `Onthophagus vermiculatus`)
  )

# Check data --------------------------------------------------------------

## Check fields

# datasetName
distinct(dungfauna_occurrence, datasetName)

# basisOfRecord
distinct(dungfauna_occurrence, basisOfRecord)

dungfauna_occurrence |>
  group_by(datasetName) |>
  distinct(basisOfRecord)

# catalogNumber
dungfauna_occurrence |>
  group_by(datasetName) |>
  summarise(n = n_distinct(catalogNumber))

# scientificName
distinct(dungfauna_occurrence, scientificName) |> print(n = 23)

# recordedBy
distinct(dungfauna_occurrence, recordedBy)

# individualCount
dungfauna_occurrence |>
   summarise(min(individualCount, na.rm = TRUE),
            max(individualCount, na.rm = TRUE))

dungfauna_occurrence |>
  filter(is.na(individualCount)) |>
  dplyr::group_by(occurrenceStatus, datasetName) |>
  distinct(samplingProtocol)

# occurrenceStatus
distinct(dungfauna_occurrence, occurrenceStatus)

dungfauna_occurrence |>
  dplyr::group_by(occurrenceStatus) |>
  summarise(min(individualCount, na.rm = TRUE),
            max(individualCount, na.rm = TRUE))

# absence_qualifier
dungfauna_occurrence |>
  filter(occurrenceStatus == "present") |>
  distinct(absence_qualifier)

dungfauna_occurrence |>
  filter(occurrenceStatus == "absent") |>
  dplyr::group_by(datasetName, samplingProtocol) |>
  distinct(absence_qualifier) |>
  arrange(samplingProtocol) |>
  relocate(datasetName, .after = last_col()) |>
  print(n = 24)

# occurrenceRemarks
distinct(dungfauna_occurrence, occurrenceRemarks)

# eventID
dungfauna_occurrence |>
  dplyr::group_by(eventID) |>
  count() |>
  filter(n != 23)

# parentEventID
dungfauna_occurrence|>
  filter(is.na(parentEventID))

dungfauna_event |>
  group_by(locationID_site, parentEventID) |>
  mutate(date_parent = date(eventDate_setup)) |>
  summarise(n = n()) |>
  arrange(desc(n)) #|> View()

# fieldNumber
dungfauna_occurrence |>
  group_by(datasetName) |>
  summarise(n = n_distinct(fieldNumber))

dungfauna_occurrence |>
  group_by(datasetName) |>
  summarise(n = n_distinct(eventID))

# eventDate_setup
dungfauna_occurrence|>
  filter(is.na(eventDate_setup))

# eventDate_collect
dungfauna_occurrence|>
  filter(is.na(eventDate_collect))

# eventDate
dungfauna_occurrence|>
  filter(is.na(eventDate))

dungfauna_occurrence|>
  dplyr::group_by(eventID) |>
  summarise(n = n_distinct(eventDate)) |>
  filter(n != 1)

dungfauna_occurrence|>
  mutate(
    eventDate_interval =
      lubridate::interval(
        str_replace_all(eventDate, "\\+0800", ":00\\+0800"),
        tzone = "Australia/Perth")
  ) |>
  filter(is.na(eventDate_interval)) |>
  distinct(eventRemarks)
# Trapping dates unknown. Month inferred from sample code sequence.

# startDayOfYear
dungfauna_occurrence|>
  filter(is.na(startDayOfYear)) |>
  distinct(eventRemarks)

dungfauna_occurrence|>
  summarise(min(startDayOfYear, na.rm = TRUE),
            max(startDayOfYear, na.rm = TRUE))

# endDayOfYear
dungfauna_occurrence|>
  filter(is.na(endDayOfYear)) |>
  distinct(eventRemarks)

dungfauna_occurrence|>
  summarise(min(endDayOfYear, na.rm = TRUE),
            max(endDayOfYear, na.rm = TRUE))

# year
dungfauna_occurrence|>
  distinct(year) |>
  arrange(year)

# month
dungfauna_occurrence|>
  distinct(month) |>
  arrange(month)

# day
dungfauna_occurrence|>
  filter(is.na(day)) |>
  distinct(eventRemarks)

dungfauna_occurrence |>
  summarise(min(day, na.rm = TRUE),
            max(day, na.rm = TRUE))

# verbatimEventDate
dungfauna_occurrence|>
  filter(is.na(verbatimEventDate))

# samplingProtocol
distinct(dungfauna_occurrence, samplingProtocol) |> print(n = 18)

# sampleSizeValue
dungfauna_occurrence |>
  filter(is.na(sampleSizeValue)) |>
  distinct(samplingProtocol)

dungfauna_occurrence |>
  filter(str_detect(samplingProtocol, "DBEE") & is.na(sampleSizeValue)) |>
  distinct(eventRemarks)

dungfauna_occurrence |>
  filter(sampleSizeValue == 1) |>
  dplyr::group_by(datasetName) |>
  summarise(n_distinct(eventID))

dungfauna_occurrence |>
  filter(sampleSizeValue != 1 & !is.na(sampleSizeValue)) |>
  dplyr::group_by(datasetName) |>
  summarise(n_distinct(eventID))

dungfauna_occurrence |>
  summarise(
    min(sampleSizeValue, na.rm = TRUE),
    mean(sampleSizeValue, na.rm = TRUE),
    max(sampleSizeValue, na.rm = TRUE)
  )

# sampleSizeUnit
distinct(dungfauna_occurrence, sampleSizeUnit)

dungfauna_occurrence |>
  filter(!is.na(sampleSizeValue) & is.na(sampleSizeUnit))

# samplingEffort
dungfauna_occurrence |>
  filter(is.na(samplingEffort)) |>
  distinct(samplingProtocol)

dungfauna_occurrence |>
  dplyr::group_by(samplingProtocol) |>
  distinct(samplingEffort) |>
  arrange(samplingProtocol)  |>
  print(n = 30)

# eventRemarks
distinct(dungfauna_occurrence, eventRemarks)

dungfauna_occurrence |>
  filter(eventRemarks == "Partial sample. Counts and absences may not be reliable.") |>
  filter(occurrenceStatus == "absent") |>
  distinct(absence_qualifier)

dungfauna_occurrence |>
  filter(eventRemarks == "Trapping dates unknown. Month inferred from sample code sequence.") |>
  filter(!is.na(startDayOfYear) | !is.na(endDayOfYear) | !is.na(day))

# locationID_site
dungfauna_occurrence |>
  filter(is.na(locationID_site))

# locationID_trap
dungfauna_occurrence |>
  filter(str_detect(samplingProtocol, "trap") &
           is.na(locationID_trap)) |>
  distinct(georeferenceProtocol)

dungfauna_occurrence |>
  filter(
    georeferenceProtocol == "coordinates taken from the site centroid, coordinateUncertaintyInMeters includes additional uncertainty associated with unknown trap location within the site" &
      !is.na(locationID_trap))

# country
distinct(dungfauna_occurrence, country)

# countryCode
distinct(dungfauna_occurrence, countryCode)

# stateProvince
distinct(dungfauna_occurrence, stateProvince)

dungfauna_occurrence |>
  group_by(datasetName) |>
  distinct(stateProvince)

# locality
dungfauna_occurrence |>
  filter(is.na(locality))

# decimalLatitude
dungfauna_occurrence |>
  filter(is.na(decimalLatitude ))

dungfauna_occurrence |>
  summarise(
    min(decimalLatitude),
    max(decimalLatitude)
  )

# decimalLongitude
dungfauna_occurrence |>
  filter(is.na(decimalLongitude ))

dungfauna_occurrence |>
  summarise(
    min(decimalLongitude),
    max(decimalLongitude)
  )

# geodeticDatum
distinct(dungfauna_occurrence, geodeticDatum)

# coordinateUncertaintyInMeters
dungfauna_occurrence |>
  filter(is.na(coordinateUncertaintyInMeters))

dungfauna_occurrence |>
  summarise(
    min(coordinateUncertaintyInMeters),
    max(coordinateUncertaintyInMeters)
  )

# coordinatePrecision
dungfauna_occurrence |>
  filter(is.na(coordinatePrecision))

dungfauna_occurrence |>
  summarise(
    min(coordinatePrecision),
    max(coordinatePrecision)
  )

# verbatimLatitude
dungfauna_occurrence |>
  filter(is.na(verbatimLatitude))

# verbatimLongitude
dungfauna_occurrence |>
  filter(is.na(verbatimLongitude))

# verbatimCoordinateSystem
distinct(dungfauna_occurrence, verbatimCoordinateSystem)

# verbatimSRS
distinct(dungfauna_occurrence, verbatimSRS)

# georeferenceProtocol
dungfauna_occurrence |>
  group_by(datasetName) |>
  distinct(georeferenceProtocol)

# georeferenceSources
distinct(dungfauna_occurrence, georeferenceSources)

dungfauna_occurrence |>
  group_by(georeferenceProtocol) |>
  distinct(georeferenceSources)

# identifiedBy
dungfauna_occurrence |>
  group_by(occurrenceStatus) |>
  distinct(identifiedBy)

# dung_type
dungfauna_occurrence |>
  group_by(datasetName) |>
  distinct(dung_type)


## View species locations
map_df <-
  dungfauna_occurrence |>
  filter(occurrenceStatus == "present") |>
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
      addCircleMarkers(
        data = map_df[[df]],
        lng = ~decimalLongitude,
        lat = ~decimalLatitude,
        label = ~locationID_site,
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

dungfauna_occurrence_summary <-
  dungfauna_occurrence |>

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


plot_ly(dungfauna_occurrence_summary |> ungroup(),
        x = ~date, y = ~count, hoverinfo='text',
        text= ~str_c(scientificName, date, sep = " ")) |>
  add_lines(color=~scientificName)



# Save data ---------------------------------------------------------------

# Save data for combining with that from other projects
write.csv(dungfauna_event,
          "data-raw/dungfauna_event.csv",
          row.names = FALSE,
          na = "")

write.csv(dungfauna_occurrence,
          "data-raw/dungfauna_occurrence.csv",
          row.names = FALSE,
          na = "")


usethis::use_data(dungfauna_event, overwrite = TRUE, compress = "xz")
usethis::use_data(dungfauna_occurrence, overwrite = TRUE, compress = "xz")
