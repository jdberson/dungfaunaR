# Figures and other information for checks by data partners ---------------

library(tidyverse)
library(leaflet)
library(plotly)


dungfauna_occurrence <-
  read_csv("data-raw/dungfauna_occurrence.csv")

dungfauna_event <-
  read_csv("data-raw/dungfauna_event.csv")

#dataset <- "Queensland Dung Beetle Project"
#dataset <- "South-western Australia"
dataset <- "Dung Beetle Ecosystem Engineers"

occurrence_to_explore <-
  dungfauna_occurrence |>
  filter(str_detect(datasetName, dataset))

event_to_explore <-
  dungfauna_event |>
  filter(str_detect(datasetName, dataset))

# Visualise trap locations ------------------------------------------------

factpal <- colorFactor(palette = "viridis",
                       #unique(event_to_explore$datasetName) # for QLD data
                       #unique(event_to_explore$basisOfRecord) # for DAFWA data
                       unique(event_to_explore$recordedBy) # for DBEE data
                       )

trap_locations <-
  leaflet(data = event_to_explore |>
            mutate(locationID_trap = if_else(is.na(locationID_trap), locationID_site, locationID_trap))) |>
  addProviderTiles("Esri.WorldImagery") |>
  addCircleMarkers(lng = ~decimalLongitude,
                   lat = ~decimalLatitude,
                   label = ~str_c(recordedBy, locationID_trap, date(eventDate_setup), sep = " "),
                   #color = ~factpal(datasetName) # for QLD data
                   #color = ~factpal(basisOfRecord) # for DAFWA data
                   color = ~factpal(recordedBy) # for DBEE data
                   )

trap_locations

# save the widget in an html file
htmlwidgets::saveWidget(trap_locations,
                        file= str_c(dataset, "_trap locations.html"))


# Visualise species distributions -----------------------------------------


# View species locations
map_df <-
  occurrence_to_explore |>
  group_by(locationID_site, scientificName) |>
  mutate(individualCount = case_when(
    is.na(individualCount) & occurrenceStatus == "present" ~ 1,
    is.na(individualCount) & occurrenceStatus == "absent" ~ 0,
    TRUE ~ individualCount
  )) |>
  summarise(decimalLongitude = decimalLongitude[1],
            decimalLatitude = decimalLatitude[1],
            total = sum(individualCount, na.rm = TRUE)) |>
  mutate(occurrenceStatus = if_else(total == 0, "absent", "present")) |>
  filter(occurrenceStatus == "present") |>
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
      addCircleMarkers(
        data = map_df[[df]],
        lng = ~decimalLongitude,
        lat = ~decimalLatitude,
        label = ~str_c(locationID_site, "Total:", total, scientificName, sep = " "),
        group = df,
        color =  ~factpal(occurrenceStatus),
        opacity = 1)
  })

species_location <-
  l_map |>
  addLayersControl(
    baseGroups = names(map_df),
    options = layersControlOptions(collapsed = FALSE)
  )

species_location


# save the widget in an html file
htmlwidgets::saveWidget(species_location,
                        file= str_c(dataset, "_species locations.html"))

# Visualise species seasonality -------------------------------------------


dungfauna_occurrence_summary <-
  occurrence_to_explore |>

  #filter(str_detect(datasetName, "2003")) |>
  filter(str_detect(basisOfRecord, "Preserved")) |>

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

season_plot <-
  plot_ly(dungfauna_occurrence_summary |> ungroup(),
          x = ~date, y = ~count, hoverinfo='text',
          text= ~str_c(scientificName, date, sep = " ")) |>
  add_lines(color=~scientificName)

season_plot

# save the widget in an html file
htmlwidgets::saveWidget(season_plot,
                        file= str_c(dataset, "_species seasonality.html"))


## For SW WA show species seasonality at each site

# species <- "Onthophagus taurus"
# species <- "Onthophagus binodis"
# species <- "Euoniticellus fulvus"
# species <- "Euoniticellus pallipes"
# species <- "Euoniticellus intermedius"
# species <- "Onitis alexis"
# species <- "Onitis aygulus"
# species <- "Bubas bison"
# species <- "Copris hispanus"
#
# species_occurrence_summary <-
#   occurrence_to_explore |>
#
#   filter(str_detect(basisOfRecord, "Preserved")) |>
#   filter(scientificName == species) |>
#
#   # Some gymnastics to calculate monthly means of each species after
#   # removing sites where species not found
#   group_by(locationID_site, scientificName, month, year) |>
#   mutate(total = sum(individualCount)) |>
#   group_by(locationID_site, scientificName) |>
#   mutate(max = max(total)) |>
#   filter(max != 0) |>
#   group_by(locationID_site, scientificName, month, year) |>
#   summarise(count = mean(individualCount, na.rm = TRUE)) |>
#   mutate(date = dmy(str_c(14, month, year, sep = "/")), .after = year)
#
# species_season_plot <-
#   plot_ly(species_occurrence_summary |> ungroup(),
#           x = ~date, y = ~count, hoverinfo='text',
#           text= ~str_c(locationID_site, scientificName, date, sep = " ")) |>
#   add_lines(color=~locationID_site)
#
# species_season_plot
#
# # save the widget in an html file
# htmlwidgets::saveWidget(species_season_plot,
#                         file= str_c(dataset, species, "site seasonality.html", sep = "_"))




# Export the data ---------------------------------------------------------

write.csv(occurrence_to_explore |>
            filter(stateProvince != "Western Australia") |>
            group_by(locationID_site) |>
            slice_head(n = 23),
          file = str_c(dataset, "_data.csv"),
          row.names = FALSE)
