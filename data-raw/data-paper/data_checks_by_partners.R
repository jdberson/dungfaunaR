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
            group_by(locationID_site) |>
            slice_head(n = 23),
          file = str_c(dataset, "_data.csv"),
          row.names = FALSE)

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
# [1] plotly_4.10.2   leaflet_2.2.0   lubridate_1.9.2 forcats_1.0.0
# [5] stringr_1.5.0   dplyr_1.1.2     purrr_1.0.2     readr_2.1.4
# [9] tidyr_1.3.0     tibble_3.2.1    ggplot2_3.4.3   tidyverse_2.0.0
#
# loaded via a namespace (and not attached):
# [1] gtable_0.3.4        htmlwidgets_1.6.2   devtools_2.4.5
# [4] remotes_2.4.2.1     processx_3.8.2      lattice_0.21-8
# [7] tzdb_0.4.0          callr_3.7.3         crosstalk_1.2.0
# [10] vctrs_0.6.3         tools_4.3.1         ps_1.7.5
# [13] generics_0.1.3      proxy_0.4-27        fansi_1.0.4
# [16] pkgconfig_2.0.3     KernSmooth_2.23-21  data.table_1.14.8
# [19] lifecycle_1.0.3     compiler_4.3.1      munsell_0.5.0
# [22] httpuv_1.6.11       htmltools_0.5.6     usethis_2.2.2
# [25] class_7.3-22        lazyeval_0.2.2      later_1.3.1
# [28] pillar_1.9.0        crayon_1.5.2        urlchecker_1.0.1
# [31] ellipsis_0.3.2      classInt_0.4-9      cachem_1.0.8
# [34] sessioninfo_1.2.2   mime_0.12           tidyselect_1.2.0
# [37] digest_0.6.33       stringi_1.7.12      sf_1.0-14
# [40] fastmap_1.1.1       rnaturalearth_0.3.4 grid_4.3.1
# [43] colorspace_2.1-0    cli_3.6.1           magrittr_2.0.3
# [46] pkgbuild_1.4.2      utf8_1.2.3          e1071_1.7-13
# [49] withr_2.5.0         scales_1.2.1        prettyunits_1.1.1
# [52] promises_1.2.1      sp_2.0-0            timechange_0.2.0
# [55] httr_1.4.7          hms_1.1.3           memoise_2.0.1
# [58] shiny_1.7.5         viridisLite_0.4.2   miniUI_0.1.1.1
# [61] profvis_0.3.8       rlang_1.1.1         Rcpp_1.0.11
# [64] xtable_1.8-4        glue_1.6.2          DBI_1.1.3
# [67] pkgload_1.3.2.1     rstudioapi_0.15.0   jsonlite_1.8.7
# [70] R6_2.5.1            fs_1.6.3            units_0.8-3
