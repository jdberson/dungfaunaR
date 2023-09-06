# library(leaflet)
# library(RColorBrewer)
# library(scales)
# library(dplyr)
# library(lubridate)
# library(stringr)
# library(htmlwidgets)
# library(ggplot2)
# library(directlabels)
# library(leafem)
# library(shinyalert)
# library(shinyWidgets)

datadata <- alldatas
num_unique_years <-
  base::length(base::unique(base::as.character(lubridate::year(datadata$date))))
# colorpalette <- 'viridis'
colorpalette <- 'YlOrRd'
# aggregation_function <- sum
aggregation_function <- function(x) { base::mean(x, na.rm=TRUE) }

datadata <- datadata[base::order(datadata$abundance_total, decreasing=TRUE),]

leaflet_base_map <-
  leaflet::leaflet(
    options =
      leaflet::leafletOptions(zoomControl = TRUE,
                     zoomSnap = 0.05,
                     zoomDelta = 12,
                     maxZoom=12,
                     minZoom=3)
    )|>
  # addTiles() |>
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
  leaflet::setView(lng = 139.94, lat = -29.82, zoom = 5) |>
  leaflet::setMaxBounds(91, -65,  197, 5)

function(input, output, session) {

  ## Interactive Map ###########################################
  prediction_image_layers <-
    shiny::reactiveValues(prev_x=NULL, x=NULL, x_to_remove=NULL)

  # Create the map
  output$prediction_map <- leaflet::renderLeaflet({
     leaflet_base_map
  })
  output$map <- leaflet::renderLeaflet({
    leaflet_base_map
  })

  get_selected_prediction <- shiny::reactive({
    monthrange <- which(month.name.custom %in% input$prediction_monthrange)
    species <- base::which(r_stack_names %in% input$prediction_species)
    prediction_image_layers$x <-
      base::paste0(base::as.character(monthrange), '_', species)
    r_stack[species,,,monthrange]
  })

  # reactive expressions that returns the set of data
  filter_by_daterange <- function(d) {
    shiny::validate(
      shiny::need(input$daterange, 'Plots need at least one year selected')
      )
    monthrange <- base::which(month.name %in% input$monthrange)
    months <- lubridate::month(d$date)
    return (
      d |>
        base::subset(
          lubridate::year(d$date) %in%
            input$daterange & months >= monthrange[1] & months <= monthrange[2])
    )
  }

  filter_by_data_source <- function(d) {
    shiny::validate(
      shiny::need(input$datasetName, 'Plots need a data source selected')
      )

    base::print(input$datasetName)

    if (input$datasetName == 'All')
      return(d)


    return(d |>
             subset(datasetName == input$datasetName))
  }

  rounded_map_bounds <- function() {
    # useful for caching purposes where slightly different views of the same
    # area will not need to have a new ggplot made
    bounds <- input$map_bounds
    if (base::is.null(bounds))
      return(NULL)

    accuracy = 1

    bounds$north = base::round(bounds$north, accuracy)
    bounds$east = base::round(bounds$east, accuracy)
    bounds$south = base::round(bounds$south, accuracy)
    bounds$west = base::round(bounds$west, accuracy)
    return(bounds)
  }

  get_within_bounds_and_daterange <- function() {
    bounds <- rounded_map_bounds()
    latRng <- base::range(bounds$north, bounds$south)
    lngRng <- base::range(bounds$east, bounds$west)

    return (
      datadata |>
        filter_by_daterange() |>
        filter_by_data_source() |>
        base::subset(
          latitude >= latRng[1] & latitude <= latRng[2] &
            longitude >= lngRng[1] & longitude <= lngRng[2])
    )
  }
  # does the same as above but also summarises the data
  # so there is only one point on the map per site
  dataInBoundsSummarised <- shiny::reactive({
    if (base::is.null(input$map_bounds))
      return(datadata[FALSE,])

    get_within_bounds_and_daterange() |>
      dplyr::group_by(
        state,
        site
      ) |>
      dplyr::summarise(
        dplyr::across(.cols = c(latitude, longitude), ~base::mean(.x)),
        dplyr::across(.cols = -c(latitude, longitude, date, trap),
                      ~aggregation_function(.x))
        ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        datacode = dplyr::row_number()
      )
  })
  dataInBounds <- shiny::reactive({
    if (base::is.null(input$map_bounds))
      return(datadata[FALSE,])

    get_within_bounds_and_daterange()
  })
  dataSummarised <- shiny::reactive({
    filter_by_daterange(datadata) |>
      filter_by_data_source() |>
      dplyr::group_by(
        state,
        site
      )|>
      dplyr::summarise(
        dplyr::across(.cols = c(latitude, longitude), ~mean(.x)),
        dplyr::across(.cols = -c(latitude, longitude, date, trap),
                      ~aggregation_function(.x)
                      ),
                n = dplyr::n(),
                n_visits = dplyr::n_distinct(date),
                n_traps = dplyr::n_distinct(trap),
                min_date = base::min(date),
                max_date = base::max(date)) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        datacode = dplyr::row_number()
      )

  })

  measure_to_readable_text <- function(measure, species, markdown=FALSE) {
    if (species != 'beetles') {
      if (species != 'All' && species != 'total') {
        if (markdown) {
          species = paste0('*', species, '*')
        } else {
          species = paste0('<em>', species, '</em>')
        }
      } else {
        species <- 'all beetles'
      }
    }

    return_character <- '<br>'

    if (measure == 'abundance')
      measure = 'number'

    legend_string <- paste('Average', measure, 'of', return_character, species,
                           return_character, 'trapped')

    if (measure == 'biomass') {
      legend_string <-
        base::paste(legend_string, return_character, '(g dry mass)')
    }
    return(legend_string)
  }

  # This observer is responsible for plotting predictions
  shiny::observe({
    if (!include_predictions)
      return(NULL)
    if (input$nav != 'Prediction')
      return(NULL)

    base::print('running plot predictions')
    base::print(input$nav)

    shiny::isolate({
      preds <- get_selected_prediction()
    })

    pred_range <-
      predictions_range[[base::which(r_stack_names %in%
                                       input$prediction_species)]]
    # pred_range <- predictions_range

    # bodged job for nicer plotting with bad model with outlier
    pred_range[2] <- base::min(50, pred_range[2])
    # check this Jake 0s to NA
    # terra way but need to use stars way
    # preds[][preds[] == 0] <- NA

    #pal <- colorBin('YlOrRd', pred_range, bins=8, pretty = TRUE, na.color='transparent')
    pal <-
      leaflet::colorNumeric(colorpalette,
                            pred_range,
                            7,
                            na.color = grDevices::rgb(0,0,0,0))
    # pal <- colorBin('YlOrRd', bins = c(0, 1,3,5,10,30,80,160), pretty = TRUE, na.color='transparent')

    proxy <- leaflet::leafletProxy("prediction_map")

    if (
      !base::is.null(prediction_image_layers$x) &&
      (base::is.null(prediction_image_layers$prev_x) ||
       prediction_image_layers$prev_x != prediction_image_layers$x)) {
      proxy |>
        leafem::addStarsImage(
          preds,
          colors = pal,
          opacity = 0.8,
          layerId = prediction_image_layers$x
          ) |>
        leaflet::addLegend(
          'bottomleft',
          pal = pal,
          values=pred_range,
          title = 'Predicted biomass',
          layerId="prediction_colorLegend"
        )
    }

    if (!base::is.null(prediction_image_layers$x_to_remove)
        && prediction_image_layers$x_to_remove != prediction_image_layers$x) {
      proxy |> removeImage(prediction_image_layers$x_to_remove)
      prediction_image_layers$x_to_remove <- NULL
    }

    if (!base::is.null(prediction_image_layers$prev_x)) {
      if (prediction_image_layers$prev_x != prediction_image_layers$x) {
        prediction_image_layers$x_to_remove <- prediction_image_layers$prev_x
      }
    }

    prediction_image_layers$prev_x <- prediction_image_layers$x

    return(proxy)
  }) |>
    shiny::bindEvent(
      input$prediction_monthrange,
      input$prediction_color,
      input$prediction_species,
      input$nav)

  show_scatter_plot <- function(measure = 'abundance', month_freq = 1) {
    base::print(base::paste0('Redrawing ', measure, ' plot'))
    base::print(input$nav)

    colorBy <- base::paste0(measure, '_', input$species)
    dat <- dataInBounds()

    if (base::nrow(dat) == 0)
      return(NULL)

    cols = 'black'

    # change date to one year, so plot only displays one years worth of data
    dat$year <- base::as.character(lubridate::year(dat$date))
    lubridate::year(dat$date) <- 1900
    dat$date <- base::as.Date(dat$date)

    date_brks <- base::seq(lubridate::ymd('1900-01-01'),
                           lubridate::ymd('1900-12-01'),
                           by = base::paste(month_freq, 'months'))

    max_val <- base::max(dat[, colorBy])
    if (max_val > 500) {
      y_brks=c(0, 1, 3, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000, 1000000)
    } else if (max_val < 100) {
      y_brks=c(0, 1, 3, 5, 10, 30, 50, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000, 1000000)
    } else {
      y_brks=c(0, 1, 3, 10, 30, 100, 300, 500, 1000, 3000, 10000, 30000, 100000, 300000, 1000000)
    }

    expand_amount = c(0.1, 0.35)
    if (month_freq == 2) {
      # small screen
      expand_amount = c(0.2, 1.2)
    }
    legend_text <- measure_to_readable_text(measure, input$species, markdown=TRUE)

    plt <-
      ggplot2::ggplot(
        dat, ggplot2::aes(x=date, y=.data[[colorBy]], group=year, color=year)
        ) +
      ggplot2::scale_colour_discrete(guide = 'none') +
      ggplot2::scale_x_date(
        date_labels = '%b', breaks=date_brks, expand=expand_amount
        ) +
      ggplot2::scale_y_continuous(trans='log1p', breaks=y_brks) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        legend.position = "none",
        axis.text = ggplot2::element_text(size = 16),
        axis.title = ggplot2::element_text(size = 18),
        axis.title.y = ggtext::element_markdown(size=18)
      ) +
      ggplot2::labs(x = 'Date', y=legend_text)

    index <- base::rowSums(base::table(dat$year,
                                       base::months(dat$date)) != 0) > 2
    years_with_enough_d <- base::names(index[index])
    plt <-
      plt +
      ggplot2::geom_smooth(
        data=dat[dat$year %in% years_with_enough_d,],
        method = lm,
        formula = y ~ splines::bs(x, 3),
        se = FALSE,
        ggplot2::aes(fill=year)) +
       directlabels::geom_dl(
         ggplot2::aes(label=year), method=list('last.points', cex=1.5)
         )
    plt <-
      plt +
      ggplot2::geom_point(
        data=dat[!(dat$year %in% years_with_enough_d),],
        alpha=0.4) +
      directlabels::geom_dl(
        ggplot2::aes(label=year), method=list('last.points', cex=1.5)
        )

    plt <-
      plt +
      ggplot2::geom_point(alpha=0.1) +
      directlabels::geom_dl(
        ggplot2::aes(label=year), method=list('last.points', cex=1.5)
        )

    plt
  }
  output$scatterSelected <- shiny::renderPlot({
    show_scatter_plot('abundance')
  }) |>
    shiny::bindCache(
      input$nav,
      input$monthrange,
      'abundance',
      input$species,
      input$daterange,
      rounded_map_bounds())
  output$scatterSelected2Months <- shiny::renderPlot({
    show_scatter_plot('abundance', 2)
  }) |>
    shiny::bindCache(
      input$nav,
      input$monthrange,
      'abundance',
      input$species,
      input$daterange,
      rounded_map_bounds())

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  shiny::observe({
    if (input$nav != 'Collection')
      return(NULL)

    base::print('running plot circle markers on data collection map')
    base::print(input$nav)

    colorBy <- base::paste0('abundance', '_', input$species)

    d <- dataSummarised()

    # d <- d |> arrange(!!sym(colorBy)) |>
    #   filter(!!sym(colorBy)!=0)
    d[d[, colorBy] == 0, colorBy] <- NA

    if (base::nrow(d) == 0) {
      leaflet::leafletProxy("map") |>
        leaflet::clearMarkers()
      return(NULL)
    }

    colorData <- d[[colorBy]]
    # pal <- colorBin(colorpalette, colorData, 7, pretty = TRUE, na.color = 'transparent')

    if (base::all(base::is.na(colorData))) {
      pal <-
        leaflet::colorFactor(
          colorpalette,
          colorData,
          7,
          na.color = grDevices::rgb(0,0,0,0.08)
          )
    } else {
      pal <-
        leaflet::colorNumeric(
          colorpalette,
          colorData,
          7,
          na.color = grDevices::rgb(0,0,0,0.08))
    }

    # bins <- colorQuantile(colorpalette, colorData,  na.rm=TRUE, names=FALSE)

    # sizeBy <- paste0('abundance', '_', input$species)
    # radius <- dataSummarised()[[sizeBy]] / max(dataSummarised()[[sizeBy]]) * 60000 + 500
    radius <- 4

    # format
    split_string <- stringr::str_split(colorBy, "_")
    measure <- split_string[[1]][1]
    species <- base::names(species_choices)[species_choices==split_string[[1]][2]]

    legend_string <- measure_to_readable_text(measure, species)

    outlines <- base::ifelse(base::is.na(d[,colorBy]), '#bdbdbd', 'black')
    opacity <- base::ifelse(base::is.na(d[,colorBy]), 0, 1)

    leaflet::leafletProxy("map", data = d) |>
      leaflet::clearMarkers() |>
      leaflet::addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = radius,
        layerId = ~datacode,
        stroke = T,
        color = outlines,
        weight = 1,
        fillOpacity = opacity,
        fillColor = pal(colorData)) |>
      leaflet::addLegend(
        "bottomleft",
        pal = pal,
        opacity = opacity,
        values = colorData,
        title = legend_string,
        layerId = "colorLegend",
        na.label = '0')
        # labFormat = function(type, cuts, p) { # custom code to display quantile range values instead of percentile values
        #   n = length(cuts)
        #   p = paste0(round(p * 100), '%')
        #   cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))
        #   # mouse over the legend labels to see the percentile ranges
        #   paste0(
        #     '<span title="', p[-n], " - ", p[-1], '">', cuts,
        #     '</span>'
        #   )
        # }
      # )
  }) |>
    shiny::bindEvent(
      input$nav,
      input$monthrange,
      'abundance',
      input$species,
      input$daterange,
      input$datasetName
      )

  # Show a popup at the given location
  showdatacodePopup <- function(datacode, lat, lng, measure='abundance') {
    dat <- dataSummarised()
    if (base::is.character(datacode)) {
      # is the site and state concatenated together
      split_string <- stringr::str_split(datacode, '_')
      state <- split_string[[1]][2]
      site <- split_string[[1]][1]
      selecteddata <- dat[dat$state==state & dat$site==site,]
    } else {
      # is the datacode id
      selecteddata <- dat[dat$datacode == datacode,]
    }

    # get the info I want to display
    foo <- selecteddata[ , base::grepl( measure , base::names( selecteddata ))] |>
      dplyr::select(-tidyselect::contains('total'))
    species <- base::gsub(base::paste0(measure, "_"), "", base::colnames(foo))
    base::colnames(foo) <- species
    if (base::length(species[foo > 0]) > 0) {
      species_observed <- base::paste0("<i>", species[foo > 0], "</i>:")
    } else {
      species_observed <- ''
    }


    # bio_foo <- selecteddata[ , grepl( "biomass" , names( selecteddata ))]
    # bio_foo <- subset(bio_foo, select=-biomass_total)
    num_traps_text <- '1 trap'
    if (selecteddata$n_traps > 1) {
      num_traps_text <- base::paste(selecteddata$n_traps, 'traps')
    }

    num_visits_text <- '1 time'
    if (selecteddata$n_visits > 1) {
      num_visits_text <- base::paste(selecteddata$n_visits, "times")
    }

    if (selecteddata$min_date == selecteddata$max_date) {
      dates_text <-
        base::paste0(' on the ',
                     base::format(selecteddata$min_date, format='%d %B %Y'))
    } else {
      dates_text <-
        base::paste0(' between ',
                     base::format(selecteddata$min_date, format='%d %B %Y'),
                     ' and ',
                     base::format(selecteddata$max_date, format='%d %B %Y'))
    }

    content <- base::paste0(
      tags$h3(base::paste0(base::gsub("([A-Z])", " \\1", selecteddata$site), ", ", stringr::str_to_title(selecteddata$state))),
      base::paste0('<h4>', measure_to_readable_text(measure, 'beetles'), '</h4>'),
      "All beetles: ", base::round(selecteddata[base::paste0(measure, '_total')], 2), '<br>',
      base::paste(
        base::paste0(
        species_observed, base::round(foo[foo > 0], 2)
        ),
        collapse="<br>"
      ),
      tags$h4("Site information"),
      "Collected from ", num_traps_text,
      ' deployed ', num_visits_text,
      "<br> ", dates_text
    )
    leaflet::leafletProxy("map") |>
      leaflet::addPopups(lng, lat, content, layerId = datacode)
  }

  # When map is clicked, show a popup with city info
  shiny::observe({
    if (input$nav != 'Collection')
      return()

    base::print('running clear popups')

    leaflet::leafletProxy("map") |>
      leaflet::clearPopups()
    event <- input$map_marker_click
    if (base::is.null(event))
      return()

    shiny::isolate({
      showdatacodePopup(event$id, event$lat, event$lng, 'abundance')
    })
  }) |> shiny::bindEvent(input$map_marker_click)

  # When the prediction map is clicked, show a popup with raster info
  shiny::observe({
    click <- input$prediction_map_click

    shiny::isolate({
      r <- get_selected_prediction()
    })

    pos <- sf::st_point(base::cbind(click$lng, click$lat)) |>
      sf::st_sfc() |>
      sf::st_set_crs(4326) |>
      sf::st_transform(sf::st_crs(r))

    value <- stringr::st_extract(get_selected_prediction(), pos) |>
      sf::st_drop_geometry()

    proxy <- leaflet::leafletProxy("prediction_map")

    if (base::is.na(value)) {
      proxy |>
        leaflet::clearPopups()
      return()
    }

    text <- base::paste0(
      "Estimate: ",
      base::round(value, 2), '<br>',
      "Latitude: ", base::round(click$lat, 3), '<br>',
      "Longitude: ", base::round(click$lng, 3), '<br>'
    )

    proxy |>
      leaflet::clearPopups() |>
      leaflet::addPopups(click$lng, click$lat, text)

  }) |>
    shiny::bindEvent(input$prediction_map_click)

  isShown <- shiny::reactiveValues(shown=TRUE)

  showHidePredButton <- function() {
    if (isShown$shown) {
      shinyjs::hide(id = 'sidebar')
      shinyjs::show(id='hideSidebar')
      shinyjs::hide(id='showSidebar')
      isShown$shown = FALSE
    } else {
      shinyjs::show(id = 'sidebar')
      shinyjs::hide(id='hideSidebar')
      shinyjs::show(id='showSidebar')
      isShown$shown = TRUE
    }
    base::print(isShown$shown)
  }

  shiny::observeEvent(input$showSidebar, {
    showHidePredButton()
  })

  shiny::observeEvent(input$hideSidebar, {
    showHidePredButton()
  })

  isShownMain <- shiny::reactiveValues(shown=TRUE)

  showHideButton <- function() {
    if (isShownMain$shown) {
      shinyjs::hide(id = 'mainPanel')
      shinyjs::show(id='hideMainPanel')
      shinyjs::hide(id='showMainPanel')
      isShownMain$shown = FALSE
    } else {
      shinyjs::show(id = 'mainPanel')
      shinyjs::hide(id='hideMainPanel')
      shinyjs::show(id='showMainPanel')
      isShownMain$shown = TRUE
    }
    base::print(isShownMain$shown)
  }

  shiny::observeEvent(input$showMainPanel, {
    showHideButton()
  })
  shiny::observeEvent(input$hideMainPanel, {
    showHideButton()
  })

  # notification that points are randomly moved for privacy
  # shown_randomly_moved_notification <- reactiveValues(shown=FALSE)
  # observeEvent(input$map_zoom, {
  #   if (shown_randomly_moved_notification$shown)
  #     return()

    # if (input$map_zoom > 10) {
    #   show_toast(
    #     title = '',
    #     text = "Note, data points are randomly moved to preserve privacy.",
    #     position = 'top',
    #     width='40%',
    #     timer=5000
    #   )
    #   shown_randomly_moved_notification$shown = TRUE
    # }
  # })


  ## Data Explorer ###########################################

  shiny::observe({
    sites <- if (base::is.null(input$states)) base::character(0) else {
      dplyr::filter(dungfauna_occurrence, stateProvince %in% input$states) |>
        {`$`('locationID_site')}() |>
        base::unique() |>
        base::sort()
    }
    stillSelected <- shiny::isolate(input$sites[input$sites %in% sites])
    shiny::updateSelectizeInput(session, "sites", choices = sites,
      selected = stillSelected, server = TRUE)
  })

  shiny::observe({
    if (base::is.null(input$goto))
      return()
    shiny::isolate({
      map <- leaflet::leafletProxy("map")
      map |> leaflet::clearPopups()
      dist <- 0.5
      data <- input$goto$data
      lat <- input$goto$lat
      lng <- input$goto$lng
      showdatacodePopup(data, lat, lng)
      map |> leaflet::fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$datatable <- DT::renderDataTable({
    df <- dungfauna_occurrence |>
      dplyr::filter(
        individualCount >= input$minTotalAbundance,
        individualCount <= input$maxTotalAbundance,
        base::is.null(input$states) | stateProvince %in% input$states,
        base::is.null(input$sites) | locationID_site %in% input$sites,
        base::is.null(input$speciesTable) | scientificName %in% input$speciesTable
      ) |>
      dplyr::mutate(
        'Click to view on map' =
          base::paste('<a class="go-map" href="" data-lat="',
                      decimalLatitude,
                      '" data-long="',
                      decimalLongitude,
                      '" data-data="',
                      base::paste0(locationID_site, '_', stateProvince),
                      '"><i class="fa fa-crosshairs"></i></a>',
                      sep="")
        )
    action <- DT::dataTableAjax(session, df, outputId = "datatable")

    DT::datatable(df,
                  options = list(ajax = list(url = action),
                                 columnDefs = list(list(visible=FALSE,
                                                        targets=c('datacode')))),
                  escape = FALSE)
  })

  ## POPUPs
  shinyalert::shinyalert(
    title = "Welcome!",
    text = 'Use the dashboard to explore the data that is included in the submitted manuscript by Berson et al.',
    size = "m",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    imageUrl = "",
    animation = TRUE
  )

  # should go at end
  shinyjs::hide('loading_screen')

}
