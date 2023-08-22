library(leaflet)
library(RColorBrewer)
library(scales)
library(dplyr)
library(lubridate)
library(stringr)
library(htmlwidgets)
library(ggplot2)
library(directlabels)
library(leafem)
library(shinyalert)
library(shinyWidgets)

datadata <- alldatas
num_unique_years <- length(unique(as.character(year(datadata$date))))
# colorpalette <- 'viridis'
colorpalette <- 'YlOrRd'
# aggregation_function <- sum
aggregation_function <- function(x) { mean(x, na.rm=TRUE) }

datadata <- datadata[order(datadata$abundance_total, decreasing=TRUE),]

leaflet_base_map <- leaflet(options=leafletOptions(zoomControl = TRUE, zoomSnap = 0.05, zoomDelta = 12, maxZoom=12, minZoom=3)) %>%
  # addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 139.94, lat = -29.82, zoom = 5) %>%
  setMaxBounds(91, -65,  197, 5)

function(input, output, session) {

  ## Interactive Map ###########################################
  prediction_image_layers <- reactiveValues(prev_x=NULL, x=NULL, x_to_remove=NULL)

  # Create the map
  output$prediction_map <- renderLeaflet({
     leaflet_base_map
  })
  output$map <- renderLeaflet({
    leaflet_base_map
  })

  get_selected_prediction <- reactive({
    monthrange <- which(month.name.custom %in% input$prediction_monthrange)
    species <- which(r_stack_names %in% input$prediction_species)
    prediction_image_layers$x <- paste0(as.character(monthrange), '_', species)
    r_stack[species,,,monthrange]
  })

  # reactive expressions that returns the set of data
  filter_by_daterange <- function(d) {
    validate(need(input$daterange, 'Plots need at least one year selected'))
    monthrange <- which(month.name %in% input$monthrange)
    months <- month(d$date)
    return (
      d %>% subset(year(d$date) %in% input$daterange & months >= monthrange[1] & months <= monthrange[2])
    )
  }

  filter_by_data_source <- function(d) {
    validate(need(input$datasetName, 'Plots need a data source selected'))

    print(input$datasetName)

    if (input$datasetName == 'All')
      return(d)


    return(d %>% subset(datasetName == input$datasetName))
  }

  rounded_map_bounds <- function() {
    # useful for caching purposes where slightly different views of the same
    # area will not need to have a new ggplot made
    bounds <- input$map_bounds
    if (is.null(bounds))
      return(NULL)

    accuracy = 1

    bounds$north = round(bounds$north, accuracy)
    bounds$east = round(bounds$east, accuracy)
    bounds$south = round(bounds$south, accuracy)
    bounds$west = round(bounds$west, accuracy)
    return(bounds)
  }

  get_within_bounds_and_daterange <- function() {
    bounds <- rounded_map_bounds()
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    return (
      datadata %>%
        filter_by_daterange() %>%
        filter_by_data_source() %>%
        subset(
          latitude >= latRng[1] & latitude <= latRng[2] &
            longitude >= lngRng[1] & longitude <= lngRng[2])
    )
  }
  # does the same as above but also summarises the data
  # so there is only one point on the map per site
  dataInBoundsSummarised <- reactive({
    if (is.null(input$map_bounds))
      return(datadata[FALSE,])

    get_within_bounds_and_daterange() %>%
      group_by(
        state,
        site
      ) %>%
      summarise(across(.cols = c(latitude, longitude), ~mean(.x)),
                across(.cols = -c(latitude, longitude, date, trap), ~aggregation_function(.x))) %>%
      ungroup() %>%
      mutate(
        datacode=row_number()
      )
  })
  dataInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(datadata[FALSE,])

    get_within_bounds_and_daterange()
  })
  dataSummarised <- reactive({
    filter_by_daterange(datadata) %>%
      filter_by_data_source() %>%
      group_by(
        state,
        site
      ) %>%
      summarise(across(.cols = c(latitude, longitude), ~mean(.x)),
                across(.cols = -c(latitude, longitude, date, trap), ~aggregation_function(.x)),
                n=n(),
                n_visits=n_distinct(date),
                n_traps=n_distinct(trap),
                min_date=min(date),
                max_date=max(date)) %>%
      ungroup() %>%
      mutate(
        datacode=row_number()
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
      legend_string <- paste(legend_string, return_character, '(g dry mass)')
    }
    return(legend_string)
  }

  # This observer is responsible for plotting predictions
  observe({
    if (!include_predictions)
      return(NULL)
    if (input$nav != 'Prediction')
      return(NULL)

    print('running plot predictions')
    print(input$nav)

    isolate({
      preds <- get_selected_prediction()
    })

    pred_range <- predictions_range[[which(r_stack_names %in% input$prediction_species)]]
    # pred_range <- predictions_range

    # bodged job for nicer plotting with bad model with outlier
    pred_range[2] <- min(50, pred_range[2])
    # check this Jake 0s to NA
    # terra way but need to use stars way
    # preds[][preds[] == 0] <- NA

    #pal <- colorBin('YlOrRd', pred_range, bins=8, pretty = TRUE, na.color='transparent')
    pal <- colorNumeric(colorpalette, pred_range, 7, na.color = rgb(0,0,0,0))
    # pal <- colorBin('YlOrRd', bins = c(0, 1,3,5,10,30,80,160), pretty = TRUE, na.color='transparent')

    proxy <- leafletProxy("prediction_map")

    if (!is.null(prediction_image_layers$x) && (is.null(prediction_image_layers$prev_x) || prediction_image_layers$prev_x != prediction_image_layers$x)) {
      proxy %>%
        addStarsImage(preds, colors = pal, opacity = 0.8, layerId = prediction_image_layers$x) %>%
        addLegend(
          'bottomleft',
          pal = pal,
          values=pred_range,
          title = 'Predicted biomass',
          layerId="prediction_colorLegend"
        )
    }

    if (!is.null(prediction_image_layers$x_to_remove)
        && prediction_image_layers$x_to_remove != prediction_image_layers$x) {
      proxy %>% removeImage(prediction_image_layers$x_to_remove)
      prediction_image_layers$x_to_remove <- NULL
    }

    if (!is.null(prediction_image_layers$prev_x)) {
      if (prediction_image_layers$prev_x != prediction_image_layers$x) {
        prediction_image_layers$x_to_remove <- prediction_image_layers$prev_x
      }
    }

    prediction_image_layers$prev_x <- prediction_image_layers$x

    return(proxy)
  }) %>% bindEvent(input$prediction_monthrange, input$prediction_color, input$prediction_species, input$nav)

  show_scatter_plot <- function(measure='abundance', month_freq = 1) {
    print(paste0('Redrawing ', measure, ' plot'))
    print(input$nav)

    colorBy <- paste0(measure, '_', input$species)
    dat <- dataInBounds()

    if (nrow(dat) == 0)
      return(NULL)

    cols = 'black'

    # change date to one year, so plot only displays one years worth of data
    dat$year <- as.character(year(dat$date))
    year(dat$date) <- 1900
    dat$date <- as.Date(dat$date)

    date_brks <- seq(ymd('1900-01-01'),ymd('1900-12-01'), by = paste(month_freq, 'months'))

    max_val <- max(dat[, colorBy])
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

    plt <- ggplot(dat, aes(x=date, y=.data[[colorBy]], group=year, color=year)) +
      scale_colour_discrete(guide = 'none') +
      scale_x_date(date_labels = '%b', breaks=date_brks, expand=expand_amount) +
      scale_y_continuous(trans='log1p', breaks=y_brks) +
      theme_classic() + theme(
        legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.title.y = ggtext::element_markdown(size=18)
      ) +
      labs(x = 'Date', y=legend_text)

    index <- rowSums(table(dat$year, months(dat$date)) != 0) > 2
    years_with_enough_d <- names(index[index])
    plt <- plt + geom_smooth(data=dat[dat$year %in% years_with_enough_d,], method = lm, formula = y ~ splines::bs(x, 3), se = FALSE, aes(fill=year)) +
       geom_dl(aes(label=year), method=list('last.points', cex=1.5))
    plt <- plt + geom_point(data=dat[!(dat$year %in% years_with_enough_d),], alpha=0.4) +
      geom_dl(aes(label=year), method=list('last.points', cex=1.5))
    plt <- plt + geom_point(alpha=0.1) +
      geom_dl(aes(label=year), method=list('last.points', cex=1.5))

    plt
  }
  output$scatterSelected <- renderPlot({
    show_scatter_plot('abundance')
  }) %>% bindCache(input$nav, input$monthrange, 'abundance', input$species, input$daterange, rounded_map_bounds())
  output$scatterSelected2Months <- renderPlot({
    show_scatter_plot('abundance', 2)
  }) %>% bindCache(input$nav, input$monthrange, 'abundance', input$species, input$daterange, rounded_map_bounds())

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    if (input$nav != 'Collection')
      return(NULL)

    print('running plot circle markers on data collection map')
    print(input$nav)

    colorBy <- paste0('abundance', '_', input$species)

    d <- dataSummarised()

    # d <- d %>% arrange(!!sym(colorBy)) %>%
    #   filter(!!sym(colorBy)!=0)
    d[d[, colorBy] == 0, colorBy] <- NA

    if (nrow(d) == 0) {
      leafletProxy("map") %>%
        clearMarkers()
      return(NULL)
    }

    colorData <- d[[colorBy]]
    # pal <- colorBin(colorpalette, colorData, 7, pretty = TRUE, na.color = 'transparent')

    if (all(is.na(colorData))) {
      pal <- colorFactor(colorpalette, colorData, 7, na.color = rgb(0,0,0,0.08))
    } else {
      pal <- colorNumeric(colorpalette, colorData, 7, na.color = rgb(0,0,0,0.08))
    }

    # bins <- colorQuantile(colorpalette, colorData,  na.rm=TRUE, names=FALSE)

    # sizeBy <- paste0('abundance', '_', input$species)
    # radius <- dataSummarised()[[sizeBy]] / max(dataSummarised()[[sizeBy]]) * 60000 + 500
    radius <- 4

    # format
    split_string <- str_split(colorBy, "_")
    measure <- split_string[[1]][1]
    species <- names(species_choices)[species_choices==split_string[[1]][2]]

    legend_string <- measure_to_readable_text(measure, species)

    outlines <- ifelse(is.na(d[,colorBy]), '#bdbdbd', 'black')
    opacity <- ifelse(is.na(d[,colorBy]), 0, 1)

    leafletProxy("map", data = d) %>%
      clearMarkers() %>%
      addCircleMarkers(~longitude, ~latitude, radius=radius, layerId=~datacode,
        stroke=T, color=outlines, weight=1, fillOpacity=opacity, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, opacity=opacity, values=colorData, title=legend_string,
        layerId="colorLegend", na.label='0')
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
  }) %>% bindEvent(input$nav, input$monthrange, 'abundance', input$species, input$daterange, input$datasetName)

  # Show a popup at the given location
  showdatacodePopup <- function(datacode, lat, lng, measure='abundance') {
    dat <- dataSummarised()
    if (is.character(datacode)) {
      # is the site and state concatenated together
      split_string <- str_split(datacode, '_')
      state <- split_string[[1]][2]
      site <- split_string[[1]][1]
      selecteddata <- dat[dat$state==state & dat$site==site,]
    } else {
      # is the datacode id
      selecteddata <- dat[dat$datacode == datacode,]
    }

    # get the info I want to display
    foo <- selecteddata[ , grepl( measure , names( selecteddata ))] %>%
      select(-contains('total'))
    species <- gsub(paste0(measure, "_"), "", colnames(foo))
    colnames(foo) <- species
    if (length(species[foo > 0]) > 0) {
      species_observed <- paste0("<i>", species[foo > 0], "</i>:")
    } else {
      species_observed <- ''
    }


    # bio_foo <- selecteddata[ , grepl( "biomass" , names( selecteddata ))]
    # bio_foo <- subset(bio_foo, select=-biomass_total)
    num_traps_text <- '1 trap'
    if (selecteddata$n_traps > 1) {
      num_traps_text <- paste(selecteddata$n_traps, 'traps')
    }

    num_visits_text <- '1 time'
    if (selecteddata$n_visits > 1) {
      num_visits_text <- paste(selecteddata$n_visits, "times")
    }

    if (selecteddata$min_date == selecteddata$max_date) {
      dates_text <- paste0(' on the ', format(selecteddata$min_date, format='%d %B %Y'))
    } else {
      dates_text <- paste0(' between ', format(selecteddata$min_date, format='%d %B %Y'), ' and ', format(selecteddata$max_date, format='%d %B %Y'))
    }

    content <- paste0(
      tags$h3(paste0(gsub("([A-Z])", " \\1", selecteddata$site), ", ", str_to_title(selecteddata$state))),
      paste0('<h4>', measure_to_readable_text(measure, 'beetles'), '</h4>'),
      "All beetles: ", round(selecteddata[paste0(measure, '_total')], 2), '<br>',
      paste(
        paste0(
        species_observed, round(foo[foo > 0], 2)
        ),
        collapse="<br>"
      ),
      tags$h4("Site information"),
      "Collected from ", num_traps_text,
      ' deployed ', num_visits_text,
      "<br> ", dates_text
    )
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = datacode)
  }

  # When map is clicked, show a popup with city info
  observe({
    if (input$nav != 'Collection')
      return()

    print('running clear popups')

    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()

    isolate({
      showdatacodePopup(event$id, event$lat, event$lng, 'abundance')
    })
  }) %>% bindEvent(input$map_marker_click)

  # When the prediction map is clicked, show a popup with raster info
  observe({
    click <- input$prediction_map_click

    isolate({
      r <- get_selected_prediction()
    })

    pos <- st_point(cbind(click$lng, click$lat)) %>%
      st_sfc() %>%
      st_set_crs(4326) %>%
      st_transform(st_crs(r))

    value <- st_extract(get_selected_prediction(), pos) %>%
      st_drop_geometry()

    proxy <- leafletProxy("prediction_map")

    if (is.na(value)) {
      proxy %>% clearPopups()
      return()
    }

    text <- paste0(
      "Estimate: ",
      round(value, 2), '<br>',
      "Latitude: ", round(click$lat, 3), '<br>',
      "Longitude: ", round(click$lng, 3), '<br>'
    )

    proxy %>% clearPopups() %>%
      addPopups(click$lng, click$lat, text)

  }) %>% bindEvent(input$prediction_map_click)

  isShown <- reactiveValues(shown=TRUE)

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
    print(isShown$shown)
  }

  observeEvent(input$showSidebar, {
    showHidePredButton()
  })

  observeEvent(input$hideSidebar, {
    showHidePredButton()
  })

  isShownMain <- reactiveValues(shown=TRUE)

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
    print(isShownMain$shown)
  }

  observeEvent(input$showMainPanel, {
    showHideButton()
  })
  observeEvent(input$hideMainPanel, {
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

  observe({
    sites <- if (is.null(input$states)) character(0) else {
      filter(dungfauna_occurrence, stateProvince %in% input$states) %>%
        `$`('locationID_site') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$sites[input$sites %in% sites])
    updateSelectizeInput(session, "sites", choices = sites,
      selected = stillSelected, server = TRUE)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      data <- input$goto$data
      lat <- input$goto$lat
      lng <- input$goto$lng
      showdatacodePopup(data, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$datatable <- DT::renderDataTable({
    df <- dungfauna_occurrence %>%
      filter(
        individualCount >= input$minTotalAbundance,
        individualCount <= input$maxTotalAbundance,
        is.null(input$states) | stateProvince %in% input$states,
        is.null(input$sites) | locationID_site %in% input$sites,
        is.null(input$speciesTable) | scientificName %in% input$speciesTable
      ) %>%
      mutate('Click to view on map' = paste('<a class="go-map" href="" data-lat="', decimalLatitude, '" data-long="', decimalLongitude, '" data-data="', paste0(locationID_site, '_', stateProvince), '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "datatable")

    DT::datatable(df, options = list(ajax = list(url = action), columnDefs=list(list(visible=FALSE, targets=c('datacode')))), escape = FALSE)
  })

  ## POPUPs
  shinyalert(
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
