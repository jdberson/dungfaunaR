library(leaflet)
library(shinyWidgets)
library(stringr)
library(shinyalert)

# Choices for drop-downs
vars_preds <- c("Biomass" = "biomass")
vars <- c("Average number of beetles trapped" = "abundance",
          "Average biomass of beetles trapped (g dry mass)" = "biomass")

species_images <- vector()
for (i in 1:length(species_choices)) {
  if (species_choices[i] == 'total') {
    species_images[i] = ''
  } else {
    species_images[i] <- paste0('beetle_images/',
                                str_replace(species_choices[i], ' ', '_'),
                                '.jpg')
  }
}


year_choices <-
  sort(unique(format(as.Date(alldatas$date), format = "%Y")))

menu_items <-
  div(
    absolutePanel(
      id = "controls",
      class = "panel panel-default",
      fixed = TRUE,
      draggable = FALSE,
      top = 100,
      left = "auto",
      right = 15,
      bottom = "auto",
      width = "auto",
      height = "auto",

      div(
        align = 'center',
        actionButton('showMainPanel', '▼', class = 'mainPanelButton'),
        shinyjs::hidden(div(
          id = 'hideMainPanel',
          actionButton('hideMainPanel', '▲', class = 'mainPanelButton')
        ))
      ),
      div(
        id = "mainPanel",


        div(
          h6(
            class = 'd-lg-none d-inline-align-middle',
            "Click the above arrow to view the map",
            align = 'center'
          ),
          h4("Select a measure and species to predict over the year", align = 'center')
        ),

        selectInput(
          "prediction_color",
          "Select a measure",
          vars_preds,
          selected = 'Biomass',
          width = '100%'
        ),
        pickerInput(
          inputId = "prediction_species",
          label = "Select a species",
          choices = species_choices,
          options = pickerOptions(size = 5),
          choicesOpt = list(content = c(
            paste0(
              "<div style='display:inline-block;vertical-align:middle;'><p style='display:inline-block;margin-left:2px;margin-top:10px;margin-bottom:10px;top:50%'>",
              names(species_choices)[1],
              "</p> <img src='",
              species_images[1],
              "' width=70 style='display:float'/></div>"
            ),
            paste0(
              "<div style='display:inline-block;vertical-align:middle;'><p style='display:inline-block;margin:0;top:50%;'><i>",
              names(species_choices)[2:length(species_choices)],
              "</i></p> <img src='",
              species_images[2:length(species_images)],
              "' width=70 style='display:float'/></div>"
            )
          )),
          width = '100%'
        ),
      )
    )
  )

year_selection_panel <- absolutePanel(
  id = 'year_selection_panel',
  fixed = TRUE,
  draggable = FALSE,
  width = '60%',
  height = 'auto',
  align = 'center',

  sliderTextInput(
    inputId = "prediction_monthrange",
    label = "",
    choices = month.name.custom,
    selected = month.name.custom[1],
    animate = animationOptions(
      interval = 1100,
      loop = FALSE,
      playButton = NULL,
      pauseButton = NULL
    ),
    grid = FALSE,
    width = '100%',
  ),
)


data_menu_inputs <- function() {
  return(div(
    selectInput(
      "color",
      "Select a measure",
      vars,
      selected = 'Average number of beetles trapped',
      width = '100%'
    ),
    selectInput(
      "datasetName",
      "Select a data source",
      dataset_sources,
      selected = 'All',
      width = '100%'
    ),
    # selectInput("species", "Select a species", species_choices, selected='total', width='100%'),
    pickerInput(
      inputId = "species",
      label = "Select a species",
      options = pickerOptions(size = 5),
      choices = species_choices,
      choicesOpt = list(content = c(
        paste0(
          "<div style='display:inline-block;vertical-align:middle;'><p style='display:inline-block;margin-left:2px;margin-top:10px;margin-bottom:10px;top:50%'>",
          names(species_choices)[1],
          "</p> <img src='",
          species_images[1],
          "' width=70 style='display:float'/></div>"
        ),
        paste0(
          "<div style='display:inline-block;vertical-align:middle;'><p style='display:inline-block;margin:0;top:50%;'><i>",
          names(species_choices)[2:length(species_choices)],
          "</i></p> <img src='",
          species_images[2:length(species_images)],
          "' width=70 style='display:float'/></div>"
        )
      )),
      width = '100%'
    ),
    pickerInput(
      inputId = "daterange",
      label = "Select years",
      choices = year_choices,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      selected = year_choices,
      width = '100%'
    ),
  ))
}

data_menu_items <- div(
  # wide view
  absolutePanel(
    id = "controls",
    class = "panel panel-default",
    fixed = TRUE,
    draggable = FALSE,
    left = "auto",
    right = 15,
    width = '30%',
    height = "auto",

    div(
      align = 'center',
      actionButton('showSidebar', '▼', class = 'sidebarButton'),
      shinyjs::hidden(div(
        id = 'hideSidebar',
        actionButton('hideSidebar', '▲', class = 'sidebarButton')
      ))
    ),

    div(
      id = 'sidebar',
      div(
        h6(
          class = 'd-lg-none d-inline-align-middle',
          "Click the above arrow to view the map",
          align = 'center'
        ),
        h4("Select a measure, species and year to view on the map ", align = 'center')
      ),
      data_menu_inputs(),

      # plotOutput("histAbundance", height = 200),
      div(
        class = 'd-lg-block d-none',
        h4("Average catch in a trap in map area", align = 'center'),
        plotOutput("scatterSelected", height = 300)
      )
    )
  )
)


loading_screen <- div(
  id='loading_screen',
  div(
    HTML('<div class="lds-roller"><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div></div>'),
    style='margin-top: 20%; margin-bottom: auto; text-align: center;'
  ),
  style='
    position: fixed; /* Stay in place */
    z-index: 1000; /* Sit on top */
    padding-top: 100px; /* Location of the box */
    left: 0;
    top: 0;
    width: 100%; /* Full width */
    height: 100%; /* Full height */
    overflow: auto; /* Enable scroll if needed */
    color: rgba(1.0,1.0,1.0,1.0);
  '
)

if (include_predictions) {
  predictions_tab <- tabPanel(
    "Prediction",
    verticalLayout(
      chooseSliderSkin("HTML5", color = 'blue'),
      div(
        class = "outer",
        # If not using custom CSS, set height of leafletOutput to a number instead of percent
        leafletOutput("prediction_map", width = "100%", height = "100%"),

        year_selection_panel,

        menu_items,

        tags$div(id = "cite",
                 'Data compiled for ',
                 tags$em('TODO'),
                 ' TODO.'
        )
      )
    )
  )
} else {
  predictions_tab <- NULL
}
collections_tab <- tabPanel(
  "Collection",
  div(
    class = "outer",

    tags$head(# Include our custom CSS
      includeCSS("styles.css"),
      includeScript("gomap.js")),

    # If not using custom CSS, set height of leafletOutput to a number instead of percent
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(
      id = 'data_year_selection_panel',
      fixed = TRUE,
      draggable = FALSE,
      bottom = 5,
      left = 0,
      right = 0,
      top = 'auto',
      width = '60%',
      height = 'auto',
      align = 'center',
      sliderTextInput(
        inputId = "monthrange",
        label = "",
        choices = month.name,
        selected = month.name[c(1, 12)],
        animate = FALSE,
        grid = FALSE,
        width = '100%'
      ),
    ),
    # Shiny versions prior to 0.11 should use class = "modal" instead.
    data_menu_items,
    tags$div(
      id = "cite",
      'Data compiled for ',
      tags$em('TODO'),
      ' TODO.',
      align = 'right'
    )
  )
)
if (include_data_table) {
  data_table_tab <- tabPanel(
    "Data",
    fluidRow(column(
      3,
      selectInput(
        "states",
        "States",
        c(
          "All states" = "",
          structure(unique(alldatas$state), names = unique(alldatas$state)),
          "wa" = "wa"
        ),
        multiple = TRUE
      )
    ),
    column(
      3,
      conditionalPanel(
        "input.states",
        selectInput("sites", "Sites", c("All sites" = ""), multiple =
                      TRUE)
      )
    )),
    fluidRow(column(
      1,
      numericInput(
        "minTotalAbundance",
        "Min abundance",
        min = 0,
        value = 0
      )
    ),
    column(
      1,
      numericInput(
        "maxTotalAbundance",
        "Max abundance",
        min = 0,
        value = 1000000
      )
    )),
    hr(),
    DT::dataTableOutput("datatable")
  )
} else {
  data_table_tab <- NULL
}

main <- navbarPage(
    title = span(
      tags$a(
        href = 'https://uwa.edu.au',
        img(src = 'logos/uwa.png', width = 100),
        style = 'text-decoration: none;'
      ),
      tags$a(
        href = 'https://dungbeetles.shinyapps.io/dungbeetlemaps/',
        img(src = 'logos/dung-beetle-maps.png', width = 150, style = 'padding-right: 15px; padding-left: 15px;'),
        style = 'text-decoration: none;'
      ),
      # only include other logos if not on mobile (so they fit)
      tags$a(
        class = 'd-lg-inline d-none',
        href = 'https://www.mla.com.au/',
        img(src = 'logos/mla_logo_home.webp', width = 90, style = 'padding-right: 15px;'),
        style = 'text-decoration: none;'
      ),
      tags$a(
        class = 'd-lg-inline d-none',
        href = 'https://www.dungbeetles.com.au/',
        img(src = 'logos/dbee-logo-stacked_primary.png', width = 90),
        style = 'text-decoration: none;'
      )
    ),
    id = "nav",
    collapsible = TRUE,
    theme = bslib::bs_theme(version = 5, bootswatch = 'zephyr'),
    shinyjs::useShinyjs(),
    collections_tab,
    predictions_tab,
    data_table_tab,
    conditionalPanel("false", icon("crosshair"))
  )


div(
  loading_screen,
  main
)
