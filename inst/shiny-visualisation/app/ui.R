# library(leaflet)
# library(shinyWidgets)
# library(stringr)
# library(shinyalert)

# Choices for drop-downs
vars_preds <- c("Biomass" = "biomass")
vars <- c("Average number of beetles trapped" = "abundance",
          "Average biomass of beetles trapped (g dry mass)" = "biomass")

species_images <- base::vector()
for (i in 1:base::length(species_choices)) {
  if (species_choices[i] == 'total') {
    species_images[i] = ''
  } else {
    species_images[i] <- base::paste0('beetle_images/',
                                stringr::str_replace(species_choices[i], ' ', '_'),
                                '.jpg')
  }
}


year_choices <-
  base::sort(base::unique(base::format(base::as.Date(alldatas$date), format = "%Y")))

menu_items <-
  htmltools::div(
    shiny::absolutePanel(
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

      htmltools::div(
        align = 'center',
        actionButton('showMainPanel', '▼', class = 'mainPanelButton'),
        shinyjs::hidden(div(
          id = 'hideMainPanel',
          actionButton('hideMainPanel', '▲', class = 'mainPanelButton')
        ))
      ),
      htmltools::div(
        id = "mainPanel",


        htmltools::div(
          h6(
            class = 'd-lg-none d-inline-align-middle',
            "Click the above arrow to view the map",
            align = 'center'
          ),
          h4("Select a measure and species to predict over the year", align = 'center')
        ),

        shiny::selectInput(
          "prediction_color",
          "Select a measure",
          vars_preds,
          selected = 'Biomass',
          width = '100%'
        ),
        shinyWidgets::pickerInput(
          inputId = "prediction_species",
          label = "Select a species",
          choices = species_choices,
          options = pickerOptions(size = 5),
          choicesOpt = list(content = c(
            base::paste0(
              "<div style='display:inline-block;vertical-align:middle;'><p style='display:inline-block;margin-left:2px;margin-top:10px;margin-bottom:10px;top:50%'>",
              base::names(species_choices)[1],
              "</p> <img src='",
              species_images[1],
              "' width=70 style='display:float'/></div>"
            ),
            base::paste0(
              "<div style='display:inline-block;vertical-align:middle;'><p style='display:inline-block;margin:0;top:50%;'><i>",
              base::names(species_choices)[2:base::length(species_choices)],
              "</i></p> <img src='",
              species_images[2:base::length(species_images)],
              "' width=70 style='display:float'/></div>"
            )
          )),
          width = '100%'
        ),
      )
    )
  )

year_selection_panel <-
  shiny::absolutePanel(
  id = 'year_selection_panel',
  fixed = TRUE,
  draggable = FALSE,
  width = '60%',
  height = 'auto',
  align = 'center',

  shinyWidgets::sliderTextInput(
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
  return(htmltools::div(
    # selectInput(
    #   "color",
    #   "Select a measure",
    #   vars,
    #   selected = 'Average number of beetles trapped',
    #   width = '100%'
    # ),
    shiny::selectInput(
      "datasetName",
      "Select a data set",
      dataset_sources,
      selected = 'All',
      width = '100%'
    ),
    # selectInput("species", "Select a species", species_choices, selected='total', width='100%'),
    shinyWidgets::pickerInput(
      inputId = "species",
      label = "Select a species",
      options = pickerOptions(size = 5),
      choices = species_choices,
      choicesOpt = list(content = c(
        base::paste0(
          "<div style='display:inline-block;vertical-align:middle;'><p style='display:inline-block;margin-left:2px;margin-top:10px;margin-bottom:10px;top:50%'>",
          base::names(species_choices)[1],
          "</p> <img src='",
          species_images[1],
          "' width=70 style='display:float'/></div>"
        ),
        base::paste0(
          "<div style='display:inline-block;vertical-align:middle;'><p style='display:inline-block;margin:0;top:50%;'><i>",
          base::names(species_choices)[2:base::length(species_choices)],
          "</i></p> <img src='",
          species_images[2:base::length(species_images)],
          "' width=70 style='display:float'/></div>"
        )
      )),
      width = '100%'
    ),
    shinyWidgets::pickerInput(
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

data_menu_items <-
  htmltools::div(
  # wide view
  shiny::absolutePanel(
    id = "controls",
    class = "panel panel-default",
    fixed = TRUE,
    draggable = FALSE,
    left = "auto",
    right = 15,
    width = '30%',
    height = "auto",

    htmltools::div(
      align = 'center',
      shiny::actionButton('showSidebar', '▼', class = 'sidebarButton'),
      shinyjs::hidden(div(
        id = 'hideSidebar',
        shiny::actionButton('hideSidebar', '▲', class = 'sidebarButton')
      ))
    ),

    htmltools::div(
      id = 'sidebar',
      htmltools::div(
        htmltools::h6(
          class = 'd-lg-none d-inline-align-middle',
          "Click the above arrow to view the map",
          align = 'center'
        ),
        htmltools::h4("Filter by data set, species and year", align = 'center')
      ),
      data_menu_inputs(),

      # plotOutput("histAbundance", height = 200),
      htmltools::div(
        class = 'd-lg-block d-none',
        htmltools::h4("Average catch in a trap in map area", align = 'center'),
        shiny::plotOutput("scatterSelected", height = 300)
      )
    )
  )
)


loading_screen <- htmltools::div(
  id='loading_screen',
  htmltools::div(
    htmltools::HTML('<div class="lds-roller"><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div></div>'),
    style = 'margin-top: 20%; margin-bottom: auto; text-align: center;'
  ),
  style = '
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
  predictions_tab <- shiny::tabPanel(
    "Prediction",
    shiny::verticalLayout(
      shinyWidgets::chooseSliderSkin("HTML5", color = 'blue'),
      htmltools::div(
        class = "outer",
        # If not using custom CSS, set height of leafletOutput to a number instead of percent
        leaflet::leafletOutput("prediction_map", width = "100%", height = "100%"),

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
collections_tab <- shiny::tabPanel(
  "Collection",
  htmltools::div(
    class = "outer",

    tags$head(# Include our custom CSS
      htmltools::includeCSS("styles.css"),
      htmltools::includeScript("gomap.js")),

    # If not using custom CSS, set height of leafletOutput to a number instead of percent
    leaflet::leafletOutput("map", width = "100%", height = "100%"),
    shiny::absolutePanel(
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
    data_menu_items
  )
)
if (include_data_table) {
  data_table_tab <- shiny::tabPanel(
    "Data",
    shiny::fluidRow(shiny::column(
      3,
      shiny::selectInput(
        "states",
        "States",
        c(
          "All states" = "",
          base::structure(base::unique(alldatas$state), names = base::unique(alldatas$state))
        ),
        multiple = TRUE
      )
    ),
    shiny::column(
      3,
      shiny::conditionalPanel(
        "input.states",
        shiny::selectInput("sites", "Sites", c("All sites" = ""), multiple =
                      TRUE)
      )
    )),
    shiny::fluidRow(shiny::column(
      1,
      shiny::numericInput(
        "minTotalAbundance",
        "Min abundance",
        min = 0,
        value = 0
      )
    ),
    shiny::column(
      1,
      shiny::numericInput(
        "maxTotalAbundance",
        "Max abundance",
        min = 0,
        value = 1000000
      )
    ),
    shiny::column(
      3,
      shiny::selectInput(
        "speciesTable",
        "Species",
        c(
          "All species" = "",
          base::structure(species, names = base::names(species))
        ),
        multiple = TRUE
      )
    )),
    htmltools::hr(),
    DT::dataTableOutput("datatable")
  )
} else {
  data_table_tab <- NULL
}

main <- shiny::navbarPage(
    id = "nav",
    collapsible = TRUE,
    theme = bslib::bs_theme(version = 5, bootswatch = 'zephyr'),
    shinyjs::useShinyjs(),
    collections_tab,
    predictions_tab,
    data_table_tab,
    shiny::conditionalPanel("false", icon("crosshair"))
  )


htmltools::div(
  loading_screen,
  main
)
