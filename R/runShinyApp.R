
#' Run the shiny app visualisation
#'
#' Starts a visualisation of the dataset in a shiny app that can be
#' viewed locally in the browser. After calling this function,
#' If the app window doesn't start automatically,
#' navigate to the port printed to your console
#' (usually [localhost:8888](localhost:8888)) in your web browser.
#'
#' @export
#'
#' @examples
#' # start the visualisation with:
#' dungfaunaR::runShinyApp()
runShinyApp <- function() {
  appDir <- system.file("shiny-visualisation", "app", package = "dungfaunaR")

  if (appDir == "") {
    stop("Could not find shiny app directory. Try re-installing `dungfaunaR`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
