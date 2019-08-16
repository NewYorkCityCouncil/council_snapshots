#' Run Council Snapshots App
#'
#' @export
#'
run_snapshots <- function() {
  shiny::runApp(system.file("shinyApp/app.R", package = "councilsnapshots"))
}
