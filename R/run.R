#' Run Council Snapshots App
#'
#' @param ... Further parameters passed to runApp
#'
#' @export
#'
run_snapshots <- function(...) {
  shiny::runApp(system.file("shinyApp/app.R", package = "councilsnapshots"), ...)
}
