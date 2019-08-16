#' Title
#'
#' @return
#' @export
#'
#' @examples
run_snapshots <- function() {
  shiny::runApp(system.file("app.R", package = "councilsnapshots"))
}
