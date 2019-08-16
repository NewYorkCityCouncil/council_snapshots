# library(git2r)
#
# clean_name <- function(str) {
#   gsub("[^a-zA-Z0-9_\\-]", "", str)
# }
#
# app_name <- "council_snapshots"
# branch_name <- repository_head()$name
#
# if(branch_name != "master") {
#   rsconnect::deployApp(appName = clean_name(paste(app_name, "staging", branch_name, sep = "-")))
# } else {
#   rsconnect::deployApp(appName = app_name)
# }

# rsconnect::deployApp(appName = "council_snapshots")

#' Title
#'
#' @return
#' @export
#'
#' @examples
deploy_shinyapps_io <- function() {
  rsconnect::deployApp(appDir = system.file("shinyApp", package = "councilsnapshots"))
}
