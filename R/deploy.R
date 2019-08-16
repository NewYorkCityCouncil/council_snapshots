#' Deploy to shinyapps.io from Travis
#'
#' @param host database hostname
#' @param user database username
#' @param password database password
#' @param branchName branch being deployed from
#' @param token rsconnect token
#' @param secret rsconnect secret
#'
#' @export
deploy_shinyapps_io <- function(host = Sys.getenv("SNAPSHOTS_DB_HOST"),
                                user = Sys.getenv("SNAPSHOTS_DB_HOST"),
                                password = Sys.getenv("SNAPSHOTS_DB_PW"),
                                branchName = Sys.getenv("TRAVIS_BRANCH"),
                                token = Sys.getenv("RSCONNECT_TOKEN"),
                                secret = Sys.getenv("RSCONNECT_SECRET")) {

  # Check for all needed info
  if(any(c(host, user, password) == "")) {
    stop("Missing database credentials")
  }

  if(branchName == "") {
    stop("Missing name of deployment")
  }

  if(any(c(token, secret) == "")) {
    stop("Missing rsconnect credentials")
  }


  # Create app name so I can stage branches
  clean_name <- function(str) {
    gsub("[^a-zA-Z0-9_\\-]", "", str)
  }

  if(branchName == "master") {
    appName <- "council_snapshots"
  } else{
    appName <- clean_name(paste("council_snapshots", "staging", branchName, sep = "-"))
  }

  # Write app environment variables to local package directory
  writeLines(c(paste0("SNAPSHOTS_DB_HOST=", host),
               paste0("SNAPSHOTS_DB_USER=", user),
               paste0("SNAPSHOTS_DB_PW=", password)),
             con = file.path(system.file("shinyApp", package = "councilsnapshots"), ".Renviron"))

  # Deploy app to shinyapps.io
  rsconnect::setAccountInfo(name='nycc',
                            token=token,
                            secret=secret)
  rsconnect::deployApp(appDir = system.file("shinyApp", package = "councilsnapshots"),
                       appName = appName)
}
