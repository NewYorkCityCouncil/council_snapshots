library(dplyr)
library(pool)
library(sf)
library(RPostgreSQL)
library(dbplyr)

# Helper function to replace collect() when we need an sf dataframe

collect_geo <- function(x, n = NULL) {
  if (!is.null(n)) {
    x <- x %>%
      head(n)
  }

  # st_read can take a DBI connection and a query
  st_read(dsn = x$src$con, query = db_sql_render(x$src$con, x))
}

# credentials are stored as environment variables
host <- Sys.getenv("SNAPSHOTS_DB_HOST")
user <- Sys.getenv("SNAPSHOTS_DB_USER")
pw <- Sys.getenv("SNAPSHOTS_DB_PW")

# Create a pool of database connections. This way the app can send concurrent
# queries when multiple users are requesting data
#
# if (!interactive()) {

  snapshots_db <- dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = "snapshots",
    host = host,
    user = user,
    password = pw
  )

  # Make sure we close database connections when the app exits
  onStop(function() {
    poolClose(snapshots_db)
  })
# } else {
#   snapshots_db <- DBI::dbConnect(
#     drv = RPostgreSQL::PostgreSQL(),
#     dbname = "snapshots",
#     host = host,
#     user = user,
#     password = pw
#   )
# }
