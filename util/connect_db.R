library(dplyr)
library(pool)
library(sf)
library(RPostgreSQL)
library(dbplyr)

collect_geo <- function(x, n = NULL) {
  if (!is.null(n)) {
    x <- x %>%
      head(n)
  }

  st_read(dsn = x$src$con, query = db_sql_render(x$src$con, x))
}

host <- Sys.getenv("SNAPSHOTS_DB_HOST")
user <- Sys.getenv("SNAPSHOTS_DB_USER")
pw <- Sys.getenv("SNAPSHOTS_DB_PW")

snapshots_db <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  dbname = "snapshots",
  host = host,
  user = user,
  password = pw
)

onStop(function() {
  poolClose(snapshots_db)
})
