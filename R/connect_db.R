#' collect_geo
#' Helper function to replace collect() when we need an sf dataframe
#' @param x A \code{tbl}
#' @param n Number of rows to collect. If \code{n} is NULL all rows are downloaded.
#'
#' @return A \code{sf} data frame.
#' @export
#'
#'
#' @importFrom dplyr %>%
collect_geo <- function(x, n = NULL) {
  if (!is.null(n)) {
    x <- x %>%
      head(n)
  }

  # st_read can take a DBI connection and a query
  sf::st_read(dsn = x$src$con, query = dbplyr::db_sql_render(x$src$con, x))
}

