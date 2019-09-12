library(testthat)

remotes::install_github('newyorkcitycouncil/council_snapshots', ref = Sys.getenv('TRAVIS_COMMIT'))

library(councilsnapshots)

test_shinyapps_io()

test_check("councilsnapshots")

test_shinyapps_io(remove = TRUE)
