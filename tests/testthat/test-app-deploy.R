library(RSelenium)

context("test-app-deploy")

clean_name <- function(str) {
  gsub("[^a-zA-Z0-9_\\-]", "", str)
}

if(branchName == "master") {
  appName <- "council_snapshots-testing"
} else{
  appName <- clean_name(paste("council_snapshots", "staging", branchName, "testing", sep = "-"))
}

remDr <- remoteDriver()
remDr$open(silent = TRUE)
appURL <- paste("https://nycc.shinyapps.io", appName, sep = "/")

test_that("can connect to app", {
  remDr$navigate(appURL)
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "")
})

remDr$close()
