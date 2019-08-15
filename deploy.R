library(git2r)

clean_name <- function(str) {
  stringr::str_remove_all(str, "[^a-zA-Z0-9\\-_]")
}

app_name <- "council_snapshots"
branch_name <- repository_head()$name

if(branch_name != master) {
  rsconnect::deployApp(appName = clean_name(paste(app_name, "staging", branch_name, sep = "-")))
} else {
  rsconnect::deployApp(appName = app_name)
}
