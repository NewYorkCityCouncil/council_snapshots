library(shiny)
library(shinydashboard)
library(tibble)
library(ggplot2)
library(dplyr)
library(lubridate)
library(extrafont)
library(webshot)
library(stringr)
library(pool)
library(plotly)
library(shinycssloaders)
library(leaflet)

# dir.create("~/.fonts")
# file.copy("www/Open_Sans/OpenSans-Regular.ttf'", "~/.fonts")
# system('fc-cache -f ~/.fonts')

# credentials are stored as environment variables
host <- Sys.getenv("SNAPSHOTS_DB_HOST")
user <- Sys.getenv("SNAPSHOTS_DB_USER")
pw <- Sys.getenv("SNAPSHOTS_DB_PW")

# Create a pool of database connections. This way the app can send concurrent
# queries when multiple users are requesting data
snapshots_db <- pool::dbPool(
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


webshot::install_phantomjs()

options(spinner.color="#2F56A6")

# utils <- list.files(path = "util", pattern = "\\.(R|r)$", full.names = TRUE)
# lapply(utils, source)
#
# modules <- list.files(path = "modules", pattern = "\\.(R|r)$", full.names = TRUE)
# lapply(modules, source)

current_week <- tbl(snapshots_db, "sr_top_10_week_district_closed") %>%
  group_by(coun_dist) %>%
  summarize(week = max(week)) %>%
  ungroup() %>%
  summarize(week = min(week)) %>%
  pull()

weeks <- tibble(week_n = 1:current_week) %>%
  mutate(end_date = ceiling_date(ymd("2019-01-01"), unit = "week")+ 7*(week_n-1),
         start_date = end_date - 6,
         start_date = if_else(year(start_date) < year(end_date), ymd("2019-01-01"), start_date),
         end_date = if_else(year(end_date) > year(start_date), ymd("2019-12-31"), end_date),
         label = paste(format(start_date, format = "%b %e"), "-", format(end_date, format = "%b %e")) %>%
           str_replace_all("\\s+", " "))
week_labels <- weeks %>% pull(week_n)
names(week_labels) <- weeks %>% pull(label)

header <- dashboardHeader(title = "")

sidebar <- dashboardSidebar(
  selectInput("coun_dist", "Council district", 1:51, selected = 1),
  selectInput("week", "Week", week_labels, selected = current_week),
  sidebarMenu(
    menuItem("311", icon = icon("phone"),
             menuSubItem("Opened complaints", "311_opened"),
             menuSubItem("Closed complaints", "311_closed")),
    menuItem("OEM", icon = icon("warning"),
             menuSubItem("Emergency incidents", "oem_created")),
    menuItem("HPD", icon = icon("home"),
             menuSubItem("Vacate orders", "vacate_issued"))
  ),
  downloadButton("pdf_report", label = "Download", style = "background-color: #fff;color: #444;display: block;margin: 12px 15px 0px 15px;")
)

body <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "council.css"),
  tags$script(async = NA, src="https://www.googletagmanager.com/gtag/js?id=UA-111461633-2"),
  includeScript("analytics.js")),
  tabItems(
    tabItem("311_opened",
            page_311_ui("num_complaints")
            ),
    tabItem("311_closed",
            page_311_ui("num_complaints_closed", open = FALSE)),
    tabItem("oem_created",
            page_oem_ui("oem_incident")),
    tabItem("vacate_issued",
            page_vacate_ui("hpd_vacate"))
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "black")

server <- function(input, output, session) {

  callModule(page_311, id = "num_complaints",
             coun_dist = reactive(input$coun_dist),
             week = reactive(input$week),
             open = TRUE,
             current_week = current_week,
             snapshots_db)

  callModule(page_311, id = "num_complaints_closed",
             coun_dist = reactive(input$coun_dist),
             week = reactive(input$week),
             open = FALSE,
             current_week = current_week)

  callModule(page_oem, id = "oem_incident",
             coun_dist = reactive(input$coun_dist),
             week = reactive(input$week),
             snapshots_db)

  callModule(page_vacate, id = "hpd_vacate",
             coun_dist = reactive(input$coun_dist),
             week = reactive(input$week),
             snapshots_db)


  output$pdf_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.docx",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).

      tmp_dir <- tempdir()

      # tempReport <- file.path(tmp_dir))
      file.copy("pdf_report/", tmp_dir, overwrite = TRUE, recursive = TRUE)

      # print(list.files(tmp_dir))

      # Set up parameters to pass to Rmd document
      params <- list(coun_dist = input$coun_dist,
                     week = input$week)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      # showModal(modalDialog(
      #   title = "Generating report",
      #   "Crunching the freshest data just for you! This might take a minute."
      # ))

      withProgress({
        rmarkdown::render(file.path(tmp_dir, "pdf_report", "pdf_report.Rmd"), output_file = file,
                        params = params, envir = new.env()
        )},
        value = .1,
        message = "Generating report",
        detail = "Crunching the freshest data just for you!")


    }
  )
}

shinyApp(ui, server)
