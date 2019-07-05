library(shiny)
library(shinydashboard)
library(tibble)
library(ggplot2)
library(dplyr)
library(lubridate)

modules <- list.files(path = "modules", pattern = "\\.(R|r)$", full.names = TRUE)
lapply(modules, source)

utils <- list.files(path = "util", pattern = "\\.(R|r)$", full.names = TRUE)
lapply(utils, source)

current_week <- tbl(snapshots_db, "sr_top_10_week_district") %>%
  summarize(week = max(week)) %>%
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

header <- dashboardHeader(title = "Council Snapshots")

sidebar <- dashboardSidebar(
  selectInput("coun_dist", "Council district", 1:51, selected = 1),
  selectInput("week", "Week", week_labels, selected = current_week),
  sidebarMenu(
    menuItem("311", icon = icon("phone"),
             menuSubItem("First 311 page", "311_test")),
    menuItem("OEM", icon = icon("warning"),
             menuSubItem("First OEM page", "oem_test")),
    menuItem("HPD", icon = icon("home"),
             menuSubItem("First HPD page", "hpd_test"))
  )
)

body <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "council.css")),
  tabItems(
    tabItem("311_test",
            example_311_ui("num_complaints")
            ),
    tabItem("oem_test",
            plotOutput("test_plot2")),
    tabItem("hpd_test",
            plotOutput("test_plot3"))
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  output$test_plot1 <- renderPlot({
    hist(rnorm(100), main = paste("CD", input$coun_dist))
  })

  output$test_plot2 <- renderPlot({
    hist(rnorm(100), main = paste("CD", input$coun_dist))
  })

  output$test_plot3 <- renderPlot({
    hist(rnorm(100), main = paste("CD", input$coun_dist))
  })


  callModule(example_311, id = "num_complaints",
             coun_dist = reactive(input$coun_dist),
             week = reactive(input$week))
}

shinyApp(ui, server)
