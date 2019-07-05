library(shiny)
library(shinydashboard)
library(tibble)
library(ggplot2)
library(dplyr)

modules <- list.files(path = "modules", pattern = "\\.(R|r)$", full.names = TRUE)
lapply(modules, source)

utils <- list.files(path = "util", pattern = "\\.(R|r)$", full.names = TRUE)
lapply(utils, source)

header <- dashboardHeader(title = "Council Snapshots")

sidebar <- dashboardSidebar(
  selectInput("coun_dist", "Council district", 1:51, selected = 1),
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
            plotOutput("test_plot1")),
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

}

shinyApp(ui, server)
