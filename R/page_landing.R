

#' Landing UI
#'
#' @param id Unique id
#' @return A shiny UI
#' @export
#'
page_landing_ui <- function(id) {

  ns <- NS(id)

  fluidPage(

    fluidRow(
      column(11, offset = 1,
             tags$div(
               tags$span(style="font-size:600%; font-family:Georgia", "Council Snapshots"),
               tags$h2("An Open Data Plug to Your District")
             )
      )
    ),
    hr(),
    fluidRow(
      column(width = 3, offset = 1,
             wellPanel(
               tags$h3("Select your desired:"),
               h4(tags$span(class="glyphicon glyphicon-chevron-down"), "Council District"),
               h4(tags$span(class="glyphicon glyphicon-chevron-down"), "Week"),
               h4(tags$span(class="glyphicon glyphicon-chevron-down"), "Dataset"),
               div(style = "text-align:center;",
                   tags$a(class = 'sidebar-toggle btn btn-success btn-lg',
                          style = "display:inline-block;width:100%",
                          `data-toggle` = 'offcanvas', role = 'button', 'Explore Now')
               ))
      ),
      column(width = 3,
             wellPanel(
               tags$h3(tags$ul(
                 tags$li("See where things are happening."),
                 tags$li("Track trends & make insights."),
                 tags$li("Be aware of top issues."),
                 tags$li("Dig deeper.")
               )
               )
             )
      ),
      column(width = 3,
             wellPanel(
               h3("New Developments"), br(),
               "Tentative Dates")
      )
    ),
    hr(),
    fluidRow(
      column(width = 3, offset = 1,
             wellPanel(
               h3("Watch Tutorial"),
               br(),
               tags$a(href="https://youtu.be/b2jPWhcD3OM", icon("play-circle", "fa-2x")),
               p("Learn to use the dashboard")
             )
      ),
      column(width = 3,
             wellPanel(
               h3("Contact Us"), br(),
               fluidRow(
                 column(width = 4,
                        tags$a(style= "display: center;", href="https://council.nyc.gov/data/",icon("globe", "fa-2x")), h6("Visit Us")
                 ),
                 column(width = 4,
                        tags$a(href="https://twitter.com/nyccouncildata", icon("hashtag", "fa-2x")), h6("Follow Us")
                 ),
                 column(width = 4,
                        tags$a(href="#", icon("envelope", "fa-2x")), h6("Email Us")
                 )
               )
             )
      ),
      column(width = 3,
             wellPanel(
               h3("Feedback"), br(), p("Let us know if you have any issues."),
               p("Tell us your suggestions for improvement.")
             )
      )
    )
  )
}





#' OEM Server
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @export
#'
page_landing <- function(input, output, session) {
}
