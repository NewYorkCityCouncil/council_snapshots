

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
      column(12, offset = 1,
             tags$div(
               tags$span(style="font-size:600%; font-family:Georgia", "Council Snapshots"),
               tags$h2("An Open Data Plug to Your District")
             )
            )
      ),
    hr(),
    fluidRow(
      column(width = 2, offset = 1,
             tags$a(style = "display:inline-block", class = 'sidebar-toggle btn btn-success btn-lg',
                    `data-toggle` = 'offcanvas', role = 'button', 'Explore Now')
      ),
      column(width = 3,
             box(
               tags$h4("Select your desired:"),
               tags$span(h4(class="glyphicon glyphicon-chevron-down","Council District")),
               br(),
               tags$span(h4(class="glyphicon glyphicon-chevron-down","Week")),
               br(),
               tags$span(h4(class="glyphicon glyphicon-chevron-down","Dataset"))
               )
             ),
      column(width = 3,
             tags$h2(tags$ul(
                 tags$li("See where things are happening."),
                 tags$li("Track trends & make insights."),
                 tags$li("Be aware of top issues."),
                 tags$li("Dig deeper.")
                 )
             )
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
    ),
    br(),
    fluidRow(
      column(width = 9, offset = 1,
             wellPanel(
               h3("New Developments"), br(),
               "Tentative Dates")
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
