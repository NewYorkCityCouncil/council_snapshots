

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
      box(width = 12,
          title = "Landing Page",
          textOutput("Welcome to Council Snapshots")
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
