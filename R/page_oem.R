

#' OEM UI
#'
#' @param id Unique id
#'
#' @return A shiny UI
#' @export
#'
#' @import sf leaflet stringr purrr plotly shinycssloaders shinydashboard
page_oem_ui <- function(id) {

  ns <- NS(id)

  fluidPage(

    fluidRow(
      box(width = 12,
          title = tagList("Emergency incident map",
                          help_tooltip(ns("oem-map-box"),
                                       "Location of emergency incidents",
                                       paste("This map shows the location of emergency",
                                             "incidents this week. The size shows the",
                                             "total length of the emergency."))),
          leafletOutput(ns("oem_map")) %>% withSpinner())),
    fluidRow(
      box(width = 12,
          title = tagList("Emergency incidents",
                          help_tooltip(ns("oem-table-box"),
                                       "List of emergency incidents",
                                       paste("Here are more details about all the",
                                             "emergency incidnets this week."))),
          DT::dataTableOutput(ns("oem_table")) %>% withSpinner()))
  )
}

#' Difftime formaters
#'
#' @param x a difftime
#'
#' @return Character vector
#' @export
#'
Fmt <- function(x) {
  UseMethod("Fmt")
}
Fmt.difftime <- function(x) {
  units(x) <- "secs"
  x <- unclass(x)
  NextMethod()
}
Fmt.default <- function(x) {
  y <- abs(x)

  # if(is.na(y)){ return("Ongoing as of last update")}

  sprintf("%02d hours %02d minutes",
          y %/% 3600,  # hours
          y %% 3600 %/% 60) # minutes
}


#' OEM Server
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param coun_dist reactive value holding selected council district
#' @param week reactive value holding selected week
#' @param snapshots_db The pool object holding database connections
#'
#' @export
#'
page_oem <- function(input, output, session, week, coun_dist, snapshots_db) {
  dists <- tbl(snapshots_db, "council_districts") %>%
    collect_geo()


  trigger <- makeReactiveTrigger()

  oem_week_dist <- reactive({
    tbl(snapshots_db, "emergency_response") %>%
      filter(date_part("week", creation_date) == local(week())) %>%
      collect_geo() %>%
      st_join(dists) %>%
      filter(coun_dist == coun_dist()) %>%
      mutate(duration = closed_date - creation_date,
             duration_pretty = ifelse(is.na(duration), "Ongoing as of last update", Fmt(duration)))
  })

  output$oem_map <- renderLeaflet({
    # trigger$trigger()
    map_updater$resume()
    leaflet() %>%
      councildown::addCouncilStyle()
  })

  map_updater <- observe(suspended = TRUE, {
    # trigger$depend()
    bbox <- as.numeric(st_bbox(oem_week_dist()))


    leafletProxy("oem_map", data = oem_week_dist()) %>%
      clearGroup("oem_incidents") %>%
      addCircleMarkers(radius = ~ifelse(is.na(duration), 5, 25*sqrt(as.double(duration)/max(as.double(duration), na.rm = TRUE))),
                       popup = ~paste(incident_type, location, paste(creation_date, closed_date, sep = " - "), duration_pretty, sep = "<br>"),
                       fillOpacity = .8, fillColor = ~ifelse(is.na(closed_date), "#D05D4E","#2F56A6"), opacity = 0, weight = 15,
                       group = "oem_incidents") %>%
      clearControls() %>%
      flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4], options = list(duration = .25))
  })

  output$oem_table <- DT::renderDataTable({
    oem_week_dist() %>%
      as.data.frame() %>%
      select(incident_type, location, creation_date, closed_date) %>%
      mutate(creation_date = format(creation_date, format = "%b %e %Y %I:%M %p"),
             closed_date = format(closed_date, format = "%b %e %Y %I:%M %p")) %>%
      DT::datatable(colnames = c("Incident type", "Location", "Created at", "Closed at"),
                    height = "400px", options = list(scrollX = TRUE, scrollY = TRUE))
  })

}
