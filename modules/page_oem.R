
dists <- tbl(snapshots_db, "council_districts") %>%
  collect_geo()

page_oem_ui <- function(id) {

  ns <- NS(id)

  fluidPage(

    fluidRow(
      box(width = 12, title = "Emergency incident map",
          leafletOutput(ns("oem_map")))),
    fluidRow(
      box(width = 12, title = "Emergency incidents",
          DT::dataTableOutput(ns("oem_table"))))
  )
}

page_oem <- function(input, output, session, week, coun_dist) {


  trigger <- makeReactiveTrigger()

  oem_week_dist <- reactive({
    tbl(snapshots_db, "emergency_response") %>%
      filter(date_part("week", creation_date) == local(week())) %>%
      collect_geo() %>%
      st_join(dists) %>%
      filter(coun_dist == coun_dist())
  })

  output$oem_map <- renderLeaflet({
    trigger$trigger()
    leaflet() %>%
      councildown::addCouncilStyle()
  })

  observe({
    trigger$depend()
    bbox <- as.numeric(st_bbox(oem_week_dist()))


    leafletProxy("oem_map", data = oem_week_dist()) %>%
      clearGroup("oem_incidents") %>%
      addCircleMarkers(radius = 5, popup = ~incident_type,
                       fillOpacity = .8, fillColor = "#2F56A6", opacity = 0, weight = 15,
                       group = "oem_incidents") %>%
      clearControls() %>%
      flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4], options = list(duration = .25))
  })

  output$oem_table <- DT::renderDataTable({
    oem_week_dist() %>%
      as.data.frame() %>%
      select(incident_type, location, creation_date, closed_date) %>%
      DT::datatable(height = "400px", options = list(scrollX = TRUE, scrollY = TRUE))
  })

}
