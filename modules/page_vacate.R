page_vacate_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      box(title = "Issued vacate orders",
          leafletOutput(ns("issued_vacate")) %>% withSpinner()),
      box(title = "Rescinded vacate orders",
          leafletOutput(ns("rescinded_vacate")) %>% withSpinner())
    ),

    fluidRow(
      uiOutput(ns("issued_table")),
      uiOutput(ns("rescinded_table"))
    )
  )
}

page_vacate <- function(input, output, session, coun_dist, week) {

  ns <- session$ns

  trigger_issued <- makeReactiveTrigger()
  trigger_rescinded <- makeReactiveTrigger()

  issued_week_dist <- reactive({
    tbl(snapshots_db, "vacate_orders") %>%
      filter(council_district == local(coun_dist()),
             date_part("week", vacate_effective_date) == local(week())) %>%
      mutate(address = paste(number, street)) %>%
      collect_geo()

  })

  rescinded_week_dist <- reactive({
    tbl(snapshots_db, "vacate_orders") %>%
      filter(council_district == local(coun_dist()),
             date_part("week", rescind_date) == local(week())) %>%
      mutate(address = paste(number, street)) %>%
      collect_geo()
  })

  output$issued_vacate <- renderLeaflet({
    trigger_issued$trigger()
    leaflet() %>%
      councildown::addCouncilStyle()
  })

  output$rescinded_vacate <- renderLeaflet({
    trigger_rescinded$trigger()
    leaflet() %>%
      councildown::addCouncilStyle()
  })

  observe(suspended = TRUE, {
    trigger_issued$depend()
    leafletProxy("issued_vacate", data = issued_week_dist()) %>%
      clearGroup("issued")

    if (nrow(issued_week_dist()) > 0) {
      bbox <- as.numeric(st_bbox(issued_week_dist()))

      leafletProxy("issued_vacate", data = issued_week_dist()) %>%
        addCircleMarkers(radius = 5,
                         fillOpacity = .8, fillColor = "#2F56A6", opacity = 0, weight = 15,
                         popup = ~paste(address, primary_vacate_reason, vacate_effective_date, sep = "<br>"),
                         group = "issued") %>%
        clearControls() %>%
        flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4], options = list(duration = .25))
    }
  })

  observe(suspended = TRUE, {
    trigger_rescinded$depend()

    leafletProxy("rescinded_vacate", data = rescinded_week_dist()) %>%
      clearGroup("rescinded")

    if (nrow(rescinded_week_dist()) > 0) {
      bbox <- as.numeric(st_bbox(rescinded_week_dist()))

      leafletProxy("rescinded_vacate", data = rescinded_week_dist()) %>%
        addCircleMarkers(radius = 5,
                         fillOpacity = .8, fillColor = "#2F56A6", opacity = 0, weight = 15,
                         popup = ~paste(address, primary_vacate_reason, rescind_date, sep = "<br>"),
                         group = "rescinded") %>%
        clearControls() %>%
        flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4], options = list(duration = .25))
    }
  })

  output$issued_table <- renderUI({
    if (nrow(issued_week_dist()) > 0) {
      box(title = "Issued vacate orders",
          DT::dataTableOutput(ns("issued_table_output")))
    } else {
      box(title = "No vacate orders issued")
    }
  })

  output$rescinded_table <- renderUI({
    if (nrow(rescinded_week_dist()) > 0) {
      box(title = "Rescinded vacate orders",
          DT::dataTableOutput(ns("rescinded_table_output")))
    } else {
      box(title = "No vacate orders rescinded")
    }
  })

  output$issued_table_output <- DT::renderDataTable({
    # req(nrow(issued_week_dist) > 0)

    issued_week_dist() %>%
      as.data.frame() %>%
      select(address, primary_vacate_reason, vacate_type, vacate_effective_date, number_of_vacated_units) %>%
      DT::datatable(options = list(scrollX = TRUE, scrollY = TRUE))
  })

  output$rescinded_table_output <- DT::renderDataTable({
    # req(nrow(rescinded_week_dist) > 0)

    rescinded_week_dist() %>%
      as.data.frame() %>%
      select(address, primary_vacate_reason, vacate_type, vacate_effective_date, number_of_vacated_units) %>%
      DT::datatable(options = list(scrollX = TRUE, scrollY = TRUE))
  })
}
