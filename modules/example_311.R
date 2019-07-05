library(leaflet)
library(stringr)
library(purrr)

example_311_ui <- function(id) {

  ns <- NS(id)

  # tagList(
  fluidRow(
    box(title = "Most common complaints", solidHeader = TRUE,
      plotOutput(ns("complaint_type_cd_week"))
    ),
    box(title = "Complaint locations", solidHeader = TRUE,
      leafletOutput(ns("complaint_map")),
      uiOutput(ns("map_legend"))
    )
  )
  # )

}

example_311 <- function(input, output, session, coun_dist, week) {

  dist_week <- reactive({
    req(coun_dist)
    req(week)

    coun_dist <- coun_dist()
    week <- week()

    tbl(snapshots_db, "sr_top_10_week_district") %>%
      filter(coun_dist == .env$coun_dist,
             week ==.env$week) %>%
      collect_geo()
  })

  output$complaint_type_cd_week <- renderPlot({
    dist_week() %>%
      mutate(complaint_type = str_wrap(complaint_type, 15)) %>%
      ggplot(aes(reorder(complaint_type, n), n)) +
      geom_col() +
      coord_flip() +
      labs(title = "Top complaints",
           x = "Complaint type",
           y = "Number of complaints") +
      councildown::theme_nycc(print = FALSE)
  })

  output$complaint_map <- renderLeaflet({
    leaflet() %>%
      councildown::addCouncilStyle()
  })


  pal <- reactive({
    req(dist_week)
    pal <- colorFactor(councildown::nycc_pal()(length(unique(dist_week()$complaint_type))),
                       dist_week()$complaint_type)

  })


  observe({

    req(dist_week)
    pal <- pal()

    bbox <- as.numeric(st_bbox(dist_week()))

    leafletProxy("complaint_map", data = dist_week() %>% st_cast("POINT")) %>%
      clearMarkers() %>%
      addCircleMarkers(radius = 4, stroke = FALSE, fillOpacity = .8,
                       fillColor = ~pal(complaint_type),
                       popup = ~ complaint_type) %>%
      clearControls() %>%
      flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4], options = list(duration = .25))
  })

  output$map_legend <- renderUI({
    req(pal)
    req(dist_week)

    pal <- pal()

    text <- sort(unique(dist_week()$complaint_type))
    cols <- pal(text)

    make_label <- function(col, text) {
      paste0(tags$span(style = paste0("display:inline-block;background:", col,";height:1rem;width:1rem;margin-right:0.5rem;")),
             text)
    }

    out <- paste(map2_chr(cols, text, make_label), collapse = "<br>")

    p(HTML(out))

  })

}

