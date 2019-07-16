library(leaflet)
library(stringr)
library(purrr)
library(plotly)

icon_func <- JS("function(cluster) {
		return L.divIcon({ html: '<span style=display:inline-block;background:#2F56A6;height:1rem;width:1rem;margin-right:0.5rem;color:#FFFFFF;>' + cluster.getChildCount() + '</span>' });
	}")

nycc_ggplotly <- function(p, toolbar = FALSE, zoom = FALSE, legend = FALSE, ...) {
  stopifnot(inherits(p, "gg"))

  if(is.null(p$mapping$text)) warning("Missing `text` aesthetic, tooltips will not be rendered.")

  p <- plotly::ggplotly(p, tooltip = "text", ...) %>%
    config(displayModeBar = toolbar) %>%
    layout(margin = list(l = 80))

  if (!zoom) {
    p <- p %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE))
  }

  if (!legend) {
    p <- hide_legend(p)
  }

  p
}

# Create module ui
opened_311_ui <- function(id) {

  # Namespace for module
  ns <- NS(id)

  fluidPage(
    # Row to hold plots
    h3("This week:"),
    fluidRow(
      box(title = "Most common complaints", solidHeader = TRUE,
          plotlyOutput(ns("complaint_type_cd_week"))
      ),
      box(title = "Complaint locations", solidHeader = TRUE,
          leafletOutput(ns("complaint_map")),
          actionLink(ns("reset_map"), "Reset map")
          # uiOutput(ns("map_legend"))
      )
    ),
    h3("Year to date:"),
    fluidRow(
      box(title = "Most common complaints", solidHeader = TRUE,
          plotlyOutput(ns("complaint_type_cd_ytd"))),
      box(title = "Number of complaints", solidHeader = TRUE,
          plotlyOutput(ns("complaint_num_cd_ytd")))
    )
  )
}

# Create module server function
# Needs coun_dist and week global inputs (passed from callModule in main app)
opened_311 <- function(input, output, session, coun_dist, week) {

  # Get the data for the selected district and week
  dist_week <- reactive({

    # Make sure we have these before trying to use them
    req(coun_dist)
    req(week)

    # Use the local() function to tell dplyr I want to evaluate these
    # not pass them to the database as part of the query
    # then use collect_geo() because I'll be making a map
    tbl(snapshots_db, "sr_ind_top_10_week_district") %>%
      filter(coun_dist == local(coun_dist()),
             week == local(week())) %>%
      collect_geo()
  })

  # Create bar chart
  output$complaint_type_cd_week <- renderPlotly({
    p <- dist_week() %>%
      count(complaint_type) %>%
      # wrap labels for prettiness
      mutate(complaint_type = str_wrap(complaint_type, 15),
             complaint_type = reorder(complaint_type, n)) %>%
      ggplot(aes(complaint_type, n, fill = complaint_type,
                 text = paste(complaint_type, n, sep = "<br>"))) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Top complaints",
           x = "Complaint type",
           y = "Number of complaints") +
      councildown::scale_fill_nycc() +
      scale_x_discrete(labels = function(x) str_replace(x, "(^.*?\\n)(.*?)(\\n.*?$)", "\\1\\2...")) +
      councildown::theme_nycc(print = FALSE)

    nycc_ggplotly(p, source = "311 complaint bar")
  })

  # Create starting map that will be updated with leafletProxy()
  output$complaint_map <- renderLeaflet({
    leaflet() %>%
      councildown::addCouncilStyle()
  })

  # The palette goes in a separate reactive so I can work with in multiple
  # places (e.g. to create the legend)
  pal <- reactive({
    req(dist_week)
    pal <- colorFactor(councildown::nycc_pal()(length(unique(dist_week()$complaint_type))),
                       reorder(count(dist_week(), complaint_type)$complaint_type, count(dist_week(), complaint_type)$n))

  })

  map_data <- reactive({
    req(dist_week)

    dist_week() %>%
      group_by(lat_lon = paste(st_coordinates(.)[,1], st_coordinates(.)[,2]),
               complaint_type) %>%
      summarize(n = n(), created_date = paste(created_date, collapse = "<br>"),
                incident_address = paste(unique(incident_address), collapse = "<br>"))
  })


  # observe(leafletProxy(...)) is the standard design pattern for
  # updating leaflet maps in response to user input
  observe({

    req(dist_week)
    pal <- pal()

    bbox <- as.numeric(st_bbox(dist_week()))

    leafletProxy("complaint_map", data = map_data()) %>%
      clearGroup("complaints") %>%
      addCircleMarkers(radius = ~4*sqrt(vapply(n, min, FUN.VALUE = numeric(1), 20)), weight = 15, fillOpacity = .8, opacity = 0,
                       fillColor = ~pal(complaint_type),
                       popup = ~ paste(complaint_type, n, incident_address, created_date, sep = "<br>"),
                       popupOptions = popupOptions(maxHeight = 100),
                       group = "complaints") %>%
      clearControls() %>%
      flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4], options = list(duration = .25))
  })

  observeEvent(event_data("plotly_click", source = "311 complaint bar"), {
    s <- event_data("plotly_click", source = "311 complaint bar")
    pal <- pal()

    if(length(s)) {
      clicked_level <- dist_week() %>%
        count(complaint_type) %>%
        mutate(complaint_type = reorder(complaint_type, n)) %>%
        pull(complaint_type) %>%
        levels() %>%
        .[s[["y"]]]

      to_map <- map_data() %>%
        filter(complaint_type == clicked_level)

      leafletProxy("complaint_map", data = to_map) %>%
        clearGroup("complaints") %>%
        addCircleMarkers(radius = ~4*sqrt(vapply(n, min, FUN.VALUE = numeric(1), 20)), weight = 15, fillOpacity = .8, opacity = 0,
                         fillColor = ~pal(complaint_type),
                         popup = ~ paste(complaint_type, n, incident_address, created_date, sep = "<br>"),
                         popupOptions = popupOptions(maxHeight = 100),
                         group = "complaints")
    }
  })

  observeEvent(input$reset_map, {
    req(dist_week)
    pal <- pal()

    bbox <- as.numeric(st_bbox(dist_week()))

    leafletProxy("complaint_map", data = dist_week()) %>%
      clearGroup("complaints") %>%
      addCircleMarkers(radius = ~4*sqrt(vapply(n, min, FUN.VALUE = numeric(1), 20)), weight = 15, fillOpacity = .8, opacity = 0,
                       fillColor = ~pal(complaint_type),
                       popup = ~ paste(complaint_type, n, incident_address, created_date, sep = "<br>"),
                       popupOptions = popupOptions(maxHeight = 100),
                       group = "complaints") %>%
      clearControls() %>%
      flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4], options = list(duration = .25))
  })

  # renderUI creates arbitrary HTML content that will be shown by the
  # corresponding uiOutput
  # output$map_legend <- renderUI({
  #   req(pal)
  #   req(dist_week)
  #
  #   pal <- pal()
  #
  #   # Make paired text color vectors
  #   text <- sort(unique(dist_week()$complaint_type))
  #   cols <- pal(text)
  #
  #   # A function to make a square of color with a label next to it
  #   make_label <- function(col, text) {
  #     paste0(tags$span(style = paste0("display:inline-block;background:", col,";height:1rem;width:1rem;margin-right:0.5rem;")),
  #            text)
  #   }
  #
  #   # Make a string of colors and labels
  #   out <- paste(map2_chr(cols, text, make_label), collapse = "<br>")
  #
  #   # Return the string as HTML to be rendered inside <p> tags
  #   p(HTML(out))
  #
  # })

  dist_ytd <- reactive({
    req(coun_dist)

    tbl(snapshots_db, "sr_top_10_ytd_district") %>%
      filter(coun_dist == local(coun_dist())) %>%
      select(-locations) %>%
      collect()
  })

  output$complaint_type_cd_ytd <- renderPlotly({
    p <- dist_ytd() %>%
      mutate(complaint_type = str_wrap(complaint_type, 15),
             complaint_type = reorder(complaint_type, n_tot)) %>%
      ggplot(aes(complaint_type, n_tot, fill = as.numeric(complaint_type),
                 text = paste(complaint_type, n_tot, sep = "<br>"))) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_gradient(low = "#2F56A6", high = "#23417D") +
      scale_x_discrete(labels = function(x) str_replace(x, "(^.*?\\n)(.*?)(\\n.*?$)", "\\1\\2...")) +
      labs(title = "Top complaints (YTD)",
           x = "Complaint type",
           y = "Number of complaints") +
      councildown::theme_nycc()

    nycc_ggplotly(p)
  })


  dist_all <- reactive({
    tbl(snapshots_db, "sr_week_district") %>%
      filter(coun_dist == local(coun_dist())) %>%
      select(-locations)
  })

  output$complaint_num_cd_ytd <- renderPlotly({

    p <- dist_all() %>%
      group_by(week) %>%
      summarize(n = sum(n)) %>%
      collect() %>%
      ggplot(aes(week, n, text = paste0("Week ", week, ": ", n, " complaints"), group = 1, group = 1)) +
      geom_point(color = "#23417D") +
      geom_line() +
      labs(title = "Service requests per week",
           x = "Week",
           y = "Number of service requests") +
    councildown::theme_nycc()

    nycc_ggplotly(p)

  })
}
