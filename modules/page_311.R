library(leaflet)
library(stringr)
library(purrr)
library(plotly)
library(shinycssloaders)

# Create module ui
page_311_ui <- function(id, open_calls = TRUE) {

  # Namespace for module
  ns <- NS(id)

  fluidPage(
    # Row to hold plots
    h3("This week:"),
    fluidRow(
      box(title = "Top complaints", solidHeader = TRUE,
          plotlyOutput(ns("complaint_type_cd_week"), height = "420px") %>% withSpinner()
      ),
      box(title = "Complaint locations", solidHeader = TRUE,
          leafletOutput(ns("complaint_map")) %>% withSpinner(),
          actionLink(ns("reset_map"), "Reset map")
          # uiOutput(ns("map_legend"))
      )
    ),
    h3("Year to date:"),
    fluidRow(
      box(title = "Top complaints", solidHeader = TRUE,
          plotlyOutput(ns("complaint_type_cd_ytd")) %>% withSpinner()),
      box(title = "Number of complaints per week", solidHeader = TRUE,
          plotlyOutput(ns("complaint_num_cd_ytd")) %>% withSpinner())
    )
  )
}

# Create module server function
# Needs coun_dist and week global inputs (passed from callModule in main app)
page_311 <- function(input, output, session, coun_dist, week, open_calls = TRUE, current_week) {

  myTrigger <- makeReactiveTrigger()

  # Get the data for the selected district and week

  if (open_calls) {

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
  } else {
    dist_week <- reactive({

      # Make sure we have these before trying to use them
      req(coun_dist)
      req(week)

      # Use the local() function to tell dplyr I want to evaluate these
      # not pass them to the database as part of the query
      # then use collect_geo() because I'll be making a map
      tbl(snapshots_db, "sr_ind_top_10_week_district_closed") %>%
        filter(coun_dist == local(coun_dist()),
               week == local(week())) %>%
        collect_geo()
    })
  }
  # Create bar chart
  output$complaint_type_cd_week <- renderPlotly({
    p <- dist_week() %>%
      count(complaint_type) %>%
      # wrap labels for prettiness
      mutate(complaint_type = tools::toTitleCase(tolower(complaint_type)) %>%
               str_wrap(15) %>%
               reorder(n)) %>%
      ggplot(aes(complaint_type, n, fill = complaint_type,
                 text = paste(complaint_type, n, sep = "<br>"))) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "Complaint type",
           y = "Number of complaints") +
      councildown::scale_fill_nycc() +
      scale_x_discrete(labels = function(x) str_replace(x, "(^.*?\\n)(.*?)(\\n.*?)+$", "\\1\\2...")) +
      councildown::theme_nycc(print = FALSE)

    nycc_ggplotly(p, source = "311 complaint bar") %>%
      event_register('plotly_click')
  })

  # Create starting map that will be updated with leafletProxy()
  output$complaint_map <- renderLeaflet({
    # myTrigger$trigger()
    map_updater$resume()
    leaflet() %>%
      councildown::addCouncilStyle()
  })

  # The palette goes in a separate reactive so I can work with in multiple
  # places (e.g. to create the legend)
  pal <- reactive({
    req(dist_week)
    dat <- dist_week() %>%
      as.data.frame()
    pal <- colorFactor(councildown::nycc_pal()(length(unique(dat$complaint_type))),
                       reorder(count(dat, complaint_type)$complaint_type, count(dat, complaint_type)$n))

  })

  map_data <- reactive({
    req(dist_week)

    dist_week() %>%
      mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
      as.data.frame() %>%
      group_by(lon, lat, complaint_type) %>%
      summarize(n = n(), created_date = paste0(created_date, collapse = "<br>"),
                incident_address = paste0(unique(incident_address), collapse = "<br>")) %>%
      st_as_sf(coords = c("lon", "lat"), crs = st_crs(dist_week()))
  })


  # observe(leafletProxy(...)) is the standard design pattern for
  # updating leaflet maps in response to user input
  map_updater <- observe(suspended = TRUE, {

    # myTrigger$depend()
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

    # print(input$complaint_map_bounds)
  })

  selected_complaint_plotly <- reactive({
    event_data("plotly_click", source = "311 complaint bar")
  })

  last_click <- NULL
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

    last_click <<- s
  })

  observeEvent(input$reset_map, {
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
    last_click <<- NULL
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


  if (open_calls) {
    dist_ytd <- reactive({
      req(coun_dist)

      tbl(snapshots_db, "sr_top_10_ytd_district") %>%
        filter(coun_dist == local(coun_dist())) %>%
        select(-locations) %>%
        collect()
    })

  } else {
    dist_ytd <- reactive({
      req(coun_dist)

      tbl(snapshots_db, "sr_top_10_ytd_district_closed") %>%
        filter(coun_dist == local(coun_dist())) %>%
        select(-locations) %>%
        collect()
    })
  }

  output$complaint_type_cd_ytd <- renderPlotly({
    p <- dist_ytd() %>%
      mutate(complaint_type = tools::toTitleCase(tolower(complaint_type)) %>%
               str_wrap(15) %>%
               reorder(n_tot)) %>%
      ggplot(aes(complaint_type, n_tot, fill = as.numeric(complaint_type),
                 text = paste(complaint_type, n_tot, sep = "<br>"))) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_gradient(low = "#2F56A6", high = "#23417D") +
      scale_x_discrete(labels = function(x) str_replace(x, "(^.*?\\n)(.*?)(\\n.*?)+$", "\\1\\2...")) +
      labs(x = "Complaint type",
           y = "Number of complaints") +
      councildown::theme_nycc()

    nycc_ggplotly(p)
  })


  if (open_calls) {
    dist_all <- reactive({
      tbl(snapshots_db, "sr_week_district") %>%
        filter(coun_dist == local(coun_dist())) %>%
        select(-locations)
    })
  } else {
    dist_all <- reactive({
      tbl(snapshots_db, "sr_week_district_closed") %>%
        filter(coun_dist == local(coun_dist())) %>%
        select(-locations)
    })
  }


  output$complaint_num_cd_ytd <- renderPlotly({

    p <- dist_all() %>%
      filter(week <= local(current_week)) %>%
      group_by(week) %>%
      summarize(n = sum(n)) %>%
      collect() %>%
      mutate(date = floor_date(ymd("2019-01-01")+(7*(week-1)), unit = "week") + 1) %>%
      ggplot(aes(date, n,
                 text = paste0("Week of ", format(date, format = "%b %e"), ": ", n, " complaints"), group = 1, group = 1)) +
      geom_point(color = "#23417D") +
      geom_line(color = "#23417D") +
      labs(x = "Week",
           y = "Number of service requests") +
      councildown::theme_nycc()

    nycc_ggplotly(p)

  })


  onBookmark({
    function(state) {
      state$values$map_bounds <- input$complaint_map_bounds
      state$values$plotly_clicked <- last_click
      state$values$session_ns <- session$ns
    }
  })

  onRestored(function(state) {

    s <- state$values$plotly_clicked
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

    bounds <- state$values$complaint_map_bounds
    leafletProxy(session$ns("complaint_map")) %>%
      flyToBounds(bounds$west, bounds$south, bounds$east, bounds$north)
  })

}


