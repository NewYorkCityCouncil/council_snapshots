library(leaflet)
library(stringr)
library(purrr)

icon_func <- JS("function(cluster) {
		return L.divIcon({ html: '<span style=display:inline-block;background:#2F56A6;height:1rem;width:1rem;margin-right:0.5rem;color:#FFFFFF;>' + cluster.getChildCount() + '</span>' });
	}")

# Create module ui
example_311_ui <- function(id) {

  # Namespace for module
  ns <- NS(id)

  # Row to hold plots
  fluidRow(
    box(title = "Most common complaints", solidHeader = TRUE,
      plotOutput(ns("complaint_type_cd_week"))
    ),
    box(title = "Complaint locations", solidHeader = TRUE,
      leafletOutput(ns("complaint_map"))
      # uiOutput(ns("map_legend"))
    )
  )

}

# Create module server function
# Needs coun_dist and week global inputs (passed from callModule in main app)
example_311 <- function(input, output, session, coun_dist, week) {

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
  output$complaint_type_cd_week <- renderPlot({
    dist_week() %>%
      count(complaint_type) %>%
      # wrap labels for prettiness
      mutate(complaint_type = str_wrap(complaint_type, 15),
             complaint_type = reorder(complaint_type, n)) %>%
      ggplot(aes(complaint_type, n, fill = complaint_type)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Top complaints",
           x = "Complaint type",
           y = "Number of complaints") +
      councildown::scale_fill_nycc() +
      councildown::theme_nycc(print = FALSE)
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

  # observe(leafletProxy(...)) is the standard design pattern for
  # updating leaflet maps in response to user input
  observe({

    req(dist_week)
    pal <- pal()

    bbox <- as.numeric(st_bbox(dist_week()))



    leafletProxy("complaint_map", data = dist_week() %>% st_cast("POINT")) %>%
      clearGroup("complaints") %>%
      addCircleMarkers(radius = 4, stroke = FALSE, fillOpacity = .8,
                       fillColor = ~pal(complaint_type),
                       popup = ~ paste(complaint_type, incident_address, created_date, sep = "<br>"),
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

}

