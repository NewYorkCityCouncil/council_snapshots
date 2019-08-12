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

makeReactiveTrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}


help_tooltip <- function(id, title, content) {
  tagList(
    span(style = "position:absolute;right:10px;bottom:10px",
      id = id,
      tags$i(class="fa fa-info-circle")),
    bsPopover(id, title, content, options = list(container = "body"))
  )
}
