#' nycc_ggplotly
#'
#' Create plotly plots that meet NYCC guidelines
#'
#' @param p a ggplot
#' @param toolbar include toolbar?
#' @param zoom allow zooming?
#' @param legend show legend?
#' @param ... further arguments passed to \code{ggplotly()}
#'
#' @return a plotly htmlwidget
#' @export
#'
#'
#' @importFrom plotly ggplotly config layout hide_legend
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

#' makeReactiveTrigger
#'
#' Manually tie together reactive expressions and observers
#'
#' @return a list containing functions \code{depend()} and \code{trigger()}
#' @export
#'
#'
#' @importFrom shiny reactiveValues isolate
#' @importFrom utils head

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
