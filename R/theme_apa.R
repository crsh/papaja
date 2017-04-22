#' APA compatible ggplot2 theme
#'
#' A ggplot2 theme with a white panel background, no grid lines, large axis and legend titles,
#' and increased text padding for better readability.
#'
#' @param base_size Numeric. Base font size; other font sizes and margins are adjusted relative
#'    to this.
#' @param base_family Character. Base font family.
#' @param box Logical. Indicates whether to draw a box around the plot.
#'
#' @details This theme is an adaptation of \code{\link[ggplot2]{theme_bw}}. In ggplot2, themes set the
#'    general aspect of the plot such as the colour of the background, gridlines, the size and colour
#'    of fonts.
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'    # Copied from ?ggtheme
#'    p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'      colour = factor(gear))) + facet_wrap(~ am)
#'    p
#'    p + theme_apa()
#'  }

theme_apa <- function(base_size = 12, base_family = "", box = FALSE) {
  adapted_theme <- ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.1), margin = ggplot2::margin(0, 0, ggplot2::rel(14), 0))

      # , axis.title = ggplot2::element_text(size = ggplot2::rel(1.1))
      , axis.title.x = ggplot2::element_text(size = ggplot2::rel(1), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(ggplot2::rel(12), 0, 0, 0))
      , axis.title.y = ggplot2::element_text(size = ggplot2::rel(1), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(0, ggplot2::rel(12), 0, 0))
      , axis.ticks.length = ggplot2::unit(ggplot2::rel(6), "points")
      , axis.text = ggplot2::element_text(size = ggplot2::rel(0.9))
      , axis.text.x = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(ggplot2::rel(6), 0, 0, 0))
      , axis.text.y = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(0, ggplot2::rel(6), 0, 0))
      , axis.line.x = ggplot2::element_line()
      , axis.line.y = ggplot2::element_line()

      , legend.title = ggplot2::element_text()
      , legend.key = ggplot2::element_rect(fill = NA, color = NA)
      , legend.key.width = ggplot2::unit(ggplot2::rel(20), "points")
      , legend.key.height = ggplot2::unit(ggplot2::rel(20), "points")
      , legend.margin = ggplot2::margin(
        t = ggplot2::rel(16)
        , r = ggplot2::rel(16)
        , b = ggplot2::rel(16)
        , l = ggplot2::rel(16)
        , unit = "points"
      )

      , panel.spacing = ggplot2::unit(ggplot2::rel(14), "points")
      , panel.grid.major.x = ggplot2::element_line(size = NA)
      , panel.grid.minor.x = ggplot2::element_line(size = NA)
      , panel.grid.major.y = ggplot2::element_line(size = NA)
      , panel.grid.minor.y = ggplot2::element_line(size = NA)

      , strip.background = ggplot2::element_rect(fill = NA, color = NA)
      , strip.text.x = ggplot2::element_text(margin = ggplot2::margin(0, 0, ggplot2::rel(16), 0)) # size = ggplot2::rel(1.1),
      , strip.text.y = ggplot2::element_text(margin = ggplot2::margin(0, 0, 0, ggplot2::rel(16))) # size = ggplot2::rel(1.1),
    )

  if(box) {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_rect(color = "black"))
  } else {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_blank())
  }

  adapted_theme
}
