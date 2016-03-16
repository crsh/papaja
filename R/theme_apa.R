#' APA compatible ggplot2 theme
#'
#' A ggplot2 theme with a white panel background, no grid lines, large axis and legend titles,
#' and increased text padding for better readability.
#'
#' @param base_size Numeric. Base font size; other font sizes and margins are adjusted relative
#'    to this.
#' @param base_family Character. Base font family.
#'
#' @details This theme is an adaptation of \link[ggplot2]{\code{theme_bw()}}. In ggplot2, themes set the
#'    general aspect of the plot such as the colour of the background, gridlines, the size and colour
#'    of fonts.
#'
#' @export
#'
#' @examples
#'
#' # Copied from ?ggtheme
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'      colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_gray()

theme_apa <- function(base_size = 18, base_family = "") {
  theme_bw(base_size, base_family) +
    theme(
      plot.title = element_text(margin = margin(0, 0, rel(14), 0))
      , axis.title = element_text(size = rel(1.2))
      , axis.title.x = element_text(margin = margin(rel(18), 0, 0, 0))
      , axis.title.y = element_text(margin = margin(0, rel(18), 0, 0))
      , axis.ticks.length = unit(rel(6), "points")
      , axis.text.x = element_text(margin = margin(rel(6), 0, 0, 0))
      , axis.text.y = element_text(margin = margin(0, rel(8), 0, 0))

      , legend.title = element_text()
      , legend.key = element_rect(fill = NA, color = NA)
      , legend.key.width = unit(rel(20), "points")
      , legend.key.height = unit(rel(25), "points")
      , legend.margin = unit(rel(18), "points")

      , panel.border = element_rect(color = "black")
      , panel.margin = unit(rel(16), "points")
      , panel.grid.major.x = element_line(size = NA)
      , panel.grid.minor.x = element_line(size = NA)
      , panel.grid.major.y = element_line(size = NA)
      , panel.grid.minor.y = element_line(size = NA)

      , strip.background = element_rect(fill = NA, color = NA)
      , strip.text.x = element_text(size = rel(1.1), margin = margin(0, 0, rel(16), 0))
      , strip.text.y = element_text(size = rel(1.1), margin = margin(0, 0, 0, rel(16)))
    )
}
