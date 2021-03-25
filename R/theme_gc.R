#' Good Charts Theme
#'
#' @param ... All arguments to pass into the base ggplot theme
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
#'   geom_point() +
#'   theme_gc()
#' }
#' @aliases theme_good_charts
#' @export
#' @import ggplot2
theme_gc <- function(...) {
  ggplot2::theme_classic(...) %+replace% theme(
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    plot.caption = element_blank(),
    axis.line.y = ggplot2::element_blank(),
    axis.title.x = element_text(size = 15, family = gcplot2::get_gc_font("base")),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(margin = margin(10, 0, 0, 0), family = gcplot2::get_gc_font("base")),
    axis.text.y = ggplot2::element_text(margin = margin(0, 15, 0, 0), family = gcplot2::get_gc_font("base")),
    axis.ticks.length = ggplot2::unit(0.3, "cm"),
    # This is a bit of a hack.  This removes the lowest y-axis tick.  I
    # doubt that there will be this many tick marks in a "good chart".
    axis.ticks.y = element_line(
      color = c(
        "transparent", "black", "black", "black", "black", "black",
        "black", "black", "black", "black", "black", "black",
        "black", "black", "black", "black", "black", "black",
        "black", "black", "black", "black", "black", "black",
        "black", "black", "black", "black", "black", "black"
      )
    ),
    axis.text = ggplot2::element_text(size = 15, color = "#000000", family = gcplot2::get_gc_font("base")),
    legend.position = "none"
  )
}
