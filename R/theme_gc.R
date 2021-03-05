#' Good Charts Theme
#'
#' @param ... All arguments to pass into the base ggplot theme
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
#'     geom_point() +
#'     theme_gc()
#' @aliases theme_good_charts
#' @export
#' @import ggplot2
theme_gc <- function(...) {
  ggplot2::theme_classic(...) %+replace% theme(
    axis.line.y = ggplot2::element_blank(),
    legend.position = "none"
  )
}
