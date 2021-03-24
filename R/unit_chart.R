#' Good Charts Inspired Unit Chart
#'
#' @param n number of items to be colored
#' @param out_of Total number of units
#' @param color_random_points (optional) Highlight random points
#' @param highlight_color (optional) The highlight point color
#' @param other_color (optional) The color for all the other points
#' @examples
#' # Create the basketball chart on page 145
#' unit_chart(3, 10000, TRUE)
#' @export
#' @import ggplot2
unit_chart <- function(n, out_of, color_random_points = FALSE, highlight_color = "#E41A1C", other_color = "#DFDFDF") {
  square <- floor(sqrt(out_of))
  x <- c()
  for (i in seq(1, square)) {
    x <- c(x, rep(1, square) * i)
  }
  data <- data.frame(x = x, y = seq(1, square), n = seq(1, square * square))
  # Add in the extra data
  current_n <- max(data$n) + 1
  next_x <- max(data$x) + 1
  next_y <- 1
  while (current_n < out_of + 1) {
    row <- data.frame(x = next_x, y = next_y, n = current_n)
    data <- rbind(data, row)

    next_y <- next_y + 1
    if (next_y > square) {
      next_y <- 1
      next_x <- next_x + 1
    }
    current_n <- current_n + 1
  }
  data <- data[1:out_of, ]
  # Add in the color flag
  if (color_random_points) {
    data <- data[sample(nrow(data)), ]
  } else {
    data <- data[with(data, order(y, x)), ]
  }
  data$color_me <- c(rep("A", n), rep("B", out_of - n))

  ggplot2::ggplot(data, aes(x, y, color = as.factor(color_me))) +
    geom_point(size = 1) +
    scale_color_manual(values = c("A" = highlight_color, "B" = other_color)) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none")
}
