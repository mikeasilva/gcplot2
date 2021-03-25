#' Convert a ggplot into a Good Charts themed plot
#'
#' @param plot A ggplot object
#' @param title (optional) The plot's title
#' @param subtitle (optional) The plot's subtitle
#' @param ylab (optional) The y-axis label
#' @param caption (optional) The caption or source line
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' p1 <- ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
#'   geom_point() +
#'   labs(
#'     title = "IRIS PETALS TELL YOU THE SPECIES",
#'     subtitle = "SETOSA IS THE SMALLEST AND VIRGINICA THE LARGEST",
#'     caption = "SOURCE: R.A. FISHER",
#'     x = "PETAL LENGTH",
#'     y = "PETAL WIDTH"
#'   )
#' gcplot(p1)
#' }
#' @export
#' @import grid
#' @import gridExtra
gcplot <- function(plot,
                   title = NA,
                   subtitle = NA,
                   ylab = NA,
                   caption = NA) {
  text_grob_x <- unit(0.008, "npc")
  # heights
  heights <- c(0, 0, 0, 0.6, 0)
  names(heights) <- c("title", "subtitle", "ylab", "plot", "caption")
  # Chart Title
  if (nchar(plot$labels$title) > 0 & is.na((title))) {
    title <- plot$labels$title
  }
  if (!is.na(title)) {
    title <- grid::textGrob(
      title,
      gp = grid::gpar(
        fontsize = 26,
        fontfamily = gcplot2::get_gc_font("title"),
        fontface = "bold"
      ),
      x = text_grob_x,
      just = c("left", "center")
    )
    heights["title"] <- 0.1
  }
  # Chart Subtitle
  if (nchar(plot$labels$subtitle) > 0 & is.na((subtitle))) {
    subtitle <- plot$labels$subtitle
  }
  if (!is.na(subtitle)) {
    subtitle <- grid::textGrob(
      subtitle,
      gp = grid::gpar(
        fontsize = 22,
        fontfamily = gcplot2::get_gc_font("title"),
        fontface = "bold"
      ),
      x = text_grob_x,
      just = c("left", "bottom")
    )
    heights["subtitle"] <- 0.08
  }
  # Chart Y-Axis Label
  if (nchar(plot$labels$y) > 0 & is.na(ylab)) {
    ylab <- plot$labels$y
  }
  if (!is.na(ylab)) {
    ylab <- grid::textGrob(
      ylab,
      gp = grid::gpar(
        fontsize = 15,
        fontfamily = gcplot2::get_gc_font("base"),
        col = "#000000"
      ),
      x = text_grob_x,
      just = c("left", "bottom")
    )
    heights["ylab"] <- 0.03
    # heights["plot"] <- heights["plot"] - heights["ylab"]
  }
  # Chart Caption
  if (nchar(plot$labels$caption) > 0 & is.na((caption))) {
    caption <- plot$labels$caption
  }
  if (!is.na(caption)) {
    caption <- grid::textGrob(
      caption,
      gp = grid::gpar(
        fontsize = 10,
        fontfamily = gcplot2::get_gc_font("base"),
        col = "#444444"#,
        #fontface = "bold"
      ),
      x = text_grob_x,
      just = c("left", "bottom")
    )
    heights["caption"] <- 0.05
  }
  # Apply the good charts base theme
  plot <- plot + gcplot2::theme_gc()
  # 12% title; 8% subtitle; 75% field; 5% source
  gridExtra::grid.arrange(title, subtitle, ylab, plot, caption, ncol = 1, heights = heights)
  # return(heights)
  # return(plot)
}
