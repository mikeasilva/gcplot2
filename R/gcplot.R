#' Convert a ggplot into a Good Charts themed plot
#'
#' @param plot A ggplot object
#' @param title (optional) The plot's title
#' @param subtitle (optional) The plot's subtitle
#' @param ylab (optional) The y-axis label
#' @param xlab (optional) The x-axis label
#' @param caption (optional) The caption or source line
#' @examples
#'\dontrun{
#' library(ggplot2)
#'
#' p1 <- ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
#'   geom_point() +
#'   labs(
#'     title = "A ROSE BY ANY OTHER NAME",
#'     subtitle = "IRISES PETAL ATTRIBUTE",
#'     caption = "SOURCE: R.A. FISHER",
#'     x = "PETAL LENGTH",
#'     y = "PETAL WIDTH"
#'   )
#' gcplot(p1)
#'}
#' @export
#' @import grid
gcplot <- function(plot,
                   title = NA,
                   subtitle = NA,
                   ylab = NA,
                   xlab = NA,
                   caption = NA) {
  #heights
  heights <- list(
    "title" = 0,
    "subtitle" = 0,
    "ylab" = 0,
    "xlab" = 0,
    "plot" = 0,
    "caption" = 0
  )
  # Chart Title
  if (nchar(plot$labels$title) > 0 & is.na((title))) {
    title <- plot$labels$title
  }
  if (!is.na(title)) {
    title <- grid::textGrob(
      title,
      gp = grid::gpar(
        fontsize = 26,
        fontface = "bold"
        # fontfamily = "PT Sans Narrow"),
      ),
      just = c("left", "bottom")
    )
    heights$title <- grid::convertUnit(grid::grobHeight(title), "mm")
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
        fontface = "bold"
        # fontfamily = "PT Sans Narrow"),
      ),
      just = c("left", "bottom")
    )
    heights$title <- grid::convertUnit(grid::grobHeight(subtitle), "mm")
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
        col = "#000000"
        # fontfamily = "Proxima Nova"),
      ),
      #x = unit(TEXT_GROB_X_OFFSET, "npc"),
      just = c("left", "bottom")
    )
    heights$ylab <- grid::convertUnit(grid::grobHeight(ylab), "mm")
  }
  # Chart X-Axis Label
  if (nchar(plot$labels$x) > 0 & is.na(xlab)) {
    xlab <- plot$labels$x
  }
  if (!is.na(xlab)) {
    xlab <- grid::textGrob(
      xlab,
      gp = grid::gpar(
        fontsize = 15,
        col = "#000000"
        # fontfamily = "Proxima Nova"),
      ),
      #x = unit(TEXT_GROB_X_OFFSET, "npc"),
      just = c("left", "bottom")
    )
    heights$xlab <- grid::convertUnit(grid::grobHeight(xlab), "mm")
  }
  # Chart Caption
  if (nchar(plot$labels$caption) > 0 & is.na((caption))) {
    caption <- plot$labels$caption
  }
  if (!is.na(caption)) {
    caption <- grid::textGrob(
      caption,
      gp = grid::gpar(
        fontsize = 14,
        col = "#444444"
        # fontfamily = "Proxima Nova"),
      ),
      #x = unit(TEXT_GROB_X_OFFSET, "npc"),
      just = c("left", "bottom")
    )
    heights$caption <- grid::convertUnit(grid::grobHeight(caption), "mm")
  }
  # Apply the good charts base theme
  plot <- plot + gcplot2::theme_gc()
  # 12% title; 8% subtitle; 75% field; 5% source
  return(plot)
}
