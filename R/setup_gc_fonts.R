#' Setup fonts for Good Charts Theme
#'
#' @param import_fonts boolean if the fonts should be imported
#' @export
#' @import extrafont
setup_gc_fonts <- function(import_fonts = FALSE) {
  if(import_fonts) {
    extrafont::font_import()
  }
  extrafont::loadfonts()
  extrafont::loadfonts(device = "win")
}
