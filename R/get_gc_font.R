#' Get a Good Charts style font
#'
#' @param chart_element The best font for the chart element
#' @export
#' @import extrafont
#' @import grDevices
get_gc_font <- function(chart_element = NA) {
  if(is.na(chart_element)) {
    errorCondition("You must specify what type of font you want.  Valid options are base and title.")
  }
  if (chart_element == "title") {
    gc_fonts <- c("PT Sans Narrow", "Arial Black", "Gill Sans MT Condensed", "Helvetica", "TT Arial", "Arial")
  } else {
    gc_fonts <- c("Proxima Nova", "Roboto", "Segoe UI", "Helvetica", "TT Arial", "Arial")
  }
  if(nzchar(system.file(package = "extrafont"))) {
    my_fonts <- extrafont::fonts()
    matches <- gc_fonts %in% my_fonts
    gc_font <- gc_fonts[matches][1]
  }
  else if (exists("X11Fonts")) {
    gc_font <- grDevices::X11Fonts()$sans[1]
  }
  else if (exists("windowsFonts")) {
    gc_font <- grDevices::windowsFonts()$sans[1]
  }
  else {
    gc_font <- "TT Arial"
  }
  return(gc_font)
}
