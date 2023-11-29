
#' Sets airo theme
#' @import ggplot2
#' @return nothing
#' @export
#'
#' @examples airo_theme()
airo_theme <- function(){
##### load default theme for ggplot and vector for colors from darjeeling1 ####
theme_set(theme_classic(base_size = 12) %+replace% theme(
  # axis.line.y = element_line(colour = "black", size = 0.2),
  # axis.line.x = element_line(colour = "black", size = 0.2),
  axis.ticks   = element_line(colour = "black", size = 0.3),
  panel.background = element_rect(size = 0.3, fill = 'grey97'),
  axis.line    = element_blank(),
  legend.background = element_blank(),
  plot.title   = element_text(size = 12, vjust = 2, hjust = 0.5),
  strip.text   = element_text(size = 12),
  strip.background = element_blank()))
}

