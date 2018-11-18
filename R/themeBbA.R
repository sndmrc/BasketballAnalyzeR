#' @noRd

themeBbA <- function(plot.bgcolor=NULL, legend.bgcolor=NULL) {
  if (!is.null(plot.bgcolor)) {
    plot.elem.bckgrnd <- element_rect(fill=plot.bgcolor)
  } else {
    plot.elem.bckgrnd <- element_blank()
  }
  if (!is.null(legend.bgcolor)) {
    legend.elem.bckgrnd <- element_rect(fill=legend.bgcolor, colour="transparent")
  } else {
    legend.elem.bckgrnd <- element_blank()
  }
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=plot.elem.bckgrnd,
        legend.key=legend.elem.bckgrnd)
}
