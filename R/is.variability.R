#' Functions to check if an object is of class 'variability'.
#'
#' @param x any R object
#' @return returns TRUE if its argument is of class 'variability' and FALSE otherwise
#' @examples
#' Pbox.BC <- subset(Pbox, Team=="Oklahoma City Thunder" & MIN >= 500,
#'                     select=c("P2p","P3p","FTp","P2A","P3A","FTA"))
#' out <- variability(data=Pbox.BC, data.var=c("P2p","P3p","FTp"),
#'                    size.var=c("P2A","P3A","FTA"), weight=TRUE)
#' is.variability(out)
#' @export


is.variability <- function(x) {
  inherits(x, "variability")
}
