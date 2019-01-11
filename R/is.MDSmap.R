#' Functions to check if an object is of class 'MDSmap'.
#'
#' @param x any R object
#' @return returns TRUE if its argument is of class 'MDSmap' and FALSE otherwise
#' @examples
#' data <- subset(Pbox, MIN >= 1500)
#' data <- data.frame(data$PTS, data$P3M, data$P2M, data$OREB + data$DREB, data$AST,
#'                    data$TOV,data$STL, data$BLK)
#'                    mds <- MDSmap(data)
#' is.MDSmap(mds)
#' @export

is.MDSmap <- function(x) {
  inherits(x, "MDSmap")
}
