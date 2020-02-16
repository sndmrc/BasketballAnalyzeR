#' Functions to check if an object is of class 'MDSmap'.
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketball.analyzer.help@gmail.com})
#' @param x an object to test
#' @seealso \code{\link{MDSmap}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return returns TRUE if its argument is of class \code{MDSmap} and FALSE otherwise
#' @examples
#' data <- subset(Pbox, MIN >= 1500)
#' data <- data.frame(data$PTS, data$P3M, data$P2M, data$OREB + data$DREB, data$AST,
#'                    data$TOV,data$STL, data$BLK)
#' mds <- MDSmap(data)
#' is.MDSmap(mds)
#' @export

is.MDSmap <- function(x) {
  inherits(x, "MDSmap")
}
