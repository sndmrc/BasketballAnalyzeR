#' Reports whether x is a 'simplereg' object
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param x an object to test.
#' @seealso \code{\link{simplereg}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return Returns TRUE if its argument is of class \code{simplereg} and FALSE otherwise.
#' @examples
#' Pbox.sel <- subset(Pbox, MIN >= 500)
#' X <- Pbox.sel$AST/Pbox.sel$MIN
#' Y <- Pbox.sel$TOV/Pbox.sel$MIN
#' Pl <- Pbox.sel$Player
#' out <- simplereg(x=X, y=Y, type="lin")
#' is.simplereg(out)
#' @export

is.simplereg <- function(x) {
  inherits(x, "simplereg")
}
