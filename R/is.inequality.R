#' Reports whether x is a 'inequality' object.
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketball.analyzer.help@gmail.com})
#' @param x an object to test.
#' @seealso \code{\link{inequality}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return Returns TRUE if its argument is of class \code{inequality} and FALSE otherwise.
#' @examples
#' Pbox.BN <- subset(Pbox, Team=="Brooklyn Nets")
#' out <- inequality(Pbox.BN$PTS, npl=8)
#' is.inequality(out)
#' @export

is.inequality <- function(x) {
  inherits(x, "inequality")
}
