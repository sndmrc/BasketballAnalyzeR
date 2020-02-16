#' Reports whether x is a fourfactors object
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketball.analyzer.help@gmail.com})
#' @param x an object to test
#' @seealso \code{\link{fourfactors}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return returns TRUE if its argument is of class \code{fourfactors} and FALSE otherwise
#' @examples
#' selTeams <- c(2,6,10,11)
#' out <- fourfactors(Tbox[selTeams,], Obox[selTeams,])
#' is.fourfactors(out)
#' @export

is.fourfactors <- function(x) {
    inherits(x, "fourfactors")
}
