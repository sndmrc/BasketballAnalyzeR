#' Reports whether x is a fourfactors object
#'
#' @param x An object to test
#' @return returns TRUE if its argument is of class 'fourfactors' and FALSE otherwise
#' @examples
#' selTeams <- c(2,6,10,11)
#' out <- fourfactors(Tbox[selTeams,], Obox[selTeams,])
#' is.fourfactors(out)
#' @export

is.fourfactors <- function(x) {
    inherits(x, "fourfactors")
}
