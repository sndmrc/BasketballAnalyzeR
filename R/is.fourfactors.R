#' Reports whether x is a fourfactors object
#'
#' @param x An object to test
#' @return returns TRUE if its argument is of class 'fourfactors' and FALSE otherwise
#' @examples
#' data("Tbox")
#' data("Pbox")
#' selectedTeams <- c(2,6,10,11)
#' out <- fourfactors(Tbox, Obox, sel=selectedTeams)
#' is.fourfactors(out)
#' @export

is.fourfactors <- function(x) {
    inherits(x, "fourfactors")
}
