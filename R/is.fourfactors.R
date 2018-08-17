#' Functions to check if an object is of class 'fourfactors'.
#'
#' @param x any R object
#' @return returns TRUE if its argument is of class 'fourfactors' and FALSE otherwise
#' @examples
#' data("Tbox")
#' data("Pbox")
#' selectedTeams <- c(2,6,10,11)
#' FF <- fourfactors(Tbox, Obox, sel=selectedTeams)
#' is.fourfactors(FF)
#' @export


is.fourfactors <- function(x) {
    inherits(x, "fourfactors")
}
