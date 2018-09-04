#' Functions to check if an object is of class 'inequality'.
#'
#' @param x any R object
#' @return returns TRUE if its argument is of class 'inequality' and FALSE otherwise
#' @examples
#' Pbox.BN <- subset(Pbox, Team=="Brooklyn Nets")
#' out <- inequality(Pbox.BN$PTS, npl=8)
#' is.inequality(out)
#' @export

is.inequality <- function(x) {
  inherits(x, "inequality")
}
