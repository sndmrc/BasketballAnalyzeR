#' Functions to check if an object is of class 'simplereg'.
#'
#' @param x any R object
#' @return returns TRUE if its argument is of class 'fourfactors' and FALSE otherwise
#' @examples
#' Pbox.sel <- subset(Pbox, MIN >= 500)
#' X <- Pbox.sel$AST/Pbox.sel$MIN
#' Y <- Pbox.sel$TOV/Pbox.sel$MIN
#' Pl <- Pbox.sel$Player
#' mod <- simplereg(x=X, y=Y, type="lin")
#' is.simplereg(mod)
#' @export

is.simplereg <- function(x) {
  inherits(x, "simplereg")
}
