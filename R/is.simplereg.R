#' Reports whether x is a simplereg object
#'
#' @param x An object to test
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
