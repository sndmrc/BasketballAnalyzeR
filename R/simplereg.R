#' Simple regression
#'
#' @param x data for x-axis
#' @param y data for y-axis
#' @param type type of regression: linear ('lin'), polynomial local fitting ('pol'), kernel smoothing ('ks')
#' @param sp parameter to control the degree of smoothing: span for loess and bandwidth for ksmooth (optional)
#' @example
#' Pbox.sel <- subset(Pbox, MIN >= 500)
#' X <- Pbox.sel$AST/Pbox.sel$MIN
#' Y <- Pbox.sel$TOV/Pbox.sel$MIN
#' Pl <- Pbox.sel$Player
#' mod <- simplereg(x=X, y=Y, type="lin")
#' @export
#' @importFrom stats ksmooth
#' @importFrom stats var
#' @importFrom stats lm

simplereg <- function(x, y, type = "lin", sp = NULL) {

  lin <- function(x, y) {
    mod <- stats::lm(y ~ x)
    b <- mod$coefficients
    R2 <- cor(x, y)^2
    return(list(Model = mod, R2 = R2, x = x, y = y, type = type))
  }

  pol <- function(x, y) {
    if (is.null(sp)) {
      mod <- loess(y ~ x, control = loess.control(surface = "direct"), degree = 2)
    } else {
      mod <- loess(y ~ x, control = loess.control(surface = "direct"), degree = 2,
                   span = sp)
    }
    R2 <- 1 - stats::var(mod$residuals)/var(y)
    return(list(Model = mod, R2 = R2, x = x, y = y, type = type))
  }

  ks <- function(x, y) {
    if (is.null(sp)) {
      mod <- stats::ksmooth(x, y, kernel = "normal", x.points = x)
    } else {
      mod <- stats::ksmooth(x, y, kernel = "normal", x.points = x, bandwidth = sp)
    }
    ord <- order(x)
    yoss <- y[ord]
    res <- yoss - mod$y
    R2 <- 1 - stats::var(res)/var(y)
    return(list(Model = mod, R2 = R2, x = x, y = y, type = type))
  }

  lst <- switch(type, lin = lin(x, y), pol = pol(x, y), ks <- ks(x, y))

  class(lst) <- append("simplereg", class(lst))
  return(lst)

}
