#' Simple linear and nonparametric regression
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketball.analyzer.help@gmail.com})
#' @param x numerical vector, input x values
#' @param y numerical vector, input y values
#' @param type character, type of regression; available options are: \code{lin} (linear regression, the default), \code{pol} (local polynomial regression of degree 2), \code{ks} (nonparametric kernel smoothing)
#' @param sp numeric, parameter to control the degree of smoothing; span for local polynomial regression and bandwidth for ksmooth
#' @seealso \code{\link{loess}}, \code{\link{ksmooth}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return An object of class \code{simplereg}, i.e. a list with the following objects:
#' @return * \code{Model}, the output model (linear regression, local polynomial regression, or kernel smoothing)
#' @return * \code{R2}, (in-sample) coefficient of determination
#' @return * \code{x}, input x values
#' @return * \code{y}, input y values
#' @return * \code{type}, type of regression
#' @examples
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
