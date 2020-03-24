#' Plot simple regression from a 'simplereg' object
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param x an object of class \code{simplereg}
#' @param labels character, labels for subjects
#' @param subset an optional vector specifying a subset of observations to be highlighted in the graph or \code{'quant'} to highligh observations above and below the upper and lower quantiles, respectively
#' @param Lx numeric; if \code{subset='quant'}, lower quantile for x (default = 0.01)
#' @param Ux numeric; if \code{subset='quant'}, upper quantile for x (default = 0.99)
#' @param Ly numeric; if \code{subset='quant'}, lower quantile of y (default = 0.01)
#' @param Uy numeric; if \code{subset='quant'}, upper quantile of y (default = 0.99)
#' @param title character, plot title
#' @param xtitle character, x-axis label
#' @param ytitle character, y-axis label
#' @param repel logical, if \code{TRUE} (the default) text labels repel away from each other
#' @param ... Other graphical parameters
#' @seealso \code{\link{simplereg}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @examples
#' Pbox.sel <- subset(Pbox, MIN >= 500)
#' X <- Pbox.sel$AST/Pbox.sel$MIN
#' Y <- Pbox.sel$TOV/Pbox.sel$MIN
#' Pl <- Pbox.sel$Player
#' mod <- simplereg(x=X, y=Y, type="lin")
#' plot(mod)
#' @method plot simplereg
#' @export
#' @importFrom stats coef
#' @importFrom stats quantile
#' @importFrom ggplot2 geom_abline

plot.simplereg <- function(x, labels = NULL, subset = NULL, Lx = 0.01, Ux = 0.99, Ly = 0.01,
    Uy = 0.99, title = "Simple regression", xtitle = NULL, ytitle = NULL, repel = TRUE, ...) {

  if (!is.simplereg(x)) {
    stop("Not a 'simplereg' object")
  }
  label <- y <- NULL
  xx <- x[["x"]]
  yy <- x[["y"]]
  type = x[["type"]]
  mod <- x[["Model"]]
  R2 <- x[["R2"]]
  R2text <- paste(round(R2, 4) * 100, "*'%'", sep = "")

  if (!is.null(subset)) {
    if (subset[1] == "quant") {
      subset <- which(xx < stats::quantile(xx, Lx) | xx > stats::quantile(xx, Ux) | yy <
                        stats::quantile(yy, Ly) | yy > stats::quantile(yy, Uy))
    }
  } else if (is.null(subset) & !is.null(labels)) {
    subset <- 1:length(xx)
  }
  if (is.null(labels)) {
    labels <- as.character(1:length(xx))
  }
  df1 <- data.frame(x = xx, y = yy, label = labels)
  df2 <- df1[subset, ]

  p <- ggplot(data = df1, aes(x = x, y = y)) + geom_point()


  if (repel) {
    p <- p + geom_text_repel(data = df2, aes(x = x, y = y, label = label), color = "blue")
  } else {
    p <- p + geom_text(data = df2, aes(x = x, y = y, label = label), color = "blue")
  }

  if (type == "lin") {
    b <- coef(mod)
    p <- p + geom_abline(slope = b[2], intercept = b[1], lwd = 1, color = "lightsteelblue4")
    if (b[2] > 0) {
      lbl <- paste("'Y =", round(b[1], 2), "+", round(b[2], 2), "X'*~~~~~~~~ R^2 ==",
                   R2text)
    } else {
      lbl <- paste("'Y =", round(b[1], 2), "-", round(abs(b[2]), 2), "X'*~~~~~~~~ R^2 ==",
                   R2text)
    }
  } else if (type == "pol") {
    idx <- order(xx)
    df3 <- data.frame(x = xx[idx], y = mod$fitted[idx])
    lbl <- paste0("R^2 == ", R2text)
    p <- p + geom_line(data = df3, aes(x = x, y = y), color = "olivedrab3", lwd = 1)
  } else if (type == "ks") {
    lbl <- paste0("R^2 == ", R2text)
    p <- p + geom_line(aes(x = mod$x, y = mod$y), color = "olivedrab3", lwd = 1)
  }

  p <- p + annotate(geom = "text", label = lbl, x = Inf, y = Inf, hjust = 1, vjust = -0.5, parse = TRUE) +
    coord_cartesian(clip = 'off') +
    labs(title = title, x = xtitle, y = ytitle) +
    theme_bw()

  return(p)
}
