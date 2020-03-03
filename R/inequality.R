#' Inequality analysis
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param data numeric vector
#' @param nplayers integer, number of players to include in the analysis
#' @return A list with the following elements: \code{Lorenz} (cumulative distribution used to plot the Lorenz curve) and \code{Gini} (Gini coefficient)
#' @seealso \code{\link{plot.inequality}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @examples
#' Pbox.BN <- subset(Pbox, Team=="Brooklyn Nets")
#' out <- inequality(Pbox.BN$PTS, nplayers=8)
#' print(out)
#' plot(out)
#' @export

inequality <- function(data, nplayers) {

  x <- stats::na.omit(data)
  x <- rev(sort(x, decreasing = T)[1:nplayers])

  xtot <- sum(x)
  xcum <- cbind((1:nplayers)/nplayers, cumsum(x)/xtot)
  xcum <- rbind(c(0, 0), xcum)
  xcummax <- xcum
  xcummax[1:nplayers, 2] <- 0

  gini <- round(100 * sum(xcum[, 1] - xcum[, 2])/sum(xcum[1:nplayers, 1]), 2)

  lor <- data.frame(xcum)
  names(lor) <- c("F", "Q")

  lst <- list(Gini = gini, Lorenz = lor)
  class(lst) <- append("inequality", class(lst))
  return(lst)
}
