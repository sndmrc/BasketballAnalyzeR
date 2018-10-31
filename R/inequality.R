#' Inequality analysis
#'
#' @param data A numerical vector
#' @param nplayers Number of players to include in the analysis
#' @return A list with the following elements:  Lorenz curve ("Lorenz") and Gini coefficient ("Gini") for a given number of players
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

  lor <- data.frame(xcum, xcummax[, 2])
  names(lor) <- c("F", "Q", "Qmax")

  lst <- list(Gini = gini, Lorenz = lor)
  class(lst) <- append("inequality", class(lst))
  return(lst)
}
