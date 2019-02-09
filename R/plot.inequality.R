#' Plot inequality
#'
#' @param x A 'inequality' objects
#' @param title Plot title
#' @param ... other graphical parameters
#' @return A ggplot2 object
#' @examples
#' Pbox.BN <- subset(Pbox, Team=="Brooklyn Nets")
#' out <- inequality(Pbox.BN$PTS, nplayers=8)
#' print(out)
#' plot(out)
#' @method plot inequality
#' @export
#' @importFrom ggplot2 geom_ribbon

plot.inequality <- function(x, title = NULL, ...) {

  if (!is.inequality(x)) {
    stop("Not an object of class 'inequality'")
  }

  Q <- Qmax <- NULL
  if (is.null(title)) {
    title <- "Lorenz curve"
  }
  lor <- x[["Lorenz"]]
  gini <- x[["Gini"]]
  lor <- 100*lor

  lor$Qmax <- c(rep(0,nrow(lor)-1), max(lor$Q))
  p <- ggplot(data = lor, aes(F, Q)) +
       theme(panel.background = element_rect(fill = "transparent"), plot.title = element_text(size = 12)) +
       geom_ribbon(aes(x = F, ymax = F, ymin = Q), fill = "dodgerblue4", alpha = 1) +
       geom_line(aes(y = Q), col = "dodgerblue", lwd = 1.2) +
       geom_line(aes(y = Qmax), col = "dodgerblue", lwd = 1.2) +
       geom_line(aes(y = F), col = "dodgerblue", lwd = 1.2) +
       annotate("text", x = 25, y = 80, label = paste("Gini index = ", gini, "%", sep = ""), size = 5) +
       labs(title=title, x="", y="")

  return(p)
}
