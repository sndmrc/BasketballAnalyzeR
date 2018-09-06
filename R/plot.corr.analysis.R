#' Plot inequality
#'
#' @param x A 'inequality' objects
#' @param title Plot title
#' @param ... other graphical parameters
#' @return A ggplot2 object
#' @examples
#' data <- data.frame(Pbox$PTS,Pbox$P3M,Pbox$P2M,
#'                    Pbox$OREB + Pbox$DREB,Pbox$AST,
#'                    Pbox$TOV,Pbox$STL,Pbox$BLK)/Pbox$MIN
#' names(data) <- c("PTS","P3M","P2M","REB","AST","TOV","STL","BLK")
#' data <- subset(data, Pbox$MIN >= 500)
#' out <- corr.analysis(data)
#' plot(out)
#' @export
#' @method plot corr.analysis
#' @importFrom ggplotify as.ggplot
#' @importFrom network network
#' @importFrom network set.edge.attribute
#' @importFrom network "%e%"
#' @importFrom GGally ggnet2
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics par
#' @importFrom graphics plot.new
#' @importFrom graphics plot.window
#' @importFrom graphics points
#' @importFrom graphics polygon
#' @importFrom graphics rect
#' @importFrom graphics segments
#' @importFrom graphics strwidth
#' @importFrom graphics symbols
#' @importFrom graphics text
#' @importFrom graphics plot
#' @importFrom corrplot corrMatOrder
#' @importFrom corrplot corrRect.hclust
#' @importFrom corrplot colorlegend


plot.corr.analysis <- function(x, title = NULL, ...) {

  cor_mtx <- x[["cor.mtx"]]
  cor_mtx_trunc <- x[["cor.mtx.trunc"]]
  cor_mtest <- x[["cor.mtest"]]
  sig.level <- x[["siglevel"]]

  cormtx <- cormtest <- siglev <- NULL
  cormtx <<- cor_mtx
  cormtest <<- cor_mtest
  siglev <<- sig.level
  p1 <- ggplotify::as.ggplot(~corr_plot_mixed(cormtx, cormtest, siglev))

  net <- network::network(cor_mtx_trunc, matrix.type = "adjacency", ignore.eval = FALSE, names.eval = "weights")
  network::set.edge.attribute(net, "edge.color", ifelse(net %e% "weights" > 0, "lightsteelblue", "tomato"))
  network::set.edge.attribute(net, "edge.size", 4 * abs(net %e% "weights"))
  p2 <- GGally::ggnet2(net, label = T, mode = "circle", node.color = "white", edge.size = "edge.size", edge.color = "edge.color")

  gridExtra::grid.arrange(p1, p2, nrow = 1)
  lst <- list(corrplot = p1, netplot = p2)
  invisible(lst)
}



