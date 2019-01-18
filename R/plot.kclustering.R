#' Plot k-means clustering of NBA teams
#'
#' @param x An object of class 'kclust'
#' @param title Plot title
#' @param ncol.arrange Number of columns when arranging multiple grobs on a page
#' @param min.mid.max A numerical vector with 3 elements: lower bound, middle dashed line, upper bound for radial axis
#' @param ... other graphical parameters
#' @return A list of ggplot2 radial plots
#' @examples
#' FF <- fourfactors(Tbox,Obox)
#' OD.Rtg <- FF$ORtg/FF$DRtg
#' F1.r <- FF$F1.D/FF$F1.O
#' F2.r <- FF$F2.O/FF$F2.D
#' F3.O <- FF$F3.D
#' F3.D <- FF$F3.O
#' P3M <- Tbox$P3M
#' STL.r <- Tbox$STL/Obox$STL
#' X <- data.frame(OD.Rtg,F1.r,F2.r,F3.O,F3.D,P3M,STL.r)
#' kclu1 <- kclustering(X)
#' plot(kclu1)
#' kclu2 <- kclustering(X, k=9)
#' plot(kclu2)
#' @method plot kclustering
#' @export


plot.kclustering <- function(x, title = NULL, ncol.arrange = NULL, min.mid.max = NULL, ...) {

  if (!is.kclustering(x)) {
    stop("Not a 'kclustering' object")
  }
  y <- label <- NULL
  k <- x[["k"]]
  if (is.null(k)) {
    varfra.nclu <- x[["VarianceBetween"]]
    nclumax <- max(x[["ClusterRange"]])
    df1 <- data.frame(x = 1:nclumax, y = varfra.nclu * 100)
    df2 <- data.frame(x = 3:nclumax, y = round((varfra.nclu[3:nclumax]/varfra.nclu[2:(nclumax - 1)] - 1) * 100, 2))
    df2$label <- paste("+", df2$y, "%", sep = "")
    df3 <- data.frame(x = 2:nclumax, y = round((varfra.nclu[2:nclumax]) * 100, 2))
    df3$label <- paste(df3$y, "%", sep = "")
    p <- ggplot(data = df1, aes(x, y)) +
      geom_line() +
      geom_point(size = 5, colour = "white") +
      geom_point(size = 2) +
      geom_line(data = df2, aes(x = x, y = y), linetype = 2) +
      geom_point(data = df2, aes(x = x, y = y), size = 6, colour = "white") +
      geom_point(data = df2, aes(x = x, y = y), shape = 0, size = 2) +
      ggrepel::geom_text_repel(data = df2, aes(x = x, y = y, label = label), vjust = +2) +
      ggrepel::geom_text_repel(data = df3, aes(x = x, y = y, label = label), vjust = +2) +
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
      xlab("Number of clusters") + ylab("BD/TD - Increments") +
      theme_bw()
    print(p)
  } else {
    profiles <- x[["Profiles"]]
    if (is.null(ncol.arrange)) {
      ncol.arrange <- ceiling(sqrt(nrow(profiles)))
    }
    if (is.null(title)) {
      title <- profiles$clustnames
    } else if (length(title)!=nrow(profiles)) {
      stop("The length of 'title' is not equal to the number of clusters")
    }
    pos.clst.nm <- which(names(profiles)=="clustnames")
    if (is.null(min.mid.max)) {
      mn <- min(profiles[, -pos.clst.nm])
      mx <- max(profiles[, -pos.clst.nm])
      ming <- -max(mn,mx)
      midg <- 0
      maxg <- max(mn,mx)
      min.mid.max <- c(ming,midg,maxg)
    }
    p <- radialprofile(data=profiles[,-pos.clst.nm], title=title, ncol.arrange=ncol.arrange, std=FALSE, min.mid.max=min.mid.max)
  }
  invisible(p)
}