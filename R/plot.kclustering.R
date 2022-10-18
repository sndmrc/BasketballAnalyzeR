#' Plot k-means clustering from a 'kclustering' object
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param x an object of class \code{kclustering}.
#' @param title character or vector of characters (when plotting radial plots of cluster profiles; see Value), plot title(s).
#' @param ncol.arrange integer, number of columns when arranging multiple grobs on a page (active when plotting radial plots of cluster profiles; see Value).
#' @param min.mid.max numeric vector with 3 elements: lower bound, middle dashed line, upper bound for radial axis (active when plotting radial plots of cluster profiles; see Value).
#' @param label.size numeric; label font size (default 2.5).
#' @param ... other graphical parameters.
#' @seealso \code{\link{kclustering}}, \code{\link{radialprofile}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return If \code{x$k} is \code{NULL}, \code{plot.kclustering} returns a single \code{ggplot2} object, displaying the pattern of the explained variance vs the number of clusters.
#' @return If \code{x$k} is not \code{NULL}, \code{plot.kclustering} returns a list of \code{ggplot2} objects, displaying the radial plots of the cluster profiles.
#' @examples
#' FF <- fourfactors(Tbox,Obox)
#' X <- with(FF, data.frame(OD.Rtg=ORtg/DRtg,
#'                F1.r=F1.Def/F1.Off, F2.r=F2.Off/F2.Def,
#'                F3.O=F3.Def, F3.D=F3.Off))
#' X$P3M <- Tbox$P3M
#' X$STL.r <- Tbox$STL/Obox$STL
#' kclu1 <- kclustering(X)
#' plot(kclu1)
#' kclu2 <- kclustering(X, k=9)
#' plot(kclu2)
#' @method plot kclustering
#' @export


plot.kclustering <- function(x, title = NULL, ncol.arrange = NULL, min.mid.max = NULL, label.size=2.5, ...) {

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
      ggrepel::geom_text_repel(data = df2, aes(x = x, y = y, label = label), force=2) +
      ggrepel::geom_text_repel(data = df3, aes(x = x, y = y, label = label), force=2) +
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
      xlab("Number of clusters") + ylab("BD/TD - Increments") +
      theme_bw()
  } else {
    profiles <- x[["Profiles"]]
    if (is.null(ncol.arrange)) {
      ncol.arrange <- ceiling(sqrt(nrow(profiles)))
    }
    if (is.null(title)) {
      #title <- profiles$clustnames
      title <- paste("Cluster", profiles$ID, "- CHI =", profiles$CHI)
    } else if (length(title)!=nrow(profiles)) {
      stop("The length of 'title' is not equal to the number of clusters")
    }
    #pos.clst.nm <- which(names(profiles)=="clustnames")
    pos.clst.nm <- which(names(profiles) %in% c("ID", "CHI"))
    if (is.null(min.mid.max)) {
      ming <- min(profiles[, -pos.clst.nm])
      maxg <- max(profiles[, -pos.clst.nm])
      midg <- 0
      min.mid.max <- c(ming,midg,maxg)
    }
    p <- radialprofile(data=profiles[,-pos.clst.nm], title=title, ncol.arrange=ncol.arrange,
                       std=FALSE, min.mid.max=min.mid.max, label.size=label.size)
  }

  if (!is.ggplot(p)) {
    invisible(p)
  } else {
    return(p)
  }

}
