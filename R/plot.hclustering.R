#' Plots hierarchical clustering from a 'hclustering' object
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param x an object of class \code{hclustering}.
#' @param title character or vector of characters (when plotting radial plots of cluster profiles; see Value), plot title(s).
#' @param profiles logical; if \code{TRUE}, displays radial plots of cluster profiles (active if \code{x$k} is not \code{NULL}; see Value).
#' @param ncol.arrange integer, number of columns when arranging multiple grobs on a page (active when plotting radial plots of cluster profiles; see Value).
#' @param circlize logical; if \code{TRUE}, plots a circular dendrogram (active when plotting a dendrogram; see Value).
#' @param horiz logical; if \code{TRUE}, plots an horizontal dendrogram (active when plotting a non circular dendrogram; see Value).
#' @param cex.labels numeric, the magnification to be used for labels (active when plotting a dendrogram; see Value).
#' @param colored.labels logical; if \code{TRUE}, assigns different colors to labels of different clusters (active when plotting a dendrogram; see Value).
#' @param colored.branches logical; if \code{TRUE}, assigns different colors to branches of different clusters (active when plotting a dendrogram; see Value).
#' @param rect logical; if \code{TRUE}, draws rectangles around the branches in order to highlight the corresponding clusters (active when plotting a dendrogram; see Value).
#' @param lower.rect numeric, a value of how low should the lower part of the rect be (active when plotting a dendrogram; see option \code{lower_rect} of \code{\link[dendextend]{rect.dendrogram}}).
#' @param min.mid.max numeric vector with 3 elements: lower bound, middle dashed line, upper bound for radial axis (active when plotting radial plots of cluster profiles; see Value).
#' @param ... other graphical parameters.
#' @seealso \code{\link{hclustering}}, \code{\link{radialprofile}}.
#' @return If \code{x$k} is \code{NULL}, \code{plot.hclustering} returns a single \code{ggplot2} object, displaying the pattern of the explained variance vs the number of clusters.
#' @return If \code{x$k} is not \code{NULL} and \code{profiles=FALSE}, \code{plot.hclustering} returns a single \code{ggplot2} object, displaying the dendrogram.
#' @return If \code{x$k} is not \code{NULL} and \code{profiles=TRUE}, \code{plot.hclustering} returns a list of \code{ggplot2} objects, displaying the radial plots of the cluster profiles.
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @examples
#' data <- with(Pbox, data.frame(PTS, P3M, REB=OREB+DREB, AST, TOV, STL, BLK, PF))
#' data <- subset(data, Pbox$MIN >= 1500)
#' ID <- Pbox$Player[Pbox$MIN >= 1500]
#' hclu1 <- hclustering(data)
#' plot(hclu1)
#' hclu2 <- hclustering(data, labels=ID, k=7)
#' plot(hclu2)
#' @method plot hclustering
#' @export
#' @importFrom dendextend circlize_dendrogram
#' @importFrom dendextend plot_horiz.dendrogram
#' @importFrom dendextend rect.dendrogram
#' @importFrom dendextend set
#' @importFrom stats as.dendrogram

plot.hclustering <- function(x, title = NULL, profiles=FALSE, ncol.arrange = NULL, circlize=FALSE, horiz=TRUE, cex.labels=0.7, colored.labels=TRUE, colored.branches=FALSE, rect=FALSE, lower.rect=NULL, min.mid.max=NULL, ...) {

  if (!is.hclustering(x)) {
    stop("Not a 'hclustering' object")
  }

  circ_dend <- function(dend) {
    dendextend::circlize_dendrogram(dend)
  }

  plot_horiz_dend <- function(dend, k, rect, lower.rect) {
    plot.new()
    oldmar <- par("mar")
    mar <- oldmar
    mar[c(1,3)] <- 0
    par(mar=mar)
    dendextend::plot_horiz.dendrogram(dend, side=T)
    if (rect) {
      usr <- par("usr")
      if (is.null(lower.rect)) lower.rect <- usr[1]-(usr[2]-usr[1])/9
      dendextend::rect.dendrogram(dend, k=k, horiz = T, lty=2, lwd=1, border="grey50", lower_rect=lower.rect)
    }
    par(oldmar)
  }

  plot_dend <- function(dend, k, rect, lower.rect) {
    plot.new()
    oldmar <- par("mar")
    mar <- oldmar
    mar[c(2,3,4)] <- 1
    par(mar=mar)
    plot(dend)
    if (rect) {
      usr <- par("usr")
      if (is.null(lower.rect)) lower.rect <- usr[3]-(usr[4]-usr[3])/9
      dendextend::rect.dendrogram(dend, k=k, horiz = F, lty=2, lwd=1, border="grey50", lower_rect=lower.rect)
    }
    par(oldmar)
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
      geom_text(data = df2, aes(x = x, y = y, label = label), vjust = +2) +
      geom_line(data = df2, aes(x = x, y = y), linetype = 2) +
      geom_point(data = df2, aes(x = x, y = y), size = 6, colour = "white") +
      geom_point(data = df2, aes(x = x, y = y), shape = 0, size = 2) +
      geom_text(data = df3, aes(x = x, y = y, label = label), vjust = +2) +
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
      xlab("Number of clusters") + ylab("BD/TD - Increments") +
      theme_bw()
  } else {
    if (profiles) {
      prfls <- x[["Profiles"]]
      if (is.null(ncol.arrange)) {
        ncol.arrange <- ceiling(sqrt(nrow(prfls)))
      }
      if (is.null(title)) {
        #title <- prfls$clustnames
        title <- paste("Cluster", prfls$ID, "- CHI =", prfls$CHI)
      } else if (length(title)!=nrow(prfls)) {
        stop("The length of 'title' is not equal to the number of clusters")
      }
      #pos.clst.nm <- which(names(prfls)=="clustnames")
      pos.clst.nm <- which(names(prfls) %in% c("ID", "CHI"))
      if (is.null(min.mid.max)) {
        ming <- min(prfls[, -pos.clst.nm])
        maxg <- max(prfls[, -pos.clst.nm])
        midg <- 0
        min.mid.max <- c(ming,midg,maxg)
      }
      p <- radialprofile(data = prfls[, -pos.clst.nm], title = title, ncol.arrange = ncol.arrange,
                        std=FALSE, min.mid.max=min.mid.max)
    } else {
      hcl <- x[["Hclust"]]
      dend <- hcl %>%
        stats::as.dendrogram() %>%
        dendextend::set("labels_cex", cex.labels)
      if (colored.labels) dend <- dend %>% dendextend::set("labels_colors", k=k)
      if (colored.branches) {
        lbl_clu1 <- x[["Subjects"]]
        lbl_clu2 <- dendextend::cutree(hcl, k=k, order_clusters_as_data=FALSE)
        lbl_clu1 <- lbl_clu1[match(names(lbl_clu2), row.names(lbl_clu1)),]
        tbl <- table(lbl_clu1$Cluster,lbl_clu2)
        lbls <- apply(tbl,2,which.max)
        dend <- dend %>% dendextend::color_branches(k=k, groupLabels=lbls)
      }
      if (circlize) {
        xpr <- substitute(circ_dend(dend))
        p <- ggplotify::as.ggplot(as.expression(xpr))
      } else {
        if (horiz) {
          xpr <- substitute(plot_horiz_dend(dend, k, rect, lower.rect))
          p <- ggplotify::as.ggplot(as.expression(xpr))
        } else {
          xpr <- substitute(plot_dend(dend, k, rect, lower.rect))
          p <- ggplotify::as.ggplot(as.expression(xpr))
        }
      }
      p <- p+labs(title=title)
    }
  }

  if (!is.ggplot(p)) {
    invisible(p)
  } else {
    return(p)
  }

}

