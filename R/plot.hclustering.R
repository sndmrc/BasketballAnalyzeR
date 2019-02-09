#' Plot hierarchical clustering of NBA teams
#'
#' @param x An object of class 'hclustering'
#' @param title Plot title
#' @param profiles Plot a radial plots of cluster mean profiles
#' @param ncol.arrange Number of columns when arranging multiple grobs on a page
#' @param circlize Plot a circular dendrogram
#' @param horiz Plot an horizontal dendrogram (only for non circular dendrograms)
#' @param cex.labels The magnification to be used for dendrogram labels
#' @param colored.labels Assign different colors to labels of different clusters
#' @param colored.branches Assign different colors to dendrogram branches of different clusters
#' @param rect Draw rectangles around the branches of a dendrogram in order to highlight the corresponding clusters
#' @param lower.rect A (scalar) value of how low should the lower part of the rect be
#' @param min.mid.max A numerical vector with 3 elements: lower bound, middle dashed line, upper bound for radial axis
#' @param ... other graphical parameters
#' @return A single ggplot2 plot or (when \code{profile=FALSE}) a list of (radial) plots (when \code{profile=TRUE}))
#' @examples
#' data <- data.frame(Pbox$PTS,Pbox$P3M,
#'                    Pbox$OREB + Pbox$DREB, Pbox$AST,
#'                    Pbox$TOV, Pbox$STL, Pbox$BLK,Pbox$PF)
#' names(data) <- c("PTS","P3M","REB","AST","TOV","STL","BLK","PF")
#' data <- subset(data, Pbox$MIN >= 1500)
#' ID <- Pbox$Player[Pbox$MIN >= 1500]
#' out <- hclustering(data, labels=ID, k=7)
#' plot(out)
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
        title <- prfls$clustnames
      } else if (length(title)!=nrow(prfls)) {
        stop("The length of 'title' is not equal to the number of clusters")
      }
      pos.clst.nm <- which(names(prfls)=="clustnames")
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

  if (is.list(p)) {
    invisible(p)
  } else {
    return(p)
  }

}

