#' Create some two-dimensional plots for multidimensional scaling (MDS)
#'
#' @param data A dataframe to use for plot
#' @param labels labels
#' @param subset subset
#' @param zoom zoom
#' @param var var
#' @param title title
#' @param colors colors
#' @param contour contour
#' @param col.palette Colour palette
#' @param arrange A logical value. If TRUE radial plots are arranged in a single plot
#' @param ncol.arrange  The number of columns in the grid of arranged plots
#' @return A list with two objects: a list of plots (named 'listPlots') and a dataframe with MDS coordinates
#' @examples
#' data("Pbox")
#' dts <- subset(Pbox, Team=="Houston Rockets" & MIN>=500)
#' barline(data=dts, id="Player", bars=c("P2p","P3p","FTp"),
#'         line="MIN", order.by="Player",
#'         labels.bars=c("2P","3P","FT"), title="Houston Rockets")
#' @export
#' @importFrom directlabels geom_dl
#' @importFrom ggplot2 geom_contour
#' @importFrom MASS isoMDS
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom stats loess
#' @importFrom stats loess.control
#' @importFrom stats predict
#' @importFrom stats cmdscale
#' @importFrom stats dist
#' @importFrom stats sd
#' @importFrom grDevices rainbow

MDSmap <- function(data, labels = NULL, subset = NULL, zoom = NULL, var = NULL,
                   title = NULL, colors = NULL, contour = FALSE, col.palette = NULL,
                   arrange = FALSE, ncol.arrange = NULL) {

  X1 <- X2 <- x <- y <- z <- '..level..' <- NULL
  if (is.null(labels[1])) {
    labels <- as.character(1:nrow(data))
  }
  rownames(data) <- labels

  if (is.null(title)) {
    title <- "MDS Map"
  }

  if (!is.null(var)) {
    if (is.numeric(var)) {
      varnames <- names(data)[var]
    } else if (is.character(var)) {
      varnames <- var
    } else {
      stop("'var' must be a string or a number")
    }
  }

  data <- scale(data)
  dist.mat <- dist(data)

  # MDS - 2 dimensions
  mds <- MASS::isoMDS(dist.mat, k = 2, y = cmdscale(dist.mat, 2), maxit = 100)
  config <- data.frame(mds$points)
  stress <- mds$stress

  warn <- "BE CAREFUL: BAD FIT!!"
  if (stress <= 5) {
    warn <- "EXCELLENT FIT!!"
  } else if (stress > 5 & stress <= 10) {
    warn <- "GOOD FIT"
  } else {
    warn <- "FAIR FIT"
  }
  subtitle <- paste("Stress Index = ", round(stress, 2), "% - ", warn, sep = "")


  if (is.null(var)) { # If 'var' is NULL
    p <- ggplot(data = config, aes(x = X1, y = X2))
    if (is.null(zoom)) {
      xmin <- min(config[, 1]) - sd(config[, 1])
      xmax <- max(config[, 1]) + sd(config[, 1])
      p <- p + xlim(c(xmin, xmax))
    } else {
      xmin <- zoom[1]
      xmax <- zoom[2]
      ymin <- zoom[3]
      ymax <- zoom[4]
      p <- p + xlim(c(xmin, xmax)) + ylim(c(ymin, ymax))
    }
    if (is.null(subset)) {
      p <- p + geom_text(aes(label = row.names(config)), size = 3)
    } else {
      subset1 <- config[-subset, ]
      subset2 <- config[subset, ]
      p <- p + geom_text(data = subset1, aes(label = row.names(subset1)), size = 3) +
        geom_text(data = subset2, aes(label = row.names(subset2)),
                  size = 4, col = "tomato", fontface = 2)
    }
    p <- p + ggtitle(title) + xlab("") + ylab("") +
      annotate(geom = "text", label = subtitle, x = Inf, y = Inf, hjust = 1, vjust = -1) +
      theme_bw()
    listPlots <- p
  } else {  # If 'var' is not NULL
    if (is.null(col.palette)) {
      col.palette <- grDevices::rainbow(300, alpha = 1, start = 0.2, end = 1)
    }

    # List of level plots
    nv <- length(var)
    listPlots <- vector(nv, mode = "list")
    names(listPlots) <- varnames
    for (k in 1:nv) {
      dts <- data.frame(config[, 1:2], subset(data, select = var[k]))
      names(dts) <- c("D1", "D2", "Z")
      xyz.fit <- stats::loess(Z ~ D1 + D2, dts, control = loess.control(surface = "direct"), degree = 2)

      xnew <- seq(min(dts$D1), max(dts$D1), len = 60)
      ynew <- seq(min(dts$D2), max(dts$D2), len = 60)
      dts <- expand.grid(x = xnew, y = ynew)
      names(dts) <- c("D1", "D2")
      z <- stats::predict(xyz.fit, newdata = dts)

      mtx_melt <- data.frame(x = rep(xnew, nrow(z)), y = rep(ynew, each = ncol(z)), z = as.vector(z))

      p <- ggplot(data = mtx_melt, aes(x = x, y = y, z = z)) + geom_tile(aes(fill = z)) +
        scale_fill_gradientn(name = varnames[k], colours = col.palette) +
        xlab("") + ylab("")
      if (contour) {
        p <- p + geom_contour(color = "black", alpha = 0.5, show.legend = T) +
          directlabels::geom_dl(aes(label = ..level..),
                                method = "far.from.others.borders", stat = "contour")
      }
      listPlots[[k]] <- p
    }
  }

  # Arrange radial plots
  if (arrange) {
    if (is.null(ncol.arrange)) {
      listPlots <- gridExtra::arrangeGrob(grobs = listPlots, ncol = ceiling(sqrt(length(listPlots))))
    } else {
      listPlots <- gridExtra::arrangeGrob(grobs = listPlots, ncol = ncol.arrange)
    }
  }

  list(listPlots = listPlots, Coord = config)
}
