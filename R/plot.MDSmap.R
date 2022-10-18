#' Draws two-dimensional plots for multidimensional scaling (MDS) from a 'MDSmap' object
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param x an object of class \code{MDSmap}.
#' @param level.plot logical; if TRUE, draws a level plot, otherwise draws a scatter plot (not active if \code{zvar=NULL}).
#' @param z.var character vector; defines the set of variables (available in the \code{data} data frame of \code{\link{MDSmap}}) used to color-coding the points in the map (for scatter plots) or, alternatively, overlap to the map a colored level plot.
#' @param title character, plot title.
#' @param labels character vector, labels for (x, y) points (only for single scatter plot).
#' @param repel_labels logical; if \code{TRUE}, draw text labels using repelling (not for highlighted points) (see \code{\link[ggrepel]{geom_text_repel}}).
#' @param text_label logical; if \code{TRUE}, draw a rectangle behind the text labels (not active if \code{subset=NULL}).
#' @param label_size numeric;  label font size (default \code{label_size=3}, for scatter plots).
#' @param subset logical vector, to select a subset of points to be highlighted.
#' @param col.subset character, color for the subset of points.
#' @param zoom numeric vector with 4 elements; \code{c(xmin,xmax,ymin,ymax)} for the x- and y-axis limits of the plot.
#' @param palette color palette.
#' @param contour logical; if \code{TRUE}, contour lines are plotted (not active if \code{level.plot=FALSE}).
#' @param ncol.arrange integer, number of columns when arranging multiple grobs on a page.
#' @param ... other graphical parameters.
#' @seealso \code{\link{MDSmap}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return A single \code{ggplot2} plot or a list of \code{ggplot2} plots
#' @examples
#' data <- data.frame(Pbox$PTS, Pbox$P3M, Pbox$P2M, Pbox$OREB + Pbox$DREB, Pbox$AST,
#' Pbox$TOV,Pbox$STL, Pbox$BLK)
#' names(data) <- c('PTS','P3M','P2M','REB','AST','TOV','STL','BLK')
#' selp <- which(Pbox$MIN >= 1500)
#' data <- data[selp,]
#' id <- Pbox$Player[selp]
#' mds <- MDSmap(data)
#' plot(mds, labels=id, z.var="P2M", level.plot=FALSE, palette=rainbow)
#' @method plot MDSmap
#' @export

plot.MDSmap <- function(x, z.var = NULL, level.plot=TRUE,  title = NULL, labels = NULL,
                        repel_labels = FALSE, text_label = TRUE, label_size=3, subset = NULL, col.subset = "gray50",
                        zoom = NULL, palette = NULL, contour = FALSE, ncol.arrange = NULL, ...) {

  if (!is.MDSmap(x)) {
    stop("Not a 'MDSmap' object")
  }
  X1 <- X2 <- Z <- '..level..' <- NULL
  if (!is.null(z.var)) {
    if (!inherits(data, "dist")) {
      data <- x$data
    } else {
      stop("MDS was calculated using a distance matrix. Cannot find the 'z.var' variable")
    }
  }

  if (!is.null(z.var)) {
    if (is.numeric(z.var)) {
      varnames <- names(data)[z.var]
    } else if (is.character(z.var)) {
      varnames <- z.var
    } else {
      stop("'z.var' must be a string or a number")
    }
  }

  if (is.null(title)) {
    title <- "MDS Map"
  }

  config <- data.frame(x$points)
  stress <- x$stress

  warn <- "BE CAREFUL: BAD FIT!!"
  if (stress <= 5) {
    warn <- "EXCELLENT FIT!!"
  } else if (stress > 5 & stress <= 10) {
    warn <- "GOOD FIT"
  } else {
    warn <- "FAIR FIT"
  }
  #subtitle <- paste("Stress Index = ", round(stress, 2), "% - ", warn, sep = "")
  subtitle <- paste("Stress Index = ", round(stress, 2), "%", sep = "")

  if (is.null(z.var)) { ### If 'z.var' is NULL
    listPlots <- vector(1, mode = "list")
    p <- scatterplot(data=config, data.var=c("X1","X2"), labels=labels, repel_labels=repel_labels,
           text_label=text_label, label_size=label_size, subset=subset, col.subset = col.subset,
           zoom = NULL, title = title)
    p <- p + xlab("") + ylab("") +
      annotate(geom = "text", label = subtitle, x = Inf, y = Inf, hjust = 1, vjust = -1) +
      coord_cartesian(xlim=c(zoom[1], zoom[2]), ylim=c(zoom[3], zoom[4]), clip = 'off')
    listPlots[[1]] <- p
  } else if (!is.null(z.var) & !level.plot) {  ### If 'z.var' is not NULL & level.plot=FALSE
    nv <- length(z.var)
    listPlots <- vector(nv, mode = "list")
    names(listPlots) <- varnames
    for (k in 1:nv) {
      vark <- varnames[k]
      dts <- data.frame(config[, 1:2], subset(data, select = vark))
      names(dts) <- c("X1", "X2", vark)
      p <- scatterplot(data=dts, data.var=c("X1","X2"), z.var=vark, palette=palette,
            labels=labels, repel_labels=repel_labels, text_label=text_label, label_size=label_size,
            subset=subset, col.subset = col.subset, zoom = zoom, title = title)
      p <- p + xlab("") + ylab("") +
        annotate(geom = "text", label = subtitle, x = Inf, y = Inf, hjust = 1, vjust = -1) +
        coord_cartesian(clip = 'off')
      listPlots[[k]] <- p
    }
  } else if (!is.null(z.var) & level.plot) {  ### If 'z.var' is not NULL & level.plot=TRUE
    if (is.null(palette)) {
      cols <- grDevices::rainbow(100, alpha = 1, start = 0.2, end = 1)
    } else {
      cols <- palette(100)
    }
    # List of level plots
    nv <- length(z.var)
    listPlots <- vector(nv, mode = "list")
    names(listPlots) <- varnames
    for (k in 1:nv) {
      dts <- data.frame(config[, 1:2], subset(data, select = varnames[k]))
      names(dts) <- c("D1", "D2", "Z")
      xyz.fit <- stats::loess(Z ~ D1 + D2, dts, control = loess.control(surface = "direct"), degree = 2)

      xnew <- seq(min(dts$D1), max(dts$D1), len = 60)
      ynew <- seq(min(dts$D2), max(dts$D2), len = 60)
      dts <- expand.grid(x = xnew, y = ynew)
      names(dts) <- c("D1", "D2")
      z <- stats::predict(xyz.fit, newdata = dts)
      mtx_melt <- data.frame(X1 = rep(xnew, nrow(z)), X2 = rep(ynew, each = ncol(z)), Z = as.vector(z))

      p <- ggplot(data = mtx_melt, aes(x = X1, y = X2, z = Z)) + geom_tile(aes(fill = Z)) +
        scale_fill_gradientn(name = varnames[k], colours = cols) +
        xlab("") + ylab("") + theme(panel.background = element_blank())
      if (contour) {
        p <- p + geom_contour(color = "black", alpha = 0.5, show.legend = TRUE) +
          directlabels::geom_dl(aes(label = ..level..),
                                method = "far.from.others.borders", stat = "contour")
      }
      listPlots[[k]] <- p
    }
  }
  # Arrange level plots
  if (length(listPlots)>1) {
    if (is.null(ncol.arrange)) {
      ncol.arrange <- ceiling(sqrt(length(listPlots)))
    }
    p <- gridExtra::arrangeGrob(grobs = listPlots, ncol = ncol.arrange)
  } else {
    p <- listPlots[[1]]
  }
  grid::grid.draw(p)

  invisible(listPlots)
}
