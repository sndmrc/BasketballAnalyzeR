#' Draw some two-dimensional plots for multidimensional scaling (MDS)
#'
#' @param x An object of class 'MDSmap'
#' @param z.var The stratification variable (color)
#' @param level.plot If TRUE, draw a level plot, otherwise draw a scatter plot
#' @param title Plot title
#' @param labels Text labels for (x, y) points; only for single scatter plot
#' @param repel_labels If TRUE, draw text labels of not evidenced points using repelling
#' @param text_label If TRUE, draw a rectangle behind the evidenced text
#' @param subset Subset of points to be evidenced
#' @param col.subset Color for the subset of points
#' @param zoom X and Y axis range
#' @param palette Color palette
#' @param contour If TRUE, contour lines are plotted
#' @param ncol.arrange Number of columns when arranging multiple grobs on a page
#' @param ... other graphical parameters
#' @return A single ggplot2 plot or a list of ggplot2 plots
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
                        repel_labels = FALSE, text_label = TRUE, subset = NULL, col.subset = "gray50",
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
  subtitle <- paste("Stress Index = ", round(stress, 2), "% - ", warn, sep = "")


  if (is.null(z.var)) { ### If 'z.var' is NULL
    listPlots <- vector(1, mode = "list")
    p <- scatterplot(data=config, data.var=c("X1","X2"), labels=labels, repel_labels=repel_labels,
           text_label=text_label, subset=subset, col.subset = col.subset, zoom = zoom, title = title,
           draw.plot=FALSE)
    p <- p + xlab("") + ylab("") +
      annotate(geom = "text", label = subtitle, x = Inf, y = Inf, hjust = 1, vjust = -1) +
      coord_cartesian(clip = 'off')
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
            labels=labels, repel_labels=repel_labels, text_label=text_label, subset=subset, col.subset = col.subset,
            zoom = zoom, title = title, draw.plot=FALSE)
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
      p <- gridExtra::arrangeGrob(grobs = listPlots, ncol = ceiling(sqrt(length(listPlots))))
    } else {
      p <- gridExtra::arrangeGrob(grobs = listPlots, ncol = ncol.arrange)
    }
    grid::grid.draw(p)
  } else {
    listPlots <- listPlots[[1]]
    grid::grid.draw(listPlots)
  }

  invisible(listPlots)
}
