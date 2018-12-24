#' Scatter plot and matrix of scatter plot
#'
#' @param data A dataframe
#' @param z Stratification variable
#' @param z.name Name of stratification variable
#' @param palette Color palette
#' @param labels Text labels for (x, y) points; only for single scatter plot
#' @param subset Subset of points to be evidenced; only for single scatter plot
#' @param text_label If TRUE, draw a rectangle behind the evidenced text
#' @param col.subset Color for the subset of points; only for single scatter plot
#' @param zoom X and Y axis range; only for single scatter plot
#' @param title Plot title
#' @param upper  See help for GGally::ggpairs
#' @param lower  See help for GGally::ggpairs
#' @param diag  See help for GGally::ggpairs
#' @return A ggplot2 plot
#' @examples
#' # Single scatter plot
#' data <- Pbox[, c("PTS","P3M")]
#' scatterplot(data, labels=as.character(Pbox$Player), subset=1:3)
#' # Matrix of scatter plots
#' data <- Pbox[, c("PTS","P3M","P2M","OREB")]
#' scatterplot(data, z=Pbox$Team)
#' @export
#' @importFrom GGally ggpairs

scatterplot <- function(data, z=NULL, z.name=NULL, palette = NULL, labels = NULL, text_label=TRUE,
                        subset = NULL, col.subset='gray50', zoom = NULL, title = NULL,
                        upper = list(continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na"),
                        lower=list(continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"),
                        diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag")) {

  x <- y <- NULL
  if (!is.data.frame(data)) {
    stop("'data' must be a matrix or a data frame")
  } else if (is.data.frame(data) & ncol(data)==1) {
    stop("The number of columns in 'data' must be 2 or more")
  }
  if (is.null(labels)) {
    labels <- 1:nrow(data)
  }
  nm.vars <- names(data)

  if (ncol(data)==2) {
    if (is.null(z)) {
      names(data) <- c("x","y")
      p <- ggplot(data=data, aes(x=x, y=y))
    } else {
      if (!is.numeric(z)) {
        z <- factor(z)
      }
      data <- cbind(data, z)
      names(data) <- c("x","y","z")
      p <- ggplot(data=data, aes(x=x, y=y, color=z))
    }
    if (!is.null(zoom)) {
      xmin <- zoom[1]
      xmax <- zoom[2]
      ymin <- zoom[3]
      ymax <- zoom[4]
      p <- p + xlim(c(xmin, xmax)) + ylim(c(ymin, ymax))
    }
    if (is.null(subset)) {
      p <- p + geom_text(aes(label = labels), size = 3)
    } else {
      subset1 <- data[-subset, ]
      subset2 <- data[subset, ]
      subset1.labels <- labels[-subset]
      subset2.labels <- labels[subset]
      if (is.null(labels)) {
        p <- p + geom_point(data = subset1, size = 3) +
          geom_point(data = subset2, size = 4, col = col.subset)
      } else {
        p <- p + geom_text(data = subset1, aes(label = subset1.labels), size = 3)
        if (text_label) {
          p <- p + ggrepel::geom_label_repel(data = subset2, aes(label = subset2.labels),
                                             size = 4, col = col.subset, fontface = 2)
        } else {
          p <- p + ggrepel::geom_text_repel(data = subset2, aes(label = subset2.labels),
                                              size = 4, col = col.subset, fontface = 2)
        }
      }
    }
    if (!is.null(palette) & is.factor(z)) {
      p <- p + scale_color_manual(palette=palette)
    } else if (!is.null(palette) & !is.factor(z)) {
      p <- p + scale_color_gradientn(colors=palette(length(unique(z))))
    }
    p <- p + labs(title=title, x=nm.vars[1], y=nm.vars[2]) +
      ggplot2::guides(color=guide_legend(title=z.name)) +
      theme_bw()
  }
  else if (ncol(data)>2) {
    if (is.null(z)) {
      p <- GGally::ggpairs(data, title=title,
                           lower=lower, upper=upper, diag=diag)
    } else {
      p <- GGally::ggpairs(data, mapping=aes(color=z), title=title,
                           lower=lower, upper=upper, diag=diag)
    }
  }
  p <- p + theme_bw()
  if (!is.null(palette) & is.factor(z)) {
    p <- p + theme(legend.position="none")
  }
  print(p)
  invisible(p)
}

