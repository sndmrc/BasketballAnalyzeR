#' Draws a scatter plot or a matrix of scatter plots
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketball.analyzer.help@gmail.com})
#' @param data an object of class \code{data.frame}.
#' @param data.var character or numeric vector, name or column number of variables (in \code{data} object) used on the axes of scatter plot(s).
#' @param z.var character or number, name or column number of variable (in \code{data} object) used to assign colors to points (see Details).
#' @param palette color palette (active when plotting a single scatter plot; see Value).
#' @param labels character vector, labels for points (active when plotting a single scatter plot, see Value).
#' @param subset logical or numeric vector, to select a subset of points to be highlighted (active when plotting a single scatter plot; see Value).
#' @param repel_labels logical; if \code{TRUE}, draws text labels of not highlighted points using repelling (active when plotting a single scatter plot; see Value).
#' @param text_label logical; if \code{TRUE}, draws a rectangle behind the labels of highlighted points (active when plotting a single scatter plot; see Value).
#' @param label_size integer number;  label font size (default \code{label_size=3}).
#' @param col.subset character, color for the labels and rectangles of highlighted points (active when plotting a single scatter plot; see Value).
#' @param zoom  numeric vector with 4 elements; \code{c(xmin,xmax,ymin,ymax)} for the x- and y-axis limits of the plot (active when plotting a single scatter plot; see Value).
#' @param title character, plot title.
#' @param legend logical, if \code{legend=FALSE} legend is removed (active when plotting a single scatter plot with \code{z.var} not \code{NULL}; see Value).
#' @param upper  list, may contain the variables \code{continuous}, \code{combo}, \code{discrete}, and \code{na} (active when plotting a matrix of scatter plot; see Value and \code{upper} in \code{\link[GGally]{ggpairs}})
#' @param lower  list, may contain the variables \code{continuous}, \code{combo}, \code{discrete}, and \code{na} (active when plotting a matrix of scatter plot; see Value and \code{lower} in \code{\link[GGally]{ggpairs}})
#' @param diag  list, may contain the variables \code{continuous}, \code{discrete}, and \code{na} (active when plotting a matrix of scatter plot; see Value and \code{diag} in \code{\link[GGally]{ggpairs}})
#' @return A \code{ggplot2} object with a single scatter plot if \code{length(data.var)=2} or a matrix of scatter plots if \code{length(data.var)>2}.
#' @seealso \code{\link[GGally]{ggpairs}}
#' @details If \code{length(data.var)=2}, the variable specified in \code{z.var} can be numeric or factor; if \code{length(data.var)>2}, the variable specified in \code{z.var} must be a factor.
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @examples
#' # Single scatter plot
#' Pbox.sel <- subset(Pbox, MIN>= 500)
#' X <- data.frame(AST=Pbox.sel$AST/Pbox.sel$MIN,TOV=Pbox.sel$TOV/Pbox.sel$MIN)
#' X$PTSpm <- Pbox.sel$PTS/Pbox.sel$MIN
#' mypal <- colorRampPalette(c("blue","yellow","red"))
#' scatterplot(X, data.var=c("AST","TOV"), z.var="PTSpm", labels=1:nrow(X), palette=mypal)
#' # Matrix of scatter plots
#' data <- Pbox[1:50, c("PTS","P3M","P2M","OREB","Team")]
#' scatterplot(data, data.var=1:4, z.var="Team")
#' @export
#' @importFrom GGally ggpairs
#' @importFrom ggplot2 is.ggplot

scatterplot <- function(data, data.var, z.var=NULL, palette=NULL, labels=NULL, repel_labels=FALSE, text_label=TRUE,
                        label_size=3, subset = NULL, col.subset='gray50', zoom = NULL, title = NULL, legend=TRUE,
                        upper = list(continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na"),
                        lower=list(continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"),
                        diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag")) {

  x <- y <- z <- NULL
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  } else if (is.data.frame(data) & ncol(data)==1) {
    stop("The number of columns in 'data' must be 2 or more")
  }

  if (is.numeric(data.var)) {
    nm.data.vars <- names(data)[data.var]
  } else if (is.character(data.var)) {
    nm.data.vars <- data.var
  } else {
    stop("'data.var' must be numeric or character")
  }

  if (length(z.var)>1) {
    stop("The length of 'z.var' must be 1")
  }
  if (!is.null(z.var)) {
    if (is.numeric(z.var)) {
      nm.z.var <- names(data)[z.var]
    } else if (is.character(z.var)) {
      nm.z.var <- z.var
    } else {
      stop("'z.var' must be numeric or character")
    }
  }

  if (length(data.var)==2) {
    df <- data[, data.var]
    names(df) <- c("x","y")
    if (is.null(z.var)) {
      df$text <- paste0(nm.data.vars[1],": ",df$x,"<br>",
                        nm.data.vars[2],": ",df$y,"<br>")
      p <- ggplot(data=df, aes(x=x, y=y, text=text))
    } else {
      z <- data[, z.var]
      if (is.character(z)) {
        z <- factor(z)
        df$z <- z
      } else if (is.factor(z) | is.numeric(z)) {
        df$z <- z
      }
      df$text <- paste0(nm.data.vars[1],": ",df$x,"<br>",
                        nm.data.vars[2],": ",df$y,"<br>", z.var,": ", z)
      p <- ggplot(data=df, aes(x=x, y=y, color=z, text=text))
    }
    if (!is.null(zoom)) {
      xmin <- zoom[1]
      xmax <- zoom[2]
      ymin <- zoom[3]
      ymax <- zoom[4]
      p <- p + coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax))
    }

    if (is.null(subset)) { ### if 'subset' is not defined
      if (is.null(labels)) {
        p <- p + geom_point()
      } else {
        if (repel_labels) {
          p <- p + ggrepel::geom_text_repel(aes(label = labels), size = label_size)
        } else {
          p <- p + geom_text(aes(label = labels), size = label_size)
        }
      }
    } else {  ### if 'subset' is defined
      subset1 <- df[-subset, ]
      subset2 <- df[subset, ]
      subset1.labels <- labels[-subset]
      subset2.labels <- labels[subset]
      if (is.null(labels)) {
        p <- p + geom_point(data = subset1, size = 3) +
          geom_point(data = subset2, size = 4, col = col.subset)
      } else {
        if (repel_labels) {
          p <- p + ggrepel::geom_text_repel(data=subset1, aes(label=subset1.labels),
                                            size = label_size)
        } else {
          p <- p + geom_text(data=subset1, aes(label=subset1.labels),
                             size = label_size)
        }
        if (text_label) {
          p <- p + ggrepel::geom_label_repel(data = subset2, aes(label=subset2.labels),
                                             size = label_size+1, col = col.subset, fontface = 2)
        } else {
          p <- p + ggrepel::geom_text_repel(data = subset2, aes(label=subset2.labels),
                                            size = label_size+1, col = col.subset, fontface = 2)
        }
      }
    }
    if (!is.null(palette) & !is.null(z) & is.factor(z)) {
      p <- p + scale_color_manual(name=nm.z.var, values=palette(length(unique(z))))
    } else if (!is.null(palette) & !is.null(z) & !is.factor(z)) {
      p <- p + scale_color_gradientn(name=nm.z.var, colors=palette(length(unique(z))))
    }
    p <- p + labs(title=title, x=nm.data.vars[1], y=nm.data.vars[2]) +
      theme_bw()

  }  else if (length(data.var)>2) { ### Matrix of scatter plots ###
    if (is.null(z.var)) {
      df <- data[, data.var]
      p <- GGally::ggpairs(df, title=title,
                           lower=lower, upper=upper, diag=diag)
    } else {
      df <- data[, data.var]
      df$z <- data[, z.var]
      if (is.numeric(z.var)) {
        names(df)[ncol(df)] <- names(data)[z.var]
      } else {
        names(df)[ncol(df)] <- z.var
      }
      p <- GGally::ggpairs(df, mapping=aes_string(color=names(df)[ncol(df)]), title=title,
                           lower=lower, upper=upper, diag=diag)
    }
  }
  p <- p + theme_bw()
  if (!is.null(z.var) & !legend) {
    p <- p + theme(legend.position="none")
  }

  return(p)
}

