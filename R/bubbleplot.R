#' Draws a bubble plot
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param data a data frame.
#' @param id character, name of the ID variable.
#' @param x character, name of the x-axis variable.
#' @param y character, name of the y-axis variable.
#' @param col character, name of variable on the color axis.
#' @param size character, name of variable on the size axis.
#' @param text.col character, name of variable for text colors.
#' @param text.size numeric, text font size (default 2.5).
#' @param scale.size logical; if \code{TRUE}, size variable is rescaled between 0 and 100.
#' @param labels character vector, variable labels (on legend and axis).
#' @param mx numeric, x-coordinate of the vertical axis; default is the mean value of \code{x} variable.
#' @param my numeric, y-coordinate of the horizontal axis; default is the mean value of \code{y} variable.
#' @param mcol numeric, midpoint of the diverging scale (see \code{\link[ggplot2]{scale_colour_gradient2}}); default is the mean value of \code{col} variable.
#' @param title character, plot title.
#' @param repel logical; if \code{TRUE}, activate text repelling.
#' @param text.legend logical; if \code{TRUE}, show the legend for text color.
#' @param hline logical; if \code{TRUE}, a horizontal line is drawn with y intercept at the mean value of the variable on the y axis.
#' @param vline logical; if \code{TRUE}, a vertical line is drawn with x intercept at the mean value of the variable on the x axis.
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return A \code{ggplot2} object
#' @examples
#' X <- with(Tbox, data.frame(T=Team, P2p=P2p, P3p=P3p, FTp=FTp, AS=P2A+P3A+FTA))
#' labs <- c("2-point shots (% made)","3-point shots (% made)",
#'           "free throws (% made)","Total shots attempted")
#' bubbleplot(X, id="T", x="P2p", y="P3p", col="FTp",
#'            size="AS", labels=labs)
#' @export
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 scale_size_area
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_vline
#' @importFrom dplyr mutate

bubbleplot <- function(data, id, x, y, col, size, text.col=NULL, text.size=2.5,
                       scale.size=TRUE, labels = NULL, mx = NULL, my = NULL,
                       mcol = NULL, title = NULL, repel = TRUE, text.legend=TRUE,
                       hline=TRUE, vline=TRUE) {

  ID <- textColor <- NULL
  if (is.null(text.col)) {
    dts <- data %>%
      dplyr::mutate(ID  = !!rlang::sym(id) , x = !!rlang::sym(x), y = !!rlang::sym(y),
             col = !!rlang::sym(col), size = !!rlang::sym(size)) %>%
      dplyr::select(ID, x, y, col, size)
  } else {
    dts <- data %>%
      dplyr::mutate(ID  = !!rlang::sym(id) , x = !!rlang::sym(x), y = !!rlang::sym(y),
             col = !!rlang::sym(col), size = !!rlang::sym(size),
             textColor=!!rlang::sym(text.col)) %>%
      dplyr::select(ID, x, y, col, size, textColor)
  }
  if (is.null(labels)) {
    labels <- names(dts)[-1]
  }

  if (is.null(mx))
    mx <- mean(dts$x)
  if (is.null(my))
    my <- mean(dts$y)
  if (is.null(mcol) & !is.factor(dts$col))
    mcol <- mean(dts$col)

  xmin <- min(dts$x) - (mx - min(dts$x))/4
  xmax <- max(dts$x) + (max(dts$x) - mx)/4
  ymin <- min(dts$y) - (my - min(dts$y))/4
  ymax <- max(dts$y) + (max(dts$y) - my)/4
  if (scale.size) {
    dts$size <- (dts$size - min(dts$size))/(max(dts$size) - min(dts$size)) * 100
  }
  p <- ggplot(dts, aes(x = x, y = y, label = ID)) +
    geom_point(aes(size = size, fill = col), shape = 21, colour = "gray") +
    scale_size_area(max_size = 10, guide = guide_legend(override.aes = list(colour = "black", fill="black"))) +
    labs(x = labels[1], y = labels[2], fill = labels[3], size = labels[4], title = title) +
    xlim(xmin, xmax) + ylim(ymin, ymax)

  if (hline) {
    p <- p + geom_hline(yintercept = my)
  }

  if (vline) {
    p <- p + geom_vline(xintercept = mx)
  }

  if (!is.factor(dts$col)) {
    p <- p + scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mcol)
  }

  if (is.null(text.col)) {
    if (repel) {
      p <- p + ggrepel::geom_text_repel(size = text.size)
    } else {
      p <- p + geom_text(size = text.size)
    }
  } else {
    if (repel) {
      p <- p + ggrepel::geom_text_repel(aes(color=textColor), size = text.size, show.legend=text.legend)
    } else {
      p <- p + geom_text(aes(color=textColor), size = text.size, show.legend=text.legend)
    }
  }
  return(p)
}
