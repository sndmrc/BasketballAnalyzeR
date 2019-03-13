#' Create a bubble plot
#'
#' @param data A dataframe
#' @param id The name of the ID variable
#' @param x Name of variable on the x axis
#' @param y Name of variable on the y axis
#' @param col Name of variable on the color axis
#' @param size Name of variable on the size axis
#' @param text.col Name of variable for text colors
#' @param text.size Text font size
#' @param scale.size If TRUE, 'size' variable is rescaled between 0 and 100.
#' @param labels Vector of variable labels (on legend and axis)
#' @param mx mx
#' @param my my
#' @param mcol mcol
#' @param title Title for the plot
#' @param repel Activate text repelling
#' @param text.legend If TRUE, show the legend for text color
#' @return A ggplot2 object
#' @examples
#' X <- data.frame(T=Tbox$Team, P2p=Tbox$P2p, P3p=Tbox$P3p,
#'        FTp=Tbox$FTp, AS=Tbox$P2A + Tbox$P3A + Tbox$FTA)
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

bubbleplot <- function(data, id, x, y, col, size, text.col=NULL, text.size=2.5, scale.size=TRUE, labels = NULL, mx = NULL, my = NULL,
                       mcol = NULL, title = NULL, repel = TRUE, text.legend=TRUE) {

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
    geom_point(aes(size = size, fill = col), shape = 21, colour = "white") +
    scale_size_area(max_size = 10, guide = guide_legend(override.aes = list(colour = "black"))) +
    geom_hline(yintercept = my) +  geom_vline(xintercept = mx) +
    labs(x = labels[1], y = labels[2], fill = labels[3], size = labels[4], title = title) +
    xlim(xmin, xmax) + ylim(ymin, ymax)

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
