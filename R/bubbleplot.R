#' Create a bubble plot
#'
#' @param data A dataframe
#' @param id The name of the ID variable
#' @param x Name of variable on the x axis
#' @param y Name of variable on the y axis
#' @param col Name of variable on the color axis
#' @param size Name of variable on the size axis
#' @param labels Vector of labels for 4 variables
#' @param mx mx
#' @param my my
#' @param mcol mcol
#' @param title Title for the plot
#' @param repel Activate text repelling
#' @return A ggplot2 object
#' @export
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 scale_size_area
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_vline

bubbleplot <- function(data, id, x, y, col, size, labels = NULL, mx = NULL, my = NULL,
                       mcol = NULL, title = NULL, repel = FALSE) {

  ID <- NULL
  dts <- data %>% dplyr::select(id, x, y, col, size) %>% dplyr::rename(ID = !!id,
                  x = !!x, y = !!y, col = !!col, size = !!size) %>% stats::na.omit()
  # names(dts) <- c('id','x','y','col','size')

  if (is.null(labels)) {
    labels <- names(dts)[-1]
  }

  if (is.null(mx))
    mx <- mean(dts$x)
  if (is.null(my))
    my <- mean(dts$y)
  if (is.null(mcol))
    mcol <- mean(dts$col)

  xmin <- min(dts$x) - (mx - min(dts$x))/4
  xmax <- max(dts$x) + (max(dts$x) - mx)/4
  ymin <- min(dts$y) - (my - min(dts$y))/4
  ymax <- max(dts$y) + (max(dts$y) - my)/4
  dts$size <- (dts$size - min(dts$size))/(max(dts$size) - min(dts$size)) * 100

  p <- ggplot(dts, aes(x = x, y = y, label = ID)) +
    geom_point(aes(size = size, fill = col), shape = 21, colour = "white") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mcol) +
    scale_size_area(max_size = 10, guide = guide_legend(override.aes = list(colour = "black"))) +
    geom_hline(yintercept = my) +  geom_vline(xintercept = mx) +
    labs(x = labels[1], y = labels[2], fill = labels[3], size = labels[4], title = title) +
    xlim(xmin, xmax) + ylim(ymin, ymax)

  if (repel) {
    p <- p + ggrepel::geom_text_repel(size = 2.5)
  } else {
    p <- p + geom_text(size = 2.5)
  }

  print(p)
  invisible(p)
}
