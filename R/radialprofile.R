#' Plot radial plot for player profiles
#'
#' @param data A dataframe
#' @param group A string containing the name of the variable with players' names
#' @param perc perc
#' @param std std
#' @param title Plot title
#' @param arrange A logical value. If TRUE radial plots are arranged in a single plot
#' @param ncol.arrange  The number of columns in the grid of arranged plots
#' @return A list of ggplot2 radial plots or a single ggplot2 plot of arranged radial plots
#' @examples
#' data("Pbox")
#' Pbox.PG <- Pbox[1:6,]
#' X <- data.frame(Pbox.PG$P2M, Pbox.PG$P3M, Pbox.PG$OREB+Pbox.PG$DREB,
#'                 Pbox.PG$AST, Pbox.PG$TO)/Pbox.PG$MIN
#' X <- cbind(Pbox.PG$Player,X)
#' names(X) <- c("ID","P2M","P3M","REB","AST","TO")
#' radialprofile(data=X, group="ID", ncol.arrange=3)
#' @export
#' @importFrom gridExtra grid.arrange

radialprofile <- function(data, group, perc = FALSE, std = TRUE, title = NULL,
                          ncol.arrange = NULL) {

  # Reorder columns
  pos_group <- which(names(data) == group)
  ord_cols <- c(pos_group, (1:ncol(data))[-pos_group])
  profile <- data[, ord_cols]
  names(profile)[1] <- "group"

  # Add title
  if (!is.null(title) & length(title)==nrow(profile)) {
    profile[, 1] <- title
  } else if (!is.null(title) & length(title)!=nrow(profile))  {
    warning("The length of 'title' is not equal to the number of rows of 'data'")
  }

  # Remove missing rows with values
  if (any(is.na(profile[, -1]))) {
    rowsNA <- apply(profile[, -1], 1, function(x) any(is.na(x)))
    warning(paste("Removed", sum(rowsNA), "rows containing missing values"),
            call. = FALSE)
    if (length(title) == nrow(profile)) {
      title <- title[!rowsNA]
    }
    profile <- profile[!rowsNA, ]
  }
  npl <- nrow(profile)

  # Set defaults
  if (npl == 1 & std == TRUE) {
    print("One subject: std parameter set to FALSE")
    std <- FALSE
  }

  if (std == TRUE) {
    profile[, -1] <- scale(profile[, -1])
  }

  if (perc == TRUE & std == FALSE) {
    ming <- 0
    maxg <- 100
    midg <- 50
  } else {
    ming <- min(profile[, -1])
    maxg <- max(profile[, -1])
    midg <- (min(profile[, -1]) + max(profile[, -1]))/2
  }

  # List of radial plots
  listPlots <- vector(npl, mode = "list")
  for (i in 1:npl) {
    X <- profile[i, ]
    listPlots[[i]] <- CreateRadialPlot(X, grid.min = ming, grid.mid = midg, grid.max = maxg,
                                       label.gridline.min = F, gridline.mid.colour = "dodgerblue", group.line.width = 0.7,
                                       group.point.size = 2, label.centre.y = F, background.circle.colour = "dodgerblue",
                                       plot.extent.x.sf = 1.2, plot.extent.y.sf = 1.2, titolo = TRUE)
  }
  names(listPlots) <- profile[, 1]

  # Arrange radial plots
  if (is.null(ncol.arrange)) {
    out <- gridExtra::grid.arrange(grobs = listPlots, ncol = ceiling(sqrt(length(listPlots))))
  } else {
    out <- gridExtra::grid.arrange(grobs = listPlots, ncol = ncol.arrange)
  }
  invisible(listPlots)

}
