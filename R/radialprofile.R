#' Plot radial plot for player profiles
#'
#' @param data A dataframe
#' @param perc perc
#' @param std std
#' @param title Title(s) for radial plots(s)
#' @param ncol.arrange  The number of columns in the grid of arranged plots
#' @param min.mid.max A numerical vector with 3 elements: lower bound, middle dashed line, upper bound for radial axis
#' @return A list of ggplot2 radial plots or a single ggplot2 plot of arranged radial plots
#' @examples
#' data("Pbox")
#' Pbox.PG <- Pbox[1:6,]
#' X <- data.frame(Pbox.PG$P2M, Pbox.PG$P3M, Pbox.PG$OREB+Pbox.PG$DREB,
#'                 Pbox.PG$AST, Pbox.PG$TO)/Pbox.PG$MIN
#' names(X) <- c("P2M","P3M","REB","AST","TO")
#' radialprofile(data=X, ncol.arrange=3, title=Pbox.PG$Player)
#' @export
#' @importFrom gridExtra grid.arrange

radialprofile <- function(data, perc = FALSE, std = TRUE, title = NULL,
                          ncol.arrange = NULL, min.mid.max=NULL) {

  # Set plot titles
  if (is.null(title)) {
    group <- 1:nrow(data)
  } else {
    if (length(title)==nrow(data)) {
      group <- factor(title)
    } else {
      stop("The length of 'title' is not equal to the number of rows of 'data'")
    }
  }
  profile <- cbind(group, data)

  # Remove rows with missing values
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
    warning("One subject: std parameter set to FALSE")
    std <- FALSE
  }

  if (std == TRUE) {
    profile[, -1] <- scale(profile[, -1])
  }

  if (is.null(min.mid.max)) {
    if (perc & !std) {
      ming <- 0
      midg <- 50
      maxg <- 100
    } else if (!perc & std) {
      mn <- min(profile[, -1])
      mx <- max(profile[, -1])
      ming <- -max(mn,mx)
      midg <- 0
      maxg <- max(mn,mx)
    } else {
      ming <- min(profile[, -1])
      midg <- (min(profile[, -1]) + max(profile[, -1]))/2
      maxg <- max(profile[, -1])
    }
  } else {
    ming <- min.mid.max[1]
    midg <- min.mid.max[2]
    maxg <- min.mid.max[3]
  }

  # List of radial plots
  listggplots <- vector(npl, mode = "list")
  for (i in 1:npl) {
    X <- profile[i, ]
    listggplots[[i]] <- CreateRadialPlot(X, grid.min = ming, grid.mid = midg, grid.max = maxg,
                                       label.gridline.min = F, gridline.mid.colour = "dodgerblue", group.line.width = 0.7,
                                       group.point.size = 2, label.centre.y = F, background.circle.colour = "dodgerblue",
                                       plot.extent.x.sf = 1.2, plot.extent.y.sf = 1.2, titolo = TRUE)
  }
  names(listggplots) <- profile[, 1]

  # Arrange radial plots
  if (is.null(ncol.arrange)) {
    ncol.arrange - ceiling(sqrt(length(listggplots)))
  }
  gridExtra::grid.arrange(grobs = listggplots, ncol = ncol.arrange)

  invisible(listggplots)
}
