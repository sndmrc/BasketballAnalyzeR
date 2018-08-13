radialprofile <- function(data, group, perc = F, std = T, title = NULL, arrange = FALSE,
                          ncol.arrange = NULL) {

  # Reorder columns
  pos_group <- which(names(data) == group)
  ord_cols <- c(pos_group, (1:ncol(data))[-pos_group])
  profile <- data[, ord_cols]
  names(profile)[1] <- "group"

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

  # Add title
  if (!is.null(title)) {
    profile[, 1] <- paste(profile[, 1], " - ", title, sep = "")
  }

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
  if (arrange) {
    if (is.null(ncol.arrange)) {
      out <- grid.arrange(grobs = listPlots, ncol = ceiling(sqrt(length(listPlots))))
    } else {
      out <- grid.arrange(grobs = listPlots, ncol = ncol.arrange)
    }
  } else {
    out <- listPlots
  }
  return(out)

}
