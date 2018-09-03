#' Variability analysis
#'
#' @param data A dataframe
#' @param size A vector of variable names or of column numbers defining variables used for bubble sizes
#' @param weight If TRUE, calculates weighted variation coefficients
#' @return A list with the following elements: ranges, standard deviations, variation coefficients, and two dataframes (data, size)
#' @examples
#' Pbox.BC <- subset(Pbox, Team=="Oklahoma City Thunder" & MIN >= 500,
#'                     select=c("P2p","P3p","FTp","P2A","P3A","FTA"))
#' list_variability <- variability(data=Pbox.BC, size=c("P2A","P3A","FTA"), weight=TRUE)
#' print(list_variability)
#' plot(list_variability)
#' @export

variability <- function(data, size, weight = FALSE) {

  cvfun <- function(x) {
    s = sd(x)
    cv = s/mean(x)
    rg <- max(x) - min(x)
    c(s, cv, rg)
  }

  wcvfun <- function(x, w) {
    wmean <- stats::weighted.mean(x, w)
    wsd <- sqrt(stats::weighted.mean(x^2, w) - wmean^2)
    rg <- max(x) - min(x)
    c(wsd, wsd/wmean, rg)
  }

  data <- stats::na.omit(data)

  if (is.character(size)) {
    sel <- names(data) %in% size
    if (all(!sel))
      stop(paste(size, "is not a column of 'data'"))
    df1 <- data[, !sel]
    df2 <- data[, sel, drop = F]
  } else if (is.numeric(size)) {
    df1 <- data[, -size]
    df2 <- data[, size, drop = F]
  }

  nc1 <- ncol(df1)
  nc2 <- ncol(df2)
  if (weight) {
    if (nc1 == nc2) {
      mtx <- mapply(wcvfun, df1, df2)
    } else if (nc2 == 1) {
      mtx <- apply(df1, 2, wcvfun, w = df2[, 1])
    }
  } else {
    mtx <- apply(df1, 2, cvfun)
  }
  rownames(mtx) <- c("Range", "VC", "SD")

  lst <- list(weight = weight, range = mtx[3, ], VC = mtx[2, ], SD = mtx[1, ], data = df1, size = df2)
  class(lst) <- append("variability", class(lst))
  return(lst)
}

