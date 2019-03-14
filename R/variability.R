#' Variability analysis
#'
#' @param data A dataframe
#' @param data.var A vector of variable names or of column numbers defining variables whose variability will be analyzed by 'variability'
#' @param size.var A vector of variable names or of column numbers defining variables used for bubble sizes
#' @param VC If TRUE, calculate variation coefficients of variables in data.var
#' @param weight If TRUE, calculates weighted variation coefficients
#' @return A list with the following elements: ranges, standard deviations, variation coefficients, and two dataframes (data, size)
#' @examples
#' Pbox.BC <- subset(Pbox, Team=="Oklahoma City Thunder" & MIN >= 500,
#'                     select=c("P2p","P3p","FTp","P2A","P3A","FTA"))
#' list_variability <- variability(data=Pbox.BC, data.var=c("P2p","P3p","FTp"),
#'                                 size.var=c("P2A","P3A","FTA"), weight=TRUE)
#' print(list_variability)
#' plot(list_variability, leg.brk=c(10,25,50,100,500,1000), max.circle=30)
#' @export

variability <- function(data, data.var, size.var, VC=TRUE, weight = FALSE) {

  cvfun <- function(x, VC) {
    mn <- mean(x, na.rm=TRUE)
    s = sqrt(mean((x-mn)^2, na.rm=TRUE)) # Population SD
    rg <- max(x, na.rm=TRUE) - min(x, na.rm=TRUE)
    if (VC) {
      cv = s/abs(mn)
      c(s, rg, cv)
    } else {
      c(s, rg)
    }
  }

  wcvfun <- function(x, w) {
    wmean <- stats::weighted.mean(x, w)
    wsd <- sqrt(stats::weighted.mean(x^2, w) - wmean^2)
    rg <- max(x) - min(x)
    c(wsd, rg, wsd/abs(wmean))
  }

  data <- stats::na.omit(data)

  if (is.character(data.var) & is.character(size.var)) {
    sel.data.var <- names(data) %in% data.var
    if (all(!sel.data.var))
      stop(paste(data.var, "not column(s) of 'data'"))
    sel.size.var <- names(data) %in% size.var
    if (all(!sel.size.var))
      stop(paste(size.var, "not column(s) of 'data'"))
    df.data <- data[, sel.data.var, drop = F]
    df.size <- data[, sel.size.var, drop = F]
  } else if (is.numeric(data.var) & is.numeric(size.var)) {
    df.data <- data[, data.var, drop = F]
    df.size <- data[, size.var, drop = F]
  }

  nc.data <- ncol(df.data)
  nc.size <- ncol(df.size)
  if ((nc.data!=nc.size & nc.size!=1)) {
    stop("'data.var' and 'size.var' must have the same number of elements")
  }

  if (weight) {
    if (nc.data == nc.size) {
      mtx <- mapply(wcvfun, df.data, df.size)
    } else if (nc.size == 1) {
      mtx <- apply(df.data, 2, wcvfun, w = df.size[, 1])
    }
  } else {
    mtx <- apply(df.data, 2, cvfun, VC=VC)
  }
  if (VC) {
    lst <- list(weight = weight, SD = mtx[1, ], range = mtx[2, ], VC = mtx[3, ], data = df.data, size = df.size)
  } else {
    lst <- list(weight = weight, SD = mtx[1, ], range = mtx[2, ], data = df.data, size = df.size)
  }

  class(lst) <- append("variability", class(lst))
  return(lst)
}

