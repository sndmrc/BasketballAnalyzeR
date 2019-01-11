#' Create some two-dimensional plots for multidimensional scaling (MDS)
#'
#' @param data A dataframe to use for plot
#' @param std If TRUE, columns of 'data' are standardized
#' @return An object of class 'MDSmap': a list with 4 objects: points, stress, data, std
#' @examples
#' data <- data.frame(Pbox$PTS, Pbox$P3M, Pbox$P2M, Pbox$OREB + Pbox$DREB, Pbox$AST,
#' Pbox$TOV,Pbox$STL, Pbox$BLK)
#' names(data) <- c('PTS','P3M','P2M','REB','AST','TOV','STL','BLK')
#' selp <- which(Pbox$MIN >= 1500)
#' data <- data[selp,]
#' id <- Pbox$Player[selp]
#' mds <- MDSmap(data)
#' plot(mds, labels=id, z.var="P2M", level.plot=FALSE, palette=rainbow)
#' @export
#' @importFrom directlabels geom_dl
#' @importFrom ggplot2 geom_contour
#' @importFrom MASS isoMDS
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom stats loess
#' @importFrom stats loess.control
#' @importFrom stats predict
#' @importFrom stats cmdscale
#' @importFrom stats dist
#' @importFrom stats sd
#' @importFrom grDevices rainbow

MDSmap <- function(data, std=TRUE) {

  if (!is.matrix(data) & !is.data.frame(data) & (!inherits(data, "dist"))) {
    stop("'data' must be a matrix, a data frame, or a distance matrix")
  }

  if (!inherits(data, "dist")) {
    if (std) {
      data_for_dist <- scale(data)
    } else {
      data_for_dist <- data
    }
    dist.mat <- dist(data_for_dist)
  } else {
    dist.mat <- data
  }

  # MDS - 2 dimensions
  out <- MASS::isoMDS(dist.mat, k=2, y=cmdscale(dist.mat, 2), maxit=100)
  out[["data"]] <- data
  out[["std"]] <- std
  class(out) <- append("MDSmap", class(out))
  return(out)
}
