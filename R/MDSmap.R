#' Multidimensional scaling (MDS)
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param data A data frame
#' @param std logical; if TRUE, \code{data} columns are standardized (centered and scaled)
#' @return An object of class \code{MDSmap}, i.e. a list with 4 objects:
#' @return * \code{points}, a k-column vector of the fitted configuration (see \code{\link[MASS]{isoMDS}});
#' @return * \code{stress}, the final stress achieved in percent  (see \code{\link[MASS]{isoMDS}});
#' @return * \code{data}, the input data frame;
#' @return * \code{std}, the logical \code{std} input.
#' @seealso \code{\link[MASS]{isoMDS}}, \code{\link{plot.MDSmap}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @examples
#' data <- with(Pbox, data.frame(PTS, P3M, P2M, REB=OREB+DREB, AST, TOV, STL, BLK))
#' selp <- which(Pbox$MIN >= 1500)
#' data <- data[selp, ]
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
