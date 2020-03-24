#' Functions to check if an object is of class 'kclustering'.
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketball.analyzer.help@gmail.com})
#' @param x an object to test
#' @seealso \code{\link{kclustering}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return returns TRUE if its argument is of class \code{kclustering} and FALSE otherwise
#' @examples
#' FF <- fourfactors(Tbox,Obox)
#' X <- with(FF, data.frame(OD.Rtg=ORtg/DRtg,
#'                F1.r=F1.Def/F1.Off, F2.r=F2.Off/F2.Def,
#'                F3.O=F3.Def, F3.D=F3.Off))
#' X$P3M <- Tbox$P3M
#' X$STL.r <- Tbox$STL/Obox$STL
#' kclu <- kclustering(X)
#' is.kclustering(kclu)
#' @export

is.kclustering <- function(x) {
  inherits(x, "kclustering")
}
