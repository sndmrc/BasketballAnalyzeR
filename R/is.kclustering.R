#' Functions to check if an object is of class 'kclust'.
#'
#' @param x any R object
#' @return returns TRUE if its argument is of class 'kclust' and FALSE otherwise
#' @examples
#' FF <- fourfactors(Tbox,Obox)
#' OD.Rtg <- FF$ORtg/FF$DRtg
#' F1.r <- FF$F1.Def/FF$F1.Off
#' F2.r <- FF$F2.Off/FF$F2.Def
#' F3.O <- FF$F3.Def
#' F3.D <- FF$F3.Off
#' P3M <- Tbox$P3M
#' STL.r <- Tbox$STL/Obox$STL
#' X <- data.frame(OD.Rtg,F1.r,F2.r,F3.O,F3.D,P3M,STL.r)
#' kclu1 <- kclustering(X)
#' is.kclustering(kclu1)
#' @export

is.kclustering <- function(x) {
  inherits(x, "kclustering")
}
