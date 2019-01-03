#' Functions to check if an object is of class 'kclust'.
#'
#' @param x any R object
#' @return returns TRUE if its argument is of class 'kclust' and FALSE otherwise
#' @examples
#' FF <- fourfactors(Tbox,Obox)
#' OD_Rtg <- FF$ORtg/FF$DRtg
#' F1_r <- FF$F1.D/FF$F1.O
#' F2_r <- FF$F2.O/FF$F2.D
#' F3_O <- FF$F3.D
#' F3_D <- FF$F3.O
#' P3M <- Tbox$P3M
#' STL_r <- Tbox$STL/Obox$STL
#' X <- data.frame(OD_Rtg,F1_r,F2_r,F3_O,F3_D,P3M,STL_r)
#' kclu1 <- kclustering(X)
#' is.kclust(kclu1)
#' @export

is.kclust <- function(x) {
  inherits(x, "kclust")
}
