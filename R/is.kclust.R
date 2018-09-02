#' Functions to check if an object is of class 'kclust'.
#'
#' @param x any R object
#' @return returns TRUE if its argument is of class 'kclust' and FALSE otherwise
#' @examples
#' FF <- fourfactors(Tbox,Obox)
#' OD_Rtg <- FF$ORT/FF$DRT
#' F1_r <- FF$F1_O/FF$F1_T
#' F2_r <- FF$F2_T/FF$F2_O
#' F3_O <- FF$F3_O
#' F3_D <- FF$F3_T
#' P3M <- Tbox$P3M
#' STL_r <- Tbox$STL/Obox$STL
#' X <- data.frame(OD_Rtg,F1_r,F2_r,F3_O,F3_D,P3M,STL_r)
#' kclu1 <- kclustering(X)
#' is.kclust(kclu1)
#' @export

is.kclust <- function(x) {
  inherits(x, "kclust")
}
