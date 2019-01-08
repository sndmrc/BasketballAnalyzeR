#' Functions to check if an object is of class 'hclustering'.
#'
#' @param x any R object
#' @return returns TRUE if its argument is of class 'hclustering' and FALSE otherwise
#' @examples
#' data <- data.frame(Pbox$PTS,Pbox$P3M,
#'                    Pbox$OREB + Pbox$DREB, Pbox$AST,
#'                    Pbox$TOV, Pbox$STL, Pbox$BLK,Pbox$PF)
#' names(data) <- c("PTS","P3M","REB","AST","TOV","STL","BLK","PF")
#' data <- subset(data, Pbox$MIN >= 1500)
#' ID <- Pbox$Player[Pbox$MIN >= 1500]
#' out <- hclustering(data, labels=ID, k=7)
#' is.hclustering(out)
#' @export

is.hclustering <- function(x) {
  inherits(x, "hclustering")
}
