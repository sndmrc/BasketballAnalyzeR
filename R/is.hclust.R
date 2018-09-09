#' Functions to check if an object is of class 'hclust'.
#'
#' @param x any R object
#' @return returns TRUE if its argument is of class 'hclust' and FALSE otherwise
#' @examples
#' data <- data.frame(Pbox$PTS,Pbox$P3M,
#'                    Pbox$OREB + Pbox$DREB, Pbox$AST,
#'                    Pbox$TOV, Pbox$STL, Pbox$BLK,Pbox$PF)
#' names(data) <- c("PTS","P3M","REB","AST","TOV","STL","BLK","PF")
#' data <- subset(data, Pbox$MIN >= 1500)
#' ID <- Pbox$Player[Pbox$MIN >= 1500]
#' out <- h.clustering(data, labels=ID, k=7)
#' is.hclust(out)
#' @export

is.hclust <- function(x) {
  inherits(x, "hclust")
}
