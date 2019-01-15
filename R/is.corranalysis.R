#' Reports whether x is a corr.analysis object
#'
#' @param x An object to test
#' @return returns TRUE if its argument is of class 'corr.analysis' and FALSE otherwise
#' @examples
#' data <- data.frame(Pbox$PTS,Pbox$P3M,Pbox$P2M,
#'                    Pbox$OREB + Pbox$DREB,Pbox$AST,
#'                    Pbox$TOV,Pbox$STL,Pbox$BLK)/Pbox$MIN
#' names(data) <- c("PTS","P3M","P2M","REB","AST","TOV","STL","BLK")
#' data <- subset(data, Pbox$MIN >= 500)
#' out <- corranalysis(data)
#' is.corranalysis(out)
#' @export

is.corranalysis <- function(x) {
  inherits(x, "corranalysis")
}
