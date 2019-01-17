#' Correlation analysis
#'
#' @param data A dataframe
#' @param threshold threshold
#' @param sl Significance level
#' @return A list with the following elements: corr.mtx, corr.mtx.trunc, cor.mtest, threshold, siglevel
#' @examples
#' data <- data.frame(Pbox$PTS,Pbox$P3M,Pbox$P2M,
#'                    Pbox$OREB + Pbox$DREB,Pbox$AST,
#'                    Pbox$TOV,Pbox$STL,Pbox$BLK)/Pbox$MIN
#' names(data) <- c("PTS","P3M","P2M","REB","AST","TOV","STL","BLK")
#' data <- subset(data, Pbox$MIN >= 500)
#' out <- corranalysis(data, threshold = 0.5)
#' plot(out)
#' @export
#' @importFrom corrplot cor.mtest
#' @importFrom stats cor

corranalysis <- function(data, threshold = 0, sl = 0.05) {

  cor_mtx <- stats::cor(data, use = "pairwise.complete.obs")
  cor_mtest <- corrplot::cor.mtest(data)

  cor_mtx_trunc <- cor_mtx
  cor_mtx_trunc[cor_mtest$p > sl] <- 0
  cor_mtx_trunc[cor_mtx^2 < threshold^2] <- 0
  diag(cor_mtx_trunc) <- 0

  lst <- list(cor.mtx = cor_mtx, cor.mtx.trunc = cor_mtx_trunc,
              cor.mtest = cor_mtest, threshold = threshold, siglevel = sl)
  class(lst) <- append("corranalysis", class(lst))
  invisible(lst)

}
