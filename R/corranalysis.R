#' Correlation analysis
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param data a data frame
#' @param threshold numeric, correlation cutoff (default 0); correlations in absolute value below \code{threshold} are set to 0
#' @param sig.level numeric, significance level (default 0.95); correlations with p-values greater that \code{1-sig.level} are set to 0
#' @seealso \code{\link{plot.corranalysis}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return A list with the following elements:
#' * \code{corr.mtx} (the complete correlation matrix)
#' * \code{corr.mtx.trunc} (the truncated correlation matrix)
#' * \code{cor.mtest} (the output of the significance test on correlations; see \code{\link[corrplot]{cor.mtest}})
#' * \code{threshold} correlation cutoff
#' * \code{sig.level} significance level
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

corranalysis <- function(data, threshold = 0, sig.level = 0.95) {

  cor_mtx <- stats::cor(data, use = "pairwise.complete.obs")
  cor_mtest <- corrplot::cor.mtest(data)

  cor_mtx_trunc <- cor_mtx
  cor_mtx_trunc[cor_mtest$p > (1-sig.level)] <- 0
  cor_mtx_trunc[cor_mtx^2 < threshold^2] <- 0
  diag(cor_mtx_trunc) <- 0

  lst <- list(cor.mtx = cor_mtx, cor.mtx.trunc = cor_mtx_trunc,
              cor.mtest = cor_mtest, threshold = threshold, sig.level = sig.level)
  class(lst) <- append("corranalysis", class(lst))
  invisible(lst)

}
