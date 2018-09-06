#' Plot inequality
#'
#' @param cor_mtx cor_mtx
#' @param cor_mtest cor_mtest
#' @param sl sl
#' @return Nothing
#' @export

corr_plot_mixed <- function(cor_mtx, cor_mtest, sl) {
  par(mar = c(0, 0, 0, 0), bg = "white")
  #e1 <- environment()
  #print(ls.str(e1))
  #print(e1)
  #print(pryr::where("cor_mtx"))

  corr_plot(cor_mtx, type = "upper", method = "ellipse", diag = F, tl.pos = "n", p.mat = cor_mtest$p, sig.level = sl)
  corr_plot(cor_mtx, type = "lower", method = "number", diag = F, add = T, tl.pos = "n", cl.pos = "n", p.mat = cor_mtest$p, sig.level = sl)
}
