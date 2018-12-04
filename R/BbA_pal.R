#' @noRd

BbA_cols <- function(...) {
  BbAcols <- c(
    `red`        = "#d11141",
    `green`      = "#00b159",
    `blue`       = "#00aedb",
    `orange`     = "#f37735",
    `yellow`     = "#ffc425",
    `light grey` = "#cccccc",
    `dark grey`  = "#8c8c8c")
  cols <- c(...)
  if (is.null(cols)) return (BbAcols)
  BbAcols[cols]
}

#' @noRd
BbA_pal <- function(palette = "main", reverse = FALSE, ...) {
  BbApal <- list(
    `main`  = BbA_cols("blue", "green", "yellow"),
    `cool`  = BbA_cols("blue", "green"),
    `hot`   = BbA_cols("yellow", "orange", "red"),
    `mixed` = BbA_cols("blue", "green", "yellow", "orange", "red"),
    `grey`  = BbA_cols("light grey", "dark grey")
  )
  if (length(palette)==1 & is.character(palette)) {
    pal <- BbApal[[palette]]
  } else {
    pal <- palette
  }
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}

