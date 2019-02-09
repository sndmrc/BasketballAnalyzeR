#' Plot inequality
#'
#' @param x A 'inequality' objects
#' @param horizontal If TRUE, the two plots are arranged horizontally
#' @param title Plot title
#' @param ... other graphical parameters
#' @return A ggplot2 object
#' @examples
#' data <- data.frame(Pbox$PTS,Pbox$P3M,Pbox$P2M,
#'                    Pbox$OREB + Pbox$DREB,Pbox$AST,
#'                    Pbox$TOV,Pbox$STL,Pbox$BLK)/Pbox$MIN
#' names(data) <- c("PTS","P3M","P2M","REB","AST","TOV","STL","BLK")
#' data <- subset(data, Pbox$MIN >= 500)
#' out <- corranalysis(data, threshold=0.5)
#' plot(out)
#' @export
#' @method plot corranalysis
#' @importFrom ggplotify as.ggplot
#' @importFrom network network
#' @importFrom network set.edge.attribute
#' @importFrom network "%e%"
#' @importFrom GGally ggnet2
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics par
#' @importFrom graphics plot.new
#' @importFrom graphics plot.window
#' @importFrom graphics points
#' @importFrom graphics polygon
#' @importFrom graphics rect
#' @importFrom graphics segments
#' @importFrom graphics strwidth
#' @importFrom graphics symbols
#' @importFrom graphics text
#' @importFrom graphics plot
#' @importFrom corrplot corrMatOrder
#' @importFrom corrplot corrRect.hclust
#' @importFrom corrplot colorlegend
#' @importFrom ggnetwork geom_nodes
#' @importFrom ggnetwork geom_edges
#' @importFrom ggnetwork ggnetwork
#' @importFrom ggnetwork theme_blank
#' @importFrom ggnetwork geom_nodetext_repel
#' @importFrom ggplot2 scale_alpha
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 scale_colour_gradientn
#' @importFrom statnet.common order


plot.corranalysis <- function(x, horizontal = TRUE, title = NULL, ...) {

  y <- xend <- yend <- edge.color <- vertex.names <- NULL
  corr_plot_mixed <- function(cor_mtx, cor_mtest, sl) {
    par(mar = c(0, 0, 0, 0), bg = "white")
    corr_plot(cor_mtx, type = "upper", method = "ellipse", diag = F, tl.pos = "n", p.mat = cor_mtest$p, sig.level = sl)
    corr_plot(cor_mtx, type = "lower", method = "number", diag = F, add = T, tl.pos = "n", cl.pos = "n", p.mat = cor_mtest$p, sig.level = sl)
  }

  if (!is.corranalysis(x)) {
    stop("Not a 'corranalysis' object")
  }

  if (horizontal) ncol=2 else ncol=1

  cor_mtx <- x[["cor.mtx"]]
  cor_mtest <- x[["cor.mtest"]]
  sig_lev <- x[["sig.level"]]
  cor_mtx_trunc <- x[["cor.mtx.trunc"]]

  # Correlation plot
  txt <- substitute(corr_plot_mixed(cor_mtx, cor_mtest, 1-sig_lev))
  p1 <- ggplotify::as.ggplot(as.expression(txt))

  # Network of correlations
  allzero <- all(cor_mtx_trunc==0)
  if (allzero) {
    cor_mtx_trunc[1:2,1:2] <- 0.01
  }
  net <- network::network(cor_mtx_trunc, matrix.type = "adjacency", ignore.eval = FALSE, names.eval = "weights")
  netwt <- (net %e% "weights")
  cols <- rev(colorRampPalette(c("#67001F", "#B2182B", "#D6604D",
                                 "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                                 "#4393C3", "#2166AC", "#053061"))(200))
  if (!allzero) {
    network::set.edge.attribute(net, "edge.color", netwt)
    datanet <- ggnetwork::ggnetwork(net, layout = "circle")
    p2 <- ggplot(datanet, aes(x = x, y = y, xend = xend, yend = yend)) +
      ggnetwork::geom_nodes(shape=21, fill="#D6EAF877", color="#3498DB77", size=15) +
      ggnetwork::geom_edges(aes(color=edge.color), size=1.5) +
      scale_colour_gradientn("", colors = cols, limits=c(-1,1)) +
      scale_alpha(guide=FALSE) +
      ggnetwork::geom_nodetext_repel(aes(label=vertex.names)) +
      ggnetwork::theme_blank()
  } else {
    datanet <- ggnetwork::ggnetwork(net, layout = "circle")
    p2 <- ggplot(datanet, aes(x = x, y = y, xend = xend, yend = yend)) +
      ggnetwork::geom_nodes(shape=21, fill="#D6EAF877", color="#3498DB77", size=15) +
      ggnetwork::geom_nodetext_repel(aes(label=vertex.names)) +
      ggnetwork::theme_blank()
  }
  listPlot <- list(corrplot = p1, netplot = p2)
  gridExtra::grid.arrange(grobs=listPlot, ncol = ncol)

  invisible(listPlot)
}

#' @note pure function
#' @noRd
corr_plot <- function (corr, method = c("circle", "square", "ellipse", "number",
                                        "shade", "color", "pie"), type = c("full", "lower", "upper"),
                       add = FALSE, col = NULL, bg = "white", title = "", is.corr = TRUE,
                       diag = TRUE, outline = FALSE, mar = c(0, 0, 0, 0), addgrid.col = NULL,
                       addCoef.col = NULL, addCoefasPercent = FALSE,
                       order = c("original", "AOE", "FPC", "hclust", "alphabet"),
                       hclust.method = c("complete","ward", "ward.D", "ward.D2", "single", "average",
                                         "mcquitty","median", "centroid"),
                       addrect = NULL, rect.col = "black",
                       rect.lwd = 2, tl.pos = NULL, tl.cex = 1, tl.col = "red",
                       tl.offset = 0.4, tl.srt = 90, cl.pos = NULL, cl.lim = NULL,
                       cl.length = NULL, cl.cex = 0.8, cl.ratio = 0.15, cl.align.text = "c",
                       cl.offset = 0.5, number.cex = 1, number.font = 2, number.digits = NULL,
                       addshade = c("negative", "positive", "all"), shade.lwd = 1,
                       shade.col = "white", p.mat = NULL, sig.level = 0.05,
                       insig = c("pch", "p-value", "blank", "n", "label_sig"),
                       pch = 4, pch.col = "black", pch.cex = 3, plotCI = c("n", "square", "circle", "rect"),
                       lowCI.mat = NULL, uppCI.mat = NULL, na.label = "?", na.label.col = "black",
                       win.asp = 1, ...)
{
  method <- match.arg(method)
  type <- match.arg(type)
  order <- match.arg(order)
  hclust.method <- match.arg(hclust.method)
  addshade <- match.arg(addshade)
  insig <- match.arg(insig)
  plotCI <- match.arg(plotCI)
  if (win.asp != 1 && !(method %in% c("circle", "square"))) {
    stop("Parameter 'win.asp' is supported only for circle and square methods.")
  }
  asp_rescale_factor <- min(1, win.asp)/max(1, win.asp)
  stopifnot(asp_rescale_factor >= 0 && asp_rescale_factor <=
              1)
  if (!is.matrix(corr) && !is.data.frame(corr)) {
    stop("Need a matrix or data frame!")
  }
  if (is.null(addgrid.col)) {
    addgrid.col <- switch(method, color = NA, shade = NA,
                          "grey")
  }
  if (any(corr < cl.lim[1]) || any(corr > cl.lim[2])) {
    stop("color limits should cover matrix")
  }
  if (is.null(cl.lim)) {
    if (is.corr) {
      cl.lim <- c(-1, 1)
    }
    else {
      corr_tmp <- corr
      diag(corr_tmp) <- ifelse(diag, diag(corr_tmp), NA)
      cl.lim <- c(min(corr_tmp, na.rm = TRUE), max(corr_tmp,
                                                   na.rm = TRUE))
    }
  }
  intercept <- 0
  zoom <- 1
  if (!is.corr) {
    c_max <- max(corr, na.rm = TRUE)
    c_min <- min(corr, na.rm = TRUE)
    if (c_max <= 0) {
      intercept <- -cl.lim[2]
      zoom <- 1/(diff(cl.lim))
    }
    else if (c_min >= 0) {
      intercept <- -cl.lim[1]
      zoom <- 1/(diff(cl.lim))
    }
    else {
      stopifnot(c_max * c_min < 0)
      stopifnot(c_min < 0 && c_max > 0)
      intercept <- 0
      zoom <- 1/max(abs(cl.lim))
    }
    if (zoom == Inf) {
      stopifnot(cl.lim[1] == 0 && cl.lim[2] == 0)
      zoom <- 0
    }
    corr <- (intercept + corr) * zoom
  }
  cl.lim2 <- (intercept + cl.lim) * zoom
  int <- intercept * zoom
  if (is.corr) {
    if (min(corr, na.rm = TRUE) < -1 - .Machine$double.eps^0.75 ||
        max(corr, na.rm = TRUE) > 1 + .Machine$double.eps^0.75) {
      stop("The matrix is not in [-1, 1]!")
    }
  }
  if (is.null(col)) {
    col <- rev(colorRampPalette(c("#67001F", "#B2182B", "#D6604D",
                              "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                              "#4393C3", "#2166AC", "#053061"))(200))
  }
  n <- nrow(corr)
  m <- ncol(corr)
  min.nm <- min(n, m)
  ord <- seq_len(min.nm)
  if (order != "original") {
    ord <- corrMatOrder(corr, order = order, hclust.method = hclust.method)
    corr <- corr[ord, ord]
  }
  if (is.null(rownames(corr))) {
    rownames(corr) <- seq_len(n)
  }
  if (is.null(colnames(corr))) {
    colnames(corr) <- seq_len(m)
  }
  apply_mat_filter <- function(mat) {
    x <- matrix(1:n * m, nrow = n, ncol = m)
    switch(type, upper = mat[row(x) > col(x)] <- Inf, lower = mat[row(x) <
                                                                    col(x)] <- Inf)
    if (!diag) {
      diag(mat) <- Inf
      mat[1, 1] <- 0
      mat[nrow(mat), ncol(mat)] <- 0
    }
    return(mat)
  }
  getPos.Dat <- function(mat) {
    tmp <- apply_mat_filter(mat)
    Dat <- tmp[is.finite(tmp)]
    ind <- which(is.finite(tmp), arr.ind = TRUE)
    Pos <- ind
    Pos[, 1] <- ind[, 2]
    Pos[, 2] <- -ind[, 1] + 1 + n
    return(list(Pos, Dat))
  }
  getPos.NAs <- function(mat) {
    tmp <- apply_mat_filter(mat)
    ind <- which(is.na(tmp), arr.ind = TRUE)
    Pos <- ind
    Pos[, 1] <- ind[, 2]
    Pos[, 2] <- -ind[, 1] + 1 + n
    return(Pos)
  }
  Pos <- getPos.Dat(corr)[[1]]
  if (any(is.na(corr)) && is.character(na.label)) {
    PosNA <- getPos.NAs(corr)
  }
  else {
    PosNA <- NULL
  }
  AllCoords <- rbind(Pos, PosNA)
  n2 <- max(AllCoords[, 2])
  n1 <- min(AllCoords[, 2])
  nn <- n2 - n1
  m2 <- max(AllCoords[, 1])
  m1 <- min(AllCoords[, 1])
  mm <- max(1, m2 - m1)
  expand_expression <- function(s) {
    ifelse(grepl("^[:=$]", s), parse(text = substring(s,
                                                      2)), s)
  }
  newrownames <- sapply(rownames(corr)[(n + 1 - n2):(n + 1 -
                                                       n1)], expand_expression)
  newcolnames <- sapply(colnames(corr)[m1:m2], expand_expression)
  DAT <- getPos.Dat(corr)[[2]]
  len.DAT <- length(DAT)
  rm(expand_expression)
  assign.color <- function(dat = DAT, color = col) {
    newcorr <- (dat + 1)/2
    newcorr[newcorr <= 0] <- 0
    newcorr[newcorr >= 1] <- 1 - 1e-16
    color[floor(newcorr * length(color)) + 1]
  }
  col.fill <- assign.color()
  isFALSE <- function(x) identical(x, FALSE)
  isTRUE <- function(x) identical(x, TRUE)
  if (isFALSE(tl.pos)) {
    tl.pos <- "n"
  }
  if (is.null(tl.pos) || isTRUE(tl.pos)) {
    tl.pos <- switch(type, full = "lt", lower = "ld", upper = "td")
  }
  if (isFALSE(cl.pos)) {
    cl.pos <- "n"
  }
  if (is.null(cl.pos) || isTRUE(cl.pos)) {
    cl.pos <- switch(type, full = "r", lower = "b", upper = "r")
  }
  if (isFALSE(outline)) {
    col.border <- col.fill
  }
  else if (isTRUE(outline)) {
    col.border <- "black"
  }
  else if (is.character(outline)) {
    col.border <- outline
  }
  else {
    stop("Unsupported value type for parameter outline")
  }
  oldpar <- par(mar = mar, bg = "white")
  on.exit(par(oldpar), add = TRUE)
  if (!add) {
    plot.new()
    xlabwidth <- max(strwidth(newrownames, cex = tl.cex))
    ylabwidth <- max(strwidth(newcolnames, cex = tl.cex))
    laboffset <- strwidth("W", cex = tl.cex) * tl.offset
    for (i in 1:50) {
      xlim <- c(m1 - 0.5 - laboffset - xlabwidth * (grepl("l",
                                                          tl.pos) | grepl("d", tl.pos)), m2 + 0.5 + mm *
                  cl.ratio * (cl.pos == "r") + xlabwidth * abs(cos(tl.srt *
                                                                     pi/180)) * grepl("d", tl.pos)) + c(-0.35, 0.15) +
        c(-1, 0) * grepl("l", tl.pos)
      ylim <- c(n1 - 0.5 - nn * cl.ratio * (cl.pos == "b") -
                  laboffset, n2 + 0.5 + laboffset + ylabwidth *
                  abs(sin(tl.srt * pi/180)) * grepl("t", tl.pos)) +
        c(-0.15, 0) + c(0, -1) * (type == "upper" &&
                                    tl.pos != "n") + c(0, 1) * grepl("d", tl.pos)
      plot.window(xlim, ylim, asp = 1, xaxs = "i", yaxs = "i")
      x.tmp <- max(strwidth(newrownames, cex = tl.cex))
      y.tmp <- max(strwidth(newcolnames, cex = tl.cex))
      laboffset.tmp <- strwidth("W", cex = tl.cex) * tl.offset
      if (max(x.tmp - xlabwidth, y.tmp - ylabwidth, laboffset.tmp -
              laboffset) < 0.001) {
        break
      }
      xlabwidth <- x.tmp
      ylabwidth <- y.tmp
      laboffset <- laboffset.tmp
      if (i == 50) {
        warning(c("Not been able to calculate text margin, ",
                  "please try again with a clean new empty window using ",
                  "{plot.new(); dev.off()} or reduce tl.cex"))
      }
    }
    if (.Platform$OS.type == "windows") {
      grDevices::windows.options(width = 7, height = 7 *
                                   diff(ylim)/diff(xlim))
    }
    plot.window(xlim = xlim, ylim = ylim, asp = win.asp,
                xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  }
  laboffset <- strwidth("W", cex = tl.cex) * tl.offset
  symbols(Pos, add = TRUE, inches = FALSE, rectangles = matrix(1,
                                                               len.DAT, 2), bg = bg, fg = bg)
  if (method == "circle" && plotCI == "n") {
    symbols(Pos, add = TRUE, inches = FALSE, circles = asp_rescale_factor *
              0.9 * abs(DAT)^0.5/2, fg = col.border, bg = col.fill)
  }
  if (method == "ellipse" && plotCI == "n") {
    ell.dat <- function(rho, length = 99) {
      k <- seq(0, 2 * pi, length = length)
      x <- cos(k + acos(rho)/2)/2
      y <- cos(k - acos(rho)/2)/2
      cbind(rbind(x, y), c(NA, NA))
    }
    ELL.dat <- lapply(DAT, ell.dat)
    ELL.dat2 <- 0.85 * matrix(unlist(ELL.dat), ncol = 2,
                              byrow = TRUE)
    ELL.dat2 <- ELL.dat2 + Pos[rep(1:length(DAT), each = 100),
                               ]
    polygon(ELL.dat2, border = col.border, col = col.fill)
  }
  if (is.null(number.digits)) {
    number.digits <- switch(addCoefasPercent + 1, 2, 0)
  }
  stopifnot(number.digits%%1 == 0)
  stopifnot(number.digits >= 0)
  if (method == "number" && plotCI == "n") {
    text(Pos[, 1], Pos[, 2], font = number.font, col = col.fill,
         labels = round((DAT - int) * ifelse(addCoefasPercent,
                                             100, 1)/zoom, number.digits), cex = number.cex)
  }
  NA_LABEL_MAX_CHARS <- 2
  if (is.matrix(PosNA) && nrow(PosNA) > 0) {
    stopifnot(is.matrix(PosNA))
    if (na.label == "square") {
      symbols(PosNA, add = TRUE, inches = FALSE, squares = rep(1,
                                                               nrow(PosNA)), bg = na.label.col, fg = na.label.col)
    }
    else if (nchar(na.label) %in% 1:NA_LABEL_MAX_CHARS) {
      symbols(PosNA, add = TRUE, inches = FALSE, squares = rep(1,
                                                               nrow(PosNA)), fg = bg, bg = bg)
      text(PosNA[, 1], PosNA[, 2], font = number.font,
           col = na.label.col, labels = na.label, cex = number.cex,
           ...)
    }
    else {
      stop(paste("Maximum number of characters for NA label is:",
                 NA_LABEL_MAX_CHARS))
    }
  }
  if (method == "pie" && plotCI == "n") {
    symbols(Pos, add = TRUE, inches = FALSE, circles = rep(0.5,
                                                           len.DAT) * 0.85, fg = col.border)
    pie.dat <- function(theta, length = 100) {
      k <- seq(pi/2, pi/2 - theta, length = 0.5 * length *
                 abs(theta)/pi)
      x <- c(0, cos(k)/2, 0)
      y <- c(0, sin(k)/2, 0)
      cbind(rbind(x, y), c(NA, NA))
    }
    PIE.dat <- lapply(DAT * 2 * pi, pie.dat)
    len.pie <- unlist(lapply(PIE.dat, length))/2
    PIE.dat2 <- 0.85 * matrix(unlist(PIE.dat), ncol = 2,
                              byrow = TRUE)
    PIE.dat2 <- PIE.dat2 + Pos[rep(1:length(DAT), len.pie),
                               ]
    polygon(PIE.dat2, border = "black", col = col.fill)
  }
  if (method == "shade" && plotCI == "n") {
    symbols(Pos, add = TRUE, inches = FALSE, squares = rep(1,
                                                           len.DAT), bg = col.fill, fg = addgrid.col)
    shade.dat <- function(w) {
      x <- w[1]
      y <- w[2]
      rho <- w[3]
      x1 <- x - 0.5
      x2 <- x + 0.5
      y1 <- y - 0.5
      y2 <- y + 0.5
      dat <- NA
      if ((addshade == "positive" || addshade == "all") &&
          rho > 0) {
        dat <- cbind(c(x1, x1, x), c(y, y1, y1), c(x,
                                                   x2, x2), c(y2, y2, y))
      }
      if ((addshade == "negative" || addshade == "all") &&
          rho < 0) {
        dat <- cbind(c(x1, x1, x), c(y, y2, y2), c(x,
                                                   x2, x2), c(y1, y1, y))
      }
      return(t(dat))
    }
    pos_corr <- rbind(cbind(Pos, DAT))
    pos_corr2 <- split(pos_corr, 1:nrow(pos_corr))
    SHADE.dat <- matrix(stats::na.omit(unlist(lapply(pos_corr2,
                                                     shade.dat))), byrow = TRUE, ncol = 4)
    segments(SHADE.dat[, 1], SHADE.dat[, 2], SHADE.dat[,
                                                       3], SHADE.dat[, 4], col = shade.col, lwd = shade.lwd)
  }
  if (method == "square" && plotCI == "n") {
    draw_method_square(Pos, DAT, asp_rescale_factor, col.border,
                       col.fill)
  }
  if (method == "color" && plotCI == "n") {
    draw_method_color(Pos, col.border, col.fill)
  }
  draw_grid(AllCoords, addgrid.col)
  if (plotCI != "n") {
    if (is.null(lowCI.mat) || is.null(uppCI.mat)) {
      stop("Need lowCI.mat and uppCI.mat!")
    }
    if (order != "original") {
      lowCI.mat <- lowCI.mat[ord, ord]
      uppCI.mat <- uppCI.mat[ord, ord]
    }
    pos.lowNew <- getPos.Dat(lowCI.mat)[[1]]
    lowNew <- getPos.Dat(lowCI.mat)[[2]]
    pos.uppNew <- getPos.Dat(uppCI.mat)[[1]]
    uppNew <- getPos.Dat(uppCI.mat)[[2]]
    if (!method %in% c("circle", "square")) {
      stop("Method shoud be circle or square if drawing confidence intervals.")
    }
    k1 <- (abs(uppNew) > abs(lowNew))
    bigabs <- uppNew
    bigabs[which(!k1)] <- lowNew[!k1]
    smallabs <- lowNew
    smallabs[which(!k1)] <- uppNew[!k1]
    sig <- sign(uppNew * lowNew)
    color_bigabs <- col[ceiling((bigabs + 1) * length(col)/2)]
    color_smallabs <- col[ceiling((smallabs + 1) * length(col)/2)]
    if (plotCI == "circle") {
      symbols(pos.uppNew[, 1], pos.uppNew[, 2], add = TRUE,
              inches = FALSE, circles = 0.95 * abs(bigabs)^0.5/2,
              bg = ifelse(sig > 0, col.fill, color_bigabs),
              fg = ifelse(sig > 0, col.fill, color_bigabs))
      symbols(pos.lowNew[, 1], pos.lowNew[, 2], add = TRUE,
              inches = FALSE, circles = 0.95 * abs(smallabs)^0.5/2,
              bg = ifelse(sig > 0, bg, color_smallabs), fg = ifelse(sig >
                                                                      0, col.fill, color_smallabs))
    }
    if (plotCI == "square") {
      symbols(pos.uppNew[, 1], pos.uppNew[, 2], add = TRUE,
              inches = FALSE, squares = abs(bigabs)^0.5, bg = ifelse(sig >
                                                                       0, col.fill, color_bigabs), fg = ifelse(sig >
                                                                                                                 0, col.fill, color_bigabs))
      symbols(pos.lowNew[, 1], pos.lowNew[, 2], add = TRUE,
              inches = FALSE, squares = abs(smallabs)^0.5,
              bg = ifelse(sig > 0, bg, color_smallabs), fg = ifelse(sig >
                                                                      0, col.fill, color_smallabs))
    }
    if (plotCI == "rect") {
      rect.width <- 0.25
      rect(pos.uppNew[, 1] - rect.width, pos.uppNew[, 2] +
             smallabs/2, pos.uppNew[, 1] + rect.width, pos.uppNew[,
                                                                  2] + bigabs/2, col = col.fill, border = col.fill)
      segments(pos.lowNew[, 1] - rect.width, pos.lowNew[,
                                                        2] + DAT/2, pos.lowNew[, 1] + rect.width, pos.lowNew[,
                                                                                                             2] + DAT/2, col = "black", lwd = 1)
      segments(pos.uppNew[, 1] - rect.width, pos.uppNew[,
                                                        2] + uppNew/2, pos.uppNew[, 1] + rect.width,
               pos.uppNew[, 2] + uppNew/2, col = "black", lwd = 1)
      segments(pos.lowNew[, 1] - rect.width, pos.lowNew[,
                                                        2] + lowNew/2, pos.lowNew[, 1] + rect.width,
               pos.lowNew[, 2] + lowNew/2, col = "black", lwd = 1)
      segments(pos.lowNew[, 1] - 0.5, pos.lowNew[, 2],
               pos.lowNew[, 1] + 0.5, pos.lowNew[, 2], col = "grey70",
               lty = 3)
    }
  }
  if (!is.null(p.mat) && insig != "n") {
    if (order != "original") {
      p.mat <- p.mat[ord, ord]
    }
    pos.pNew <- getPos.Dat(p.mat)[[1]]
    pNew <- getPos.Dat(p.mat)[[2]]
    if (insig == "label_sig") {
      if (!is.character(pch))
        pch <- "*"
      place_points <- function(sig.locs, point) {
        text(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs],
             labels = point, col = pch.col, cex = pch.cex,
             lwd = 2)
      }
      if (length(sig.level) == 1) {
        place_points(sig.locs = which(pNew < sig.level),
                     point = pch)
      }
      else {
        l <- length(sig.level)
        for (i in seq_along(sig.level)) {
          iter <- l + 1 - i
          pchTmp <- paste(rep(pch, i), collapse = "")
          if (i == length(sig.level)) {
            locs <- which(pNew < sig.level[iter])
            if (length(locs)) {
              place_points(sig.locs = locs, point = pchTmp)
            }
          }
          else {
            locs <- which(pNew < sig.level[iter] & pNew >
                            sig.level[iter - 1])
            if (length(locs)) {
              place_points(sig.locs = locs, point = pchTmp)
            }
          }
        }
      }
    }
    else {
      ind.p <- which(pNew > sig.level)
      p_inSig <- length(ind.p) > 0
      if (insig == "pch" && p_inSig) {
        points(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p],
               pch = pch, col = pch.col, cex = pch.cex, lwd = 2)
      }
      if (insig == "p-value" && p_inSig) {
        text(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p],
             round(pNew[ind.p], 2), col = pch.col)
      }
      if (insig == "blank" && p_inSig) {
        symbols(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p],
                inches = FALSE, squares = rep(1, length(pos.pNew[,
                                                                 1][ind.p])), fg = addgrid.col, bg = bg, add = TRUE)
      }
    }
  }
  if (cl.pos != "n") {
    colRange <- assign.color(dat = cl.lim2)
    ind1 <- which(col == colRange[1])
    ind2 <- which(col == colRange[2])
    colbar <- col[ind1:ind2]
    if (is.null(cl.length)) {
      cl.length <- ifelse(length(colbar) > 20, 11, length(colbar) +
                            1)
    }
    labels <- seq(cl.lim[1], cl.lim[2], length = cl.length)
    if (cl.pos == "r") {
      vertical <- TRUE
      xlim <- c(m2 + 0.5 + mm * 0.02, m2 + 0.5 + mm * cl.ratio)
      ylim <- c(n1 - 0.5, n2 + 0.5)
    }
    if (cl.pos == "b") {
      vertical <- FALSE
      xlim <- c(m1 - 0.5, m2 + 0.5)
      ylim <- c(n1 - 0.5 - nn * cl.ratio, n1 - 0.5 - nn *
                  0.02)
    }
    colorlegend(colbar = colbar, labels = round(labels, 2),
                offset = cl.offset, ratio.colbar = 0.3, cex = cl.cex,
                xlim = xlim, ylim = ylim, vertical = vertical, align = cl.align.text)
  }
  if (tl.pos != "n") {
    pos.xlabel <- cbind(m1:m2, n2 + 0.5 + laboffset)
    pos.ylabel <- cbind(m1 - 0.5, n2:n1)
    if (tl.pos == "td") {
      if (type != "upper") {
        stop("type should be \"upper\" if tl.pos is \"dt\".")
      }
      pos.ylabel <- cbind(m1:(m1 + nn) - 0.5, n2:n1)
    }
    if (tl.pos == "ld") {
      if (type != "lower") {
        stop("type should be \"lower\" if tl.pos is \"ld\".")
      }
      pos.xlabel <- cbind(m1:m2, n2:(n2 - mm) + 0.5 + laboffset)
    }
    if (tl.pos == "d") {
      pos.ylabel <- cbind(m1:(m1 + nn) - 0.5, n2:n1)
      pos.ylabel <- pos.ylabel[1:min(n, m), ]
      symbols(pos.ylabel[, 1] + 0.5, pos.ylabel[, 2], add = TRUE,
              bg = bg, fg = addgrid.col, inches = FALSE, squares = rep(1,
                                                                       length(pos.ylabel[, 1])))
      text(pos.ylabel[, 1] + 0.5, pos.ylabel[, 2], newcolnames[1:min(n,
                                                                     m)], col = tl.col, cex = tl.cex, ...)
    }
    else {
      text(pos.xlabel[, 1], pos.xlabel[, 2], newcolnames,
           srt = tl.srt, adj = ifelse(tl.srt == 0, c(0.5,
                                                     0), c(0, 0)), col = tl.col, cex = tl.cex, offset = tl.offset,
           ...)
      text(pos.ylabel[, 1], pos.ylabel[, 2], newrownames,
           col = tl.col, cex = tl.cex, pos = 2, offset = tl.offset,
           ...)
    }
  } else {
    pos.ylabel <- cbind(m1:(m1 + nn) - 0.5, n2:n1)
    pos.ylabel <- pos.ylabel[1:min(n, m), ]
    text(pos.ylabel[, 1] + 0.5, pos.ylabel[, 2], newcolnames[1:min(n,
                                                                   m)], col = tl.col, cex = tl.cex, ...)
  }
  title(title, ...)
  if (!is.null(addCoef.col) && method != "number") {
    text(Pos[, 1], Pos[, 2], col = addCoef.col, labels = round((DAT -
                                                                  int) * ifelse(addCoefasPercent, 100, 1)/zoom, number.digits),
         cex = number.cex, font = number.font)
  }
  if (type == "full" && plotCI == "n" && !is.null(addgrid.col)) {
    rect(m1 - 0.5, n1 - 0.5, m2 + 0.5, n2 + 0.5, border = addgrid.col)
  }
  if (!is.null(addrect) && order == "hclust" && type == "full") {
    corrRect.hclust(corr, k = addrect, method = hclust.method,
                    col = rect.col, lwd = rect.lwd)
  }
  invisible(corr)
}

#' @note pure function
#' @noRd
draw_method_color <- function (coords, fg, bg)
{
  symbols(coords, squares = rep(1, nrow(coords)), fg = fg,
          bg = bg, add = TRUE, inches = FALSE)
}

#' @note pure function
#' @noRd
draw_method_square <- function (coords, values, asp_rescale_factor, fg, bg)
{
  symbols(coords, add = TRUE, inches = FALSE, squares = asp_rescale_factor *
            abs(values)^0.5, bg = bg, fg = fg)
}

#' @note pure function
#' @noRd
draw_grid <- function(coords, fg) {
  symbols(coords, add = TRUE, inches = FALSE, fg = fg, bg = NA,
          rectangles = matrix(1, nrow = nrow(coords), ncol = 2))
}


