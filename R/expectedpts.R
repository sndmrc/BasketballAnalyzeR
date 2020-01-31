#' Plot expected points of player shots as a function of the distance from the basket
#'
#' @param data A data frame whose rows are field shots and columns are variables to be specified in \code{var} and optionally in \code{players}.
#' @param players Subset of players to be displayed
#' @param bw A numerical value for the smoothing bandwidth of the kernel density estimator (see \link[stats]{ksmooth})
#' @param title Plot title
#' @param palette Color palette
#' @param team team
#' @param col.team Color of the scoring probability line for team
#' @param col.hline Color of the dashed horizontal line
#' @param legend If TRUE, color legend is displayed (only when 'players' is not NULL)
#' @param xlab x-axis label
#' @param var A character string giving the numerical variable on the x-axis (default "shot_distance").
#' @return A ggplot2 plot
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP.GSW <- subset(PbP, team=="GSW" & !is.na(shot_distance))
#' plrys <- c("Stephen Curry","Kevin Durant")
#' expectedpts(data=PbP.GSW, bw=10, players=plrys, col.team='dodgerblue',
#'         palette=colorRampPalette(c("gray","black")), col.hline="red")
#' @export

expectedpts <- function(data, players=NULL, bw=10, palette=gg_color_hue, team=TRUE, col.team="gray",
          col.hline="black", xlab=NULL, title=NULL, legend=TRUE, var="shot_distance") {

  event_type <- player <- Player <- NULL
  data <- data %>% dplyr::select(dplyr::one_of(var, "points", "player", "event_type")) %>%
                   dplyr::filter(event_type=="shot" | event_type=="miss")
  x <- data[, var]
  y <- data$points

  period.length <- 12
  if (var=="playlength") {
    xrng <- c(0, 24)
    if (is.null(xlab)) xlab <- "Play length"
    ntks <- 25
  } else if (var=="totalTime") {
    xrng <- c(0, period.length*4*60)
    if (is.null(xlab)) xlab <- "Total time"
    ntks <- 10
  } else if (var=="periodTime") {
    xrng <- c(0, period.length*60)
    if (is.null(xlab)) xlab <- "Period time"
    ntks <- 10
  } else if (var=="shot_distance") {
    if (is.null(xlab)) xlab <- "Shot distance"
    ntks <- NULL
    xrng <- range(data[, var], na.rm=TRUE)
  } else {
    if (is.null(xlab)) xlab <- var
  }

  if (team) {
    ksm <- stats::ksmooth(x=x, y=y, bandwidth=bw, x.points=x, kernel='normal')
    ksm <- as.data.frame(ksm[c("x", "y")])
    ksm$Player <- "Team"
  }

  npl <- 0
  if (!is.null(players)) {
    npl <- length(players)
    kmslst <- vector(npl+1, mode="list")
    for (k in 1:npl) {
      playerk <- players[k]
      datak <- subset(data, player==playerk)
      xk <- datak[, var]
      yk <- datak$points
      ksm_k <- stats::ksmooth(x=xk, y=yk, bandwidth=bw, x.points=xk, kernel='normal')
      ksm_k <- as.data.frame(ksm_k[c("x", "y")])
      ksm_k$Player <- playerk
      kmslst[[k]] <- ksm_k
    }
    if (team) kmslst[[npl+1]] <- ksm
    ksm <- do.call(rbind, kmslst)
    players <- sort(unique(ksm$Player))
    cols <- palette(npl+1)
    cols[players=="Team"] <- col.team
    p <- ggplot(ksm, aes(x=x, y=y, color=Player)) +
      geom_line(lwd=1.5) +
      scale_color_manual(values=cols, breaks=players)
  } else {
    p <- ggplot(ksm, aes(x=x, y=y)) +
      geom_line(color = col.team, lwd=1.5)
  }
  p <- p + geom_hline(yintercept=mean(y), col=col.hline, lty=2, lwd=1.2) +
    labs(x=xlab, y="Expected Points", title=title) +
    theme_bw()
  if (!legend) {
    p <- p + theme(legend.position="none")
  }

  return(p)
}
