#' Scoring probability plot
#'
#' @param data A play-by-play data frame
#' @param var A character string giving the numerical variable whose density to be estimated. Available options: "playlength", "periodTime", "totalTime", "shot_distance".
#' @param shot.type A character string giving the type of shots to be analyzed. Available options: "2P", "3P", "field"
#' @param players Subset of players to be displayed
#' @param bw A numerical value for the smoothing bandwidth of the kernel density estimator (see \link[stats]{ksmooth})
#' @param period.length period.length
#' @param title Plot title
#' @param palette Color palette
#' @param col.team Color of the scoring probability line for team
#' @param legend If TRUE, color legend is displayed (only when 'players' is not NULL)
#' @return A ggplot2 plot
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP.GSW <- subset(PbP, team=="GSW" & result!="")
#' players <- c("Kevin Durant","Draymond Green","Klay Thompson")
#' p <- scoringprob(data=PbP.GSW, shot.type="2P", players=players,
#'                  var="shot_distance", col.team="gray")
#' @export
#' @importFrom stats ksmooth
#' @importFrom grDevices hcl

scoringprob <- function(data, var, shot.type, players=NULL, bw=20, period.length=12, title=NULL,
                      palette=gg_color_hue, col.team="steelblue1", legend=TRUE) {

  ShotType <- NULL
  if (shot.type=="FT" & (var=="playlength" | var=="shot_distance")) {
    print("FT & var: invalid selection")
  }

  if (is.null(title)) {
    title <- shot.type
  }

  if (is.null(bw)) {
    bw <- "nrd0"
  }

  if (shot.type!="FT" | (var!="playlength" & var!="shot_distance")) {

    if (shot.type!="field") {
      data <- subset(data, ShotType==shot.type)
    } else {
      data <- subset(data, ShotType=="2P" | ShotType=="3P")
    }
    data <- droplev_by_col(data)
    data$result01 <- 0
    data$result01[data$result=="made"] <- 1
    data <- data[, c(var, "result01", "player")]

    if (var=="playlength") {
      xrng <- c(0, 24)
      xlab <- "Play length"
      ntks <- 25
    } else if (var=="totalTime") {
      xrng <- c(0, period.length*4*60)
      xlab <- "Total time"
      ntks <- 10
    } else if (var=="periodTime") {
      xrng <- c(0, period.length*60)
      xlab <- "Period time"
      ntks <- 10
    } else if (var=="shot_distance") {
      xlab <- "Shot distance"
      ntks <- NULL
      xrng <- range(data[, var], na.rm=TRUE)
    }
    p <- ksplot(data, var=var, bw=bw, xrng=xrng, ntks=ntks, xlab=xlab, title=title, players=players,
                legend=legend, palette=palette, col.team=col.team)
    print(p)
  }
  return(p)
}

#' @noRd
ksplot <- function(data, var, bw, xrng, ntks, players=NULL, xlab=NULL, ylab="Scoring probability", title=NULL,
                   palette=gg_color_hue, col.team="gray", legend=TRUE) {

  player <- Player <- NULL
  x <- data[, var]
  y <- data$result01
  ksm <- stats::ksmooth(x=x, y=y, bandwidth=bw, range.x=xrng, kernel='normal')
  ksm <- as.data.frame(ksm[c("x", "y")])
  ksm$Player <- "Team"
  npl <- 0
  if (!is.null(players)) {
    npl <- length(players)
    kmslst <- vector(npl+1, mode="list")
    for (k in 1:npl) {
      playerk <- players[k]
      datak <- subset(data, player==playerk)
      xk <- datak[, var]
      yk <- datak$result01
      ksm_k <- stats::ksmooth(x=xk, y=yk, bandwidth=bw, range.x=xrng, kernel='normal')
      ksm_k <- as.data.frame(ksm_k[c("x", "y")])
      ksm_k$Player <- playerk
      kmslst[[k]] <- ksm_k
    }
    kmslst[[npl+1]] <- ksm
    ksm <- do.call(rbind, kmslst)
    players <- sort(unique(ksm$Player))
    cols <- palette(length(players))
    cols[players=="Team"] <- col.team
    p <- ggplot(ksm, aes(x=x, y=y, color=Player)) +
      geom_line(lwd=1.5) +
      scale_color_manual(values=cols, breaks=players)
  } else {
    p <- ggplot(ksm, aes(x=x, y=y)) +
      geom_line(color = col.team, lwd=1.5)
  }
  p <- p + labs(title = title) +
    scale_y_continuous(name=ylab) +
    theme_bw()
  if (!is.null(ntks)) {
    p <- p + scale_x_continuous(name=xlab, limits=c(xrng[1], xrng[2]),
                                breaks=seq(xrng[1],xrng[2],length.out=ntks),
                                labels=seq(xrng[1],xrng[2],length.out=ntks))
  } else {
    p <- p + xlab(xlab)
  }
  if (!legend) {
    p <- p + theme(legend.position="none")
  }
  return(p)
}


#' @noRd
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}
