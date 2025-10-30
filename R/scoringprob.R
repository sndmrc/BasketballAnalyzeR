#' Plots scoring probability of shots as a function of a given variable
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param data a data frame whose rows are shots and with the following columns: \code{result}, \code{ShotType}, \code{player} (only if the \code{players} argument is not \code{NULL}) and at least one of \code{playlength}, \code{periodTime}, \code{totalTime}, \code{shot_distance} (the column specified in \code{var}, see Details).
#' @param var character, the string giving the name of the numerical variable according to which the scoring probability is estimated. Available options: \code{"playlength"}, \code{"periodTime"}, \code{"totalTime"}, \code{"shot_distance"}.
#' @param shot.type character, the type of shots to be analyzed; available options: \code{"2P"}, \code{"3P"}, \code{"FT"}, \code{"field"}.
#' @param players subset of players to be displayed (optional; it can be used only if the \code{player} column is present in \code{data}).
#' @param bw numeric, the smoothing bandwidth of the kernel density estimator (see \link[stats]{ksmooth}).
#' @param period.length numeric, the length of a quarter in minutes (default: 12 minutes as in NBA).
#' @param xlab character, x-axis label.
#' @param x.range numerical vector or character; available options: \code{NULL} (x-axis range defined by \code{ggplot2}, the default), \code{"auto"} (internally defined x-axis range), or a 2-component numerical vector (user-defined x-axis range).
#' @param title character, plot title.
#' @param palette color palette.
#' @param team character; if \code{TRUE} draws the scoring probability for all the shots in data.
#' @param col.team character, color of the scoring probability line for all the shots in data.
#' @param legend character; if \code{TRUE}, color legend is displayed (only when \code{players} is not \code{NULL}).
#' @details The \code{data} data frame could also be a play-by-play dataset provided that rows corresponding to events different from shots have \code{NA} in the \code{ShotType} variable.
#' @details Required columns:
#' @details * \code{result}, a factor with the following levels: \code{"made"} for made shots, \code{"miss"} for missed shots, and \code{""} for events different from shots
#' @details * \code{ShotType}, a factor with the following levels: \code{"2P"}, \code{"3P"}, \code{"FT"} (and \code{NA} for events different from shots)
#' @details * \code{player}, a factor with the name of the player who made the shot
#' @details * \code{playlength}, a numeric variable with time between the shot and the immediately preceding event
#' @details * \code{periodTime}, a numeric variable with seconds played in the quarter when the shot is attempted
#' @details * \code{totalTime}, a numeric variable with seconds played in the whole match when the shot is attempted
#' @details * \code{shot_distance}, a numeric variable with the distance of the shooting player from the basket (in feet)
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return A \code{ggplot2} plot
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP.GSW <- subset(PbP, team=="GSW" & result!="")
#' players <- c("Kevin Durant","Draymond Green","Klay Thompson")
#' scoringprob(data=PbP.GSW, shot.type="2P", players=players,
#'             var="shot_distance", col.team="gray")
#' @export
#' @importFrom stats ksmooth
#' @importFrom grDevices hcl

scoringprob <- function(data, var, shot.type, players=NULL, bw=20, period.length=12, xlab=NULL, x.range="auto", title=NULL,
                      palette=gg_color_hue, team=TRUE, col.team='dodgerblue', legend=TRUE) {

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
    x <- data[, var]

    if (length(x.range)==1 & is.character(x.range)) {
      if (x.range=="auto") {
        if (var=="playlength") {
          xrng <- c(0, 24)
          ntks <- 25
        } else if (var=="totalTime") {
          xrng <- c(0, period.length*4*60)
          ntks <- 10
        } else if (var=="periodTime") {
          xrng <- c(0, period.length*60)
          ntks <- 10
        } else if (var=="shot_distance") {
          xrng <- range(x, na.rm=TRUE)
          ntks <- NULL
        }
      }
    } else if (length(x.range)==2 & is.numeric(x.range)) {
      xrng <- x.range
      if (xrng[1]<min(x,na.rm=T)) xrng[1]=min(x,na.rm=T)
      if (xrng[2]>max(x,na.rm=T)) xrng[2]=max(x,na.rm=T)
      ntks <- NULL
    } else if (is.null(x.range)) {
      xrng <- NULL
      ntks <- NULL
    }

    if (var=="playlength") {
      if (is.null(xlab)) xlab <- "Play length"
    } else if (var=="totalTime") {
      if (is.null(xlab)) xlab <- "Total time"
    } else if (var=="periodTime") {
      if (is.null(xlab)) xlab <- "Period time"
    } else if (var=="shot_distance") {
      if (is.null(xlab)) xlab <- "Shot distance"
    } else {
      if (is.null(xlab)) xlab <- var
    }

    p <- ksplot(data, var=var, bw=bw, xrng=xrng, ntks=ntks, xlab=xlab, title=title, players=players,
                legend=legend, palette=palette, team=team, col.team=col.team)
  }
  return(p)
}

#' @noRd
ksplot <- function(data, var, bw, xrng=NULL, ntks=NULL, players=NULL, xlab=NULL, ylab="Scoring probability", title=NULL,
                   palette=gg_color_hue, col.team="gray", legend=TRUE, team=TRUE) {

  player <- Player <- NULL
  x <- data[, var]
  y <- data$result01
  if (team) {
    ksm <- stats::ksmooth(x=x, y=y, bandwidth=bw, kernel='normal')
    ksm <- as.data.frame(ksm[c("x", "y")])
    ksm$Player <- "Team"
    if (!is.null(xrng)) {
      ksm <- subset(ksm, x>=xrng[1] & x<=xrng[2])
    }
  }
  npl <- 0
  if (!is.null(players)) {
    npl <- length(players)
    kmslst <- vector(npl+1, mode="list")
    for (k in 1:npl) {
      playerk <- players[k]
      datak <- subset(data, player==playerk)
      xk <- datak[, var]
      yk <- datak$result01
      ksm_k <- stats::ksmooth(x=xk, y=yk, bandwidth=bw, kernel='normal')
      ksm_k <- as.data.frame(ksm_k[c("x", "y")])
      ksm_k$Player <- playerk
      if (!is.null(xrng)) {
        ksm_k <- subset(ksm_k, x>=xrng[1] & x<=xrng[2])
      }
      kmslst[[k]] <- ksm_k
    }
    if (team) kmslst[[npl+1]] <- ksm
    ksm <- do.call(rbind, kmslst)
    players <- sort(unique(ksm$Player))
    cols <- palette(length(players))
    cols[players=="Team"] <- col.team
    p <- ggplot(ksm, aes(x=x, y=y, color=Player)) +
      geom_line(linewidth=1.5) +
      scale_color_manual(values=cols, breaks=players)
  } else {
    p <- ggplot(ksm, aes(x=x, y=y)) +
      geom_line(color = col.team, linewidth=1.5)
  }
  p <- p + labs(title = title) +
    scale_y_continuous(name=ylab) +
    theme_bw()
  if (!is.null(ntks) & !is.null(xrng)) {
    p <- p + scale_x_continuous(name=xlab, limits=c(xrng[1], xrng[2]),
                                breaks=seq(xrng[1],xrng[2],length.out=ntks),
                                labels=seq(xrng[1],xrng[2],length.out=ntks))
  } else if (is.null(ntks) & !is.null(xrng)) {
    p <- p + xlim(xrng) + xlab(xlab)
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
