#' Computes and plots kernel density estimation of shots with respect to a concurrent variable
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param data a data frame whose rows are shots and with the following columns: \code{ShotType}, \code{player}, \code{points} and at least one of \code{playlength}, \code{periodTime}, \code{totalTime}, \code{shot_distance} (the column specified in \code{var}, see Details).
#' @param var character, a string giving the name of the numerical variable according to which the shot density is estimated. Available options: \code{"playlength"}, \code{"periodTime"}, \code{"totalTime"}, \code{"shot_distance"}.
#' @param shot.type character, a string giving the type of shots to be analyzed. Available options: \code{"2P"}, \code{"3P"}, \code{"FT"}, \code{"field"}.
#' @param thresholds numerical vector with two thresholds defining the range boundaries that divide the area under the density curve into three regions. If \code{NULL} default values are used.
#' @param best.scorer logical; if TRUE, displays the player who scored the highest number of points in the corresponding interval.
#' @param period.length numeric, the length of a quarter in minutes (default: 12 minutes as in NBA).
#' @param bw numeric, the value for the smoothing bandwidth of the kernel density estimator or a character string giving a rule to choose the bandwidth (see \link[stats]{density}).
#' @param title character, plot title.
#' @details The \code{data} data frame could also be a play-by-play dataset provided that rows corresponding to events different from shots have \code{NA} in the \code{ShotType} variable.
#' @details Required columns:
#' @details * \code{ShotType}, a factor with the following levels: \code{"2P"}, \code{"3P"}, \code{"FT"} (and \code{NA} for events different from shots)
#' @details * \code{player}, a factor with the name of the player who made the shot
#' @details * \code{points}, a numeric variable (integer) with the points scored by made shots and \code{0} for missed shots
#' @details * \code{playlength}, a numeric variable with time between the shot and the immediately preceding event
#' @details * \code{periodTime}, a numeric variable with seconds played in the quarter when the shot is attempted
#' @details * \code{totalTime}, a numeric variable with seconds played in the whole match when the shot is attempted
#' @details * \code{shot_distance}, a numeric variable with the distance of the shooting player from the basket (in feet)
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return A \code{ggplot2} plot
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' data.team  <- subset(PbP, team=="GSW" & result!="")
#' densityplot(data=data.team, shot.type="2P", var="playlength", best.scorer=TRUE)
#' data.opp <- subset(PbP, team!="GSW" & result!="")
#' densityplot(data=data.opp, shot.type="2P", var="shot_distance", best.scorer=TRUE)
#' @export
#' @importFrom stats density

densityplot <- function(data, var, shot.type="field", thresholds=NULL, best.scorer=FALSE,
                        period.length=12, bw=NULL, title=NULL) {

  ShotType <- NULL
  if (shot.type=="FT" & (var=="playlength" | var=="shot_distance")) {
    warning("'shot.type' and 'var': invalid selection")
  }

  if (is.null(title)) {
    title <- shot.type
  }

  if (is.null(bw)) {
    bw <- "nrd0"
  }

  if (shot.type!="FT" | (var!="playlength" & var!="shot_distance")) {

    if (shot.type!="field") {
      mat <- subset(data, ShotType==shot.type)
    } else {
      mat <- subset(data, ShotType=="2P" | ShotType=="3P")
    }
    mat <- droplev_by_col(mat)

    x <- mat[, var]
    if (var=="playlength") {
      xrng <- c(0, 24)
    } else if (var=="totalTime") {
      xrng <- c(0, period.length*4*60)
    } else if (var=="periodTime") {
      xrng <- c(0, period.length*60)
    } else if (var=="shot_distance") {
      if (shot.type=="field") {
        xrng <- c(0, 60)
      } else if (shot.type=="2P") {
        xrng <- c(0, 22)
      } else if (shot.type=="3P") {
        xrng <- c(22, 60)
      }
    }
    den <- stats::density(x, bw=bw, from=xrng[1], to=xrng[2])
    den <- as.data.frame(den[c("x", "y")])

    ####
    if (var=="playlength") {
      ####
      if (is.null(thresholds)) {
        thr <- c(5, 20)
      } else {
        thr <- thresholds
      }
      p <- plot_shotdensity(mat, den, var=var, thr=thr, xrng=xrng, ntks=25,
                            xadj=c(0,0,24), yadj=c(2,2,2,0.1), best.scorer=best.scorer, title=title, xlab="Play length")
      ####
    } else if (var=="totalTime") {
      ####
      if (is.null(thresholds)) {
        thr <- period.length*60*c(2, 3)
      } else {
        thr <- thresholds
      }
      p <- plot_shotdensity(mat, den, var=var, thr=thr, xrng=xrng, ntks=10, title=title,
                            xadj=c(0,0,period.length*60*4), yadj=c(2,2,2,10), best.scorer=best.scorer, xlab="Total time")

      ####
    } else if (var=="periodTime") {
      ####
      if (is.null(thresholds)) {
        thr <- period.length*60*c(1/2,3/4)
      } else {
        thr <- thresholds
      }
      p <- plot_shotdensity(mat, den, var=var, thr=thr, xrng=xrng, ntks=10, title=title,
                            xadj=c(0,0,period.length*60), yadj=c(2,2,2,10), best.scorer=best.scorer, xlab="Period time")

      ####
    } else if (var=="shot_distance") {
      ####
      if (is.null(thresholds)) {
        if (shot.type=="field") {
          thr <- c(4, 22)
        } else if (shot.type=="2P") {
          thr <- c(4, 18)
        } else if (shot.type=="3P") {
          thr <- c(25, 30)
        }
      } else {
        thr <- thresholds
      }

      if (shot.type=="field") {
        xadj <- c(0,0,36)
        yadj <- c(2,2,2,0.5)
      } else if (shot.type=="2P") {
        xadj <- c(0,0,22)
        yadj <- c(2,2,4,0.5)
      } else if (shot.type=="3P") {
        xadj <- c(22, 0, 60)
        yadj <- c(6,2,2,0.5)
      }
      p <- plot_shotdensity(mat, den, var=var, thr=thr, xrng=c(0,60), ntks=21,
                            xadj=xadj, yadj=yadj, best.scorer=best.scorer, title=title, xlab="Shot distance")

    }
    p <- p + theme_bw()
  }
  return(p)
}

#' @noRd
plot_shotdensity <- function(mat, den, var, thr, xrng, ntks, xadj, yadj, title=NULL, best.scorer=FALSE, xlab=NULL) {

  droplev_by_col <- function(data) {
    idx <- sapply(data, class)=="factor"
    data[, idx] <- lapply(data[, idx], droplevels)
    return(data)
  }

  y <- NULL
  x <- mat[, var]
  n <- nrow(mat)
  m1 <- droplev_by_col(mat[x <= thr[1], ])
  n1 <- nrow(m1)
  p1 <- round(n1/n*100,0)
  m1p <- round(sum(m1$result=="made")/n1*100,0)
  m2 <- droplev_by_col(mat[x <= thr[2] & x > thr[1], ])
  n2 <- nrow(m2)
  p2 <- round(n2/n*100,0)
  m2p <- round(sum(m2$result=="made")/n2*100,0)
  m3 <- droplev_by_col(mat[x > thr[2], ])
  n3 <- n - (n1+n2)
  p3 <- 100-(p1+p2)
  m3p <- round(sum(m3$result=="made")/n3*100,0)

  x1 <- (thr[1] + xadj[1])/2
  x2 <- (thr[1] + thr[2] + xadj[2])/2
  x3 <- (thr[2] + xadj[3])/2

  y1 <- mean(den$y[den$x<(x1 + yadj[4]) & den$x>(x1 - yadj[4])])/yadj[1]
  y2 <- mean(den$y[den$x<(x2 + yadj[4]) & den$x>(x2 - yadj[4])])/yadj[2]
  y3 <- mean(den$y[den$x<(x3 + yadj[4]) & den$x>(x3 - yadj[4])])/yadj[3]

  p <- ggplot(den,aes(x,y))+
    geom_line(col='gray',lwd=2) +
    geom_ribbon(data=subset(den,x<=thr[1]),aes(x=x, ymax=y, ymin=0), fill="blue", alpha=0.3) +
    geom_ribbon(data=subset(den,x<=thr[2] & x>thr[1]),aes(x=x, ymax=y, ymin=0), fill="blue", alpha=0.5) +
    geom_ribbon(data=subset(den,x>thr[2]),aes(x=x, ymax=y, ymin=0), fill="red", alpha=0.3) +
    annotate("text", label = paste(as.character(p1),"%",sep=""), x = x1, y = y1, size = 5, colour = "blue") +
    annotate("text", label = as.character(n1), x = x1, y = y1, size = 4, colour = "blue",vjust = 2) +
    annotate("text", label = paste("(",as.character(m1p),"% made)",sep=""), x = x1, y = y1, size = 4, colour = "blue",vjust = 4) +
    annotate("text", label = paste(as.character(p2),"%",sep=""), x = x2, y = y2, size = 5, colour = "blue") +
    annotate("text", label = as.character(n2), x = x2, y = y2, size = 4, colour = "blue",vjust = 2) +
    annotate("text", label = paste("(",as.character(m2p),"% made)",sep=""), x = x2, y = y2, size = 4, colour = "blue",vjust = 4) +
    annotate("text", label = paste(as.character(p3),"%",sep=""), x = x3, y = y3, size = 5, colour = "red") +
    annotate("text", label = as.character(n3), x = x3, y = y3, size = 4, colour = "red",vjust = 2) +
    annotate("text", label = paste("(",as.character(m3p),"% made)",sep=""), x = x3, y = y3, size = 4, colour = "red",vjust = 4) +
    labs(title = title) +
    scale_x_continuous(name=xlab, limits=c(xrng[1], xrng[2]), breaks=seq(xrng[1],xrng[2],length.out=ntks),
                       labels=seq(xrng[1],xrng[2],length.out=ntks)) +
    scale_y_continuous(name="Frequency of shots", limits=c(0, NA),labels=NULL)


  if (best.scorer) {
    points1 <- tapply(m1$points, m1$player, sum, na.rm=T)
    mpoints1 <- max(points1, na.rm=T)
    wp1 <- points1[points1==mpoints1]
    np <- length(wp1)
    if (np>1) {
      wp1 <- wp1[1]
      attr(wp1,"names") <- paste(np," players",sep="")
    }
    points2 <- tapply(m2$points,m2$player,sum)
    mpoints2 <- max(points2,na.rm=T)
    wp2 <- points2[points2==mpoints2]
    np <- length(wp2)
    if (np>1) {
      wp2 <- wp2[1]
      attr(wp2,"names") <- paste(np," players",sep="")
    }
    points3 <- tapply(m3$points,m3$player,sum)
    mpoints3 <- max(points3,na.rm=T)
    wp3 <- points3[points3==mpoints3]
    np <- length(wp3)
    if (np>1) {
      wp3 <- wp3[1]
      attr(wp3,"names") <- paste(np," players",sep="")
    }

    p <- p +
      annotate("text", label = "Best scorer:", x = x1, y = y1, size = 4, colour = "blue",vjust = -5) +
      annotate("text", label = attr(wp1,"names"), x = x1, y = y1, size = 4, colour = "blue",vjust = -3.5) +
      annotate("text", label = paste("(", as.character(wp1)," points)",sep=""), x = x1, y = y1, size = 4, colour = "blue",vjust = -2) +
      #
      annotate("text", label = "Best scorer:", x = x2, y = y2, size = 4, colour = "blue",vjust = -5) +
      annotate("text", label = attr(wp2,"names"), x = x2, y = y2, size = 4, colour = "blue",vjust = -3.5) +
      annotate("text", label = paste("(", as.character(wp2)," points)",sep=""), x = x2, y = y2, size = 4, colour = "blue",vjust = -2) +
      #
      annotate("text", label = "Best scorer:", x = x3, y = y3, size = 4, colour = "red",vjust = -5) +
      annotate("text", label = attr(wp3,"names"), x = x3, y = y3, size = 4, colour = "red",vjust = -3.5)+
      annotate("text", label = paste("(", as.character(wp3)," points)",sep=""), x = x3, y = y3, size = 4, colour = "red",vjust = -2)

  }
  return(p)
}

#' @noRd
droplev_by_col <- function(data) {
  idx <- sapply(data, class)=="factor"
  data[, idx] <- lapply(data[, idx], droplevels)
  return(data)
}
