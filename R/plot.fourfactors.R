#' Plot possessions, pace, offensive and defensive rating, and Four Factors from a 'fourfactors' object
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param x an object of class \code{fourfactors}.
#' @param title character, plot title.
#' @param ... other graphical parameters.
#' @return A list of four \code{ggplot2} plots.
#' @details The height of the bars in the two four factor plots are given by the difference between the team value and the average on the analyzed teams.
#' @seealso \code{\link{fourfactors}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @examples
#' selTeams <- c(2,6,10,11)
#' FF <- fourfactors(Tbox[selTeams,], Obox[selTeams,])
#' plot(FF)
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 theme_minimal
#' @method plot fourfactors
#' @export

plot.fourfactors <- function(x, title=NULL, ...) {

  if (!is.fourfactors(x)) {
    stop("Not a 'fourfactors' object")
  }
  PACE.Off <- PACE.Def <- ORtg <- DRtg <- Factor <- CentValue <- Value <- NULL
  ################################
  ttl <- "PACE"
  if(!is.null(title)) {
    ttl <- paste(ttl, "-", title)
  }
  PACEplot <- ggplot(data=x, aes(x=PACE.Off, y=PACE.Def, label=Team,
                                text=paste("Team:",Team,"<br>PACE Team:",PACE.Off,"<br>PACE Opp:",PACE.Def))) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label=Team))+
    labs(title=ttl)+
    labs(x = "Pace (Possessions per minute) of the Team") +
    labs(y = "Pace (Possessions per minute) of the Opponents")

  ################################
  ttl <- "ORtg and DRtg"
  if(!is.null(title)) {
    ttl <- paste(ttl, "-", title)
  }
  RTgplot <- ggplot(data=x, aes(x=ORtg, y=DRtg, label=Team,
                               text=paste("Team:",Team,"<br>Offensive rating:",ORtg,"<br>Defensive rating:",DRtg))) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = Team))+
    labs(title = ttl)+
    labs(x = "Offensive Rating of the Team (ORtg)") +
    labs(y = "Offensive Rating of the Opponents (DRtg)")
  RTgplot

  ###
  nr <- nrow(x)
  Team <- x[["Team"]]

  ################################
  x_lbls <- c("1:eFG% (Off)","2:TO.Off Ratio (Off)","3:REB% (Off)","4:FT Rate (Off)")
  ttl <- "Offensive Four Factors"
  if(!is.null(title)) {
    ttl <- paste(ttl, "-", title)
  }
  F1.Off <- x[["F1.Off"]]
  F2.Off <- x[["F2.Off"]]
  F3.Off <- x[["F3.Off"]]
  F4.Off <- x[["F4.Off"]]
  F1S.Off <- F1.Off - mean(F1.Off)
  F2S.Off <- F2.Off - mean(F2.Off)
  F3S.Off <- F3.Off - mean(F3.Off)
  F4S.Off <- F4.Off - mean(F4.Off)
  FFS.Off <- data.frame(Team = rep(Team, 4),
                      Factor = rep(x_lbls, each=nr),
                      CentValue = c(F1S.Off,F2S.Off,F3S.Off,F4S.Off),
                      Value = c(F1.Off,F2.Off,F3.Off,F4.Off))
  FFOplot <- ggplot(data=FFS.Off, aes(x=Factor, y=CentValue, fill=Team,
                                    text=paste("Team:",Team,"<br>Factor:",Factor,"<br>Value:",Value))) +
    geom_bar(stat="identity", color="black", position=position_dodge()) +
    theme_minimal() +	labs(title = ttl)

  ################################
  x_lbls <- c("1:eFG% (Def)","2:TO.Off Ratio (Def)","3:REB% (Def)","4:FT Rate (Def)")
  ttl <- "Defensive Four Factors"
  if(!is.null(title)) {
    ttl <- paste(ttl, "-", title)
  }
  F1.Def <- x[["F1.Def"]]
  F2.Def <- x[["F2.Def"]]
  F3.Def <- x[["F3.Def"]]
  F4.Def <- x[["F4.Def"]]
  F1S.Def <- F1.Def - mean(F1.Def)
  F2S.Def <- F2.Def - mean(F2.Def)
  F3S.Def <- F3.Def - mean(F3.Def)
  F4S.Def <- F4.Def - mean(F4.Def)
  FFS.Def <- data.frame(Team = rep(Team, 4),
                      Factor = rep(x_lbls, each=nr),
                      CentValue = c(F1S.Def,F2S.Def,F3S.Def,F4S.Def),
                      Value = c(F1.Def,F2.Def,F3.Def,F4.Def))
  FFDplot <- ggplot(data=FFS.Def, aes(x=Factor, y=CentValue, fill=Team,
                                    text=paste("Team:",Team,"<br>Factor:",Factor,"<br>Value:",Value))) +
    geom_bar(stat="identity", color="black", position=position_dodge()) +
    theme_minimal() +	labs(title = ttl)

  listPlots <- list(PACEplot=PACEplot, RTgplot=RTgplot, FFOplot=FFOplot, FFDplot=FFDplot)
  gridExtra::grid.arrange(grobs=listPlots, ncol=2)
  invisible(listPlots)
}
