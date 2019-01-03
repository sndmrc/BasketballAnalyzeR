#' Plot possession, pace, ratings, and Four Factors
#'
#' @param x A data.frame with calculated possession, pace, ratings, and Four Factors
#' @param title Plot title
#' @param ... other graphical parameters
#' @return A list of four ggplot2 plots
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
  PACE.O <- PACE.D <- ORtg <- DRtg <- Factor <- CentValue <- Value <- NULL
  ################################
  ttl <- "PACE"
  if(!is.null(title)) {
    ttl <- paste(ttl, "-", title)
  }
  PACEplot <- ggplot(data=x, aes(x=PACE.O, y=PACE.D, label=Team,
                                text=paste("Team:",Team,"<br>PACE Team:",PACE.O,"<br>PACE Opp:",PACE.D))) +
    geom_point() +
    geom_text_repel(aes(label=Team))+
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
    geom_text_repel(aes(label = Team))+
    labs(title = ttl)+
    labs(x = "Offensive Rating of the Team (ORtg)") +
    labs(y = "Offensive Rating of the Opponents (DRtg)")
  RTgplot

  ###
  nr <- nrow(x)
  Team <- x[["Team"]]

  ################################
  x_lbls <- c("1:eFG% (Off)","2:TO.O Ratio (Off)","3:REB% (Off)","4:FT Rate (Off)")
  ttl <- "Offensive Four Factors"
  if(!is.null(title)) {
    ttl <- paste(ttl, "-", title)
  }
  F1.O <- x[["F1.O"]]
  F2.O <- x[["F2.O"]]
  F3.O <- x[["F3.O"]]
  F4.O <- x[["F4.O"]]
  F1S.O <- F1.O - mean(F1.O)
  F2S.O <- F2.O - mean(F2.O)
  F3S.O <- F3.O - mean(F3.O)
  F4S.O <- F4.O - mean(F4.O)
  FFS.O <- data.frame(Team = rep(Team, 4),
                      Factor = rep(x_lbls, each=nr),
                      CentValue = c(F1S.O,F2S.O,F3S.O,F4S.O),
                      Value = c(F1.O,F2.O,F3.O,F4.O))
  FFOplot <- ggplot(data=FFS.O, aes(x=Factor, y=CentValue, fill=Team,
                                    text=paste("Team:",Team,"<br>Factor:",Factor,"<br>Value:",Value))) +
    geom_bar(stat="identity", color="black", position=position_dodge()) +
    theme_minimal() +	labs(title = ttl)

  ################################
  x_lbls <- c("1:eFG% (Def)","2:TO.O Ratio (Def)","3:REB% (Def)","4:FT Rate (Def)")
  ttl <- "Defensive Four Factors"
  if(!is.null(title)) {
    ttl <- paste(ttl, "-", title)
  }
  F1.D <- x[["F1.D"]]
  F2.D <- x[["F2.D"]]
  F3.D <- x[["F3.D"]]
  F4.D <- x[["F4.D"]]
  F1S.D <- F1.D - mean(F1.D)
  F2S.D <- F2.D - mean(F2.D)
  F3S.D <- F3.D - mean(F3.D)
  F4S.D <- F4.D - mean(F4.D)
  FFS.D <- data.frame(Team = rep(Team, 4),
                      Factor = rep(x_lbls, each=nr),
                      CentValue = c(F1S.D,F2S.D,F3S.D,F4S.D),
                      Value = c(F1.D,F2.D,F3.D,F4.D))
  FFDplot <- ggplot(data=FFS.D, aes(x=Factor, y=CentValue, fill=Team,
                                    text=paste("Team:",Team,"<br>Factor:",Factor,"<br>Value:",Value))) +
    geom_bar(stat="identity", color="black", position=position_dodge()) +
    theme_minimal() +	labs(title = ttl)

  listPlots <- list(PACEplot=PACEplot, RTgplot=RTgplot, FFOplot=FFOplot, FFDplot=FFDplot)
  gridExtra::grid.arrange(grobs=listPlots, ncol=2)
  invisible(listPlots)
}
