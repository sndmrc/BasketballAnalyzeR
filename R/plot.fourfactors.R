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
  PACE_T <- PACE_O <- ORT <- DRT <- Factor <- CentValue <- Value <- NULL
  ################################
  ttl <- "PACE"
  if(!is.null(title)) {
    ttl <- paste(ttl, "-", title)
  }
  PACEplot <- ggplot(data=x, aes(x=PACE_T, y=PACE_O, label=Team,
                                text=paste("Team:",Team,"<br>PACE Team:",PACE_T,"<br>PACE Opp:",PACE_O))) +
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
  RTgplot <- ggplot(data=x, aes(x=ORT, y=DRT, label=Team,
                               text=paste("Team:",Team,"<br>Offensive rating:",ORT,"<br>Defensive rating:",DRT))) +
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
  x_lbls <- c("1:eFG% (Off)","2:TO_T Ratio (Off)","3:REB% (Off)","4:FT Rate (Off)")
  ttl <- "Offensive Four Factors"
  if(!is.null(title)) {
    ttl <- paste(ttl, "-", title)
  }
  F1_T <- x[["F1_T"]]
  F2_T <- x[["F2_T"]]
  F3_T <- x[["F3_T"]]
  F4_T <- x[["F4_T"]]
  F1S_T <- F1_T - mean(F1_T)
  F2S_T <- F2_T - mean(F2_T)
  F3S_T <- F3_T - mean(F3_T)
  F4S_T <- F4_T - mean(F4_T)
  FFS_T <- data.frame(Team = rep(Team, 4),
                      Factor = rep(x_lbls, each=nr),
                      CentValue = c(F1S_T,F2S_T,F3S_T,F4S_T),
                      Value = c(F1_T,F2_T,F3_T,F4_T))
  FFOplot <- ggplot(data=FFS_T, aes(x=Factor, y=CentValue, fill=Team,
                                    text=paste("Team:",Team,"<br>Factor:",Factor,"<br>Value:",Value))) +
    geom_bar(stat="identity", color="black", position=position_dodge()) +
    theme_minimal() +	labs(title = ttl)

  ################################
  x_lbls <- c("1:eFG% (Def)","2:TO_T Ratio (Def)","3:REB% (Def)","4:FT Rate (Def)")
  ttl <- "Defensive Four Factors"
  if(!is.null(title)) {
    ttl <- paste(ttl, "-", title)
  }
  F1_O <- x[["F1_O"]]
  F2_O <- x[["F2_O"]]
  F3_O <- x[["F3_O"]]
  F4_O <- x[["F4_O"]]
  F1S_O <- F1_O - mean(F1_O)
  F2S_O <- F2_O - mean(F2_O)
  F3S_O <- F3_O - mean(F3_O)
  F4S_O <- F4_O - mean(F4_O)
  FFS_O <- data.frame(Team = rep(Team, 4),
                      Factor = rep(x_lbls, each=nr),
                      CentValue = c(F1S_O,F2S_O,F3S_O,F4S_O),
                      Value = c(F1_O,F2_O,F3_O,F4_O))
  FFDplot <- ggplot(data=FFS_O, aes(x=Factor, y=CentValue, fill=Team,
                                    text=paste("Team:",Team,"<br>Factor:",Factor,"<br>Value:",Value))) +
    geom_bar(stat="identity", color="black", position=position_dodge()) +
    theme_minimal() +	labs(title = ttl)

  listPlots <- list(PACEplot=PACEplot, RTgplot=RTgplot, FFOplot=FFOplot, FFDplot=FFDplot)
  gridExtra::grid.arrange(grobs=listPlots, ncol=2)
  invisible(listPlots)
}
