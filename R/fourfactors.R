#' Calculates possession, pace, ratings, and Four Factors
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param TEAM a data frame (see Details).
#' @param OPP a data frame (see Details).
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @seealso \code{\link{plot.fourfactors}}
#' @return An object of class \code{fourfactors}, i.e. a data frame with possession, pace, ratings and Four Factors for the analyzed teams and the opponents.
#' @examples
#' selTeams <- c(2,6,10,11)
#' FF <- fourfactors(Tbox[selTeams,], Obox[selTeams,])
#' plot(FF)
#' @export

fourfactors <- function(TEAM, OPP) {

  # Checking errors
  if (nrow(TEAM) != nrow(OPP)) {
    stop("Error: TEAM and OPP dataframe must have the same number of rows.")
  }

  # Variables used in the calculation of Pace, Ratings, Four Factors
  Team <- TEAM[["Team"]]
  P2A.Off <- TEAM[["P2A"]]
  P3A.Off <- TEAM[["P3A"]]
  FTA.Off <- TEAM[["FTA"]]
  OREB.Off <- TEAM[["OREB"]]
  TO.Off <- TEAM[["TOV"]]
  MIN.Off <- TEAM[["MIN"]]
  P2M.Off <- TEAM[["P2M"]]
  P3M.Off <- TEAM[["P3M"]]
  FTM.Off <- TEAM[["FTM"]]
  DREB.Off <- TEAM[["DREB"]]

  P2A.Def <- OPP[["P2A"]]
  P3A.Def <- OPP[["P3A"]]
  FTA.Def <- OPP[["FTA"]]
  OREB.Def <- OPP[["OREB"]]
  TO.Def <- OPP[["TOV"]]
  MIN.Def <- OPP[["MIN"]]
  P2M.Def <- OPP[["P2M"]]
  P3M.Def <- OPP[["P3M"]]
  FTM.Def <- OPP[["FTM"]]
  DREB.Def <- OPP[["DREB"]]

  # Formula (2) of the Dean Oliver paper
  # Possession and pace of the game
  # Implies that defensive rebounds have no possession value
  POSS.Off <- (P2A.Off + P3A.Off) + 0.44 * FTA.Off - OREB.Off + TO.Off
  POSS.Def <- (P2A.Def + P3A.Def) + 0.44 * FTA.Def - OREB.Def + TO.Def
  PACE.Off <- POSS.Off/MIN.Off
  PACE.Def <- POSS.Def/MIN.Def

  # Formulas (5) and (6) of the Dean Oliver paper
  # Offensive (ORtg) and Defensive (DRtg) Ratings
  ORtg <- round(100 * (2 * P2M.Off + 3 * P3M.Off + FTM.Off)/POSS.Off, 2)
  DRtg <- round(100 * (2 * P2M.Def + 3 * P3M.Def + FTM.Def)/POSS.Def, 2)

  # The four factors
  F1.Off <- round(100 * (P2M.Off + 1.5 * P3M.Off)/(P2A.Off + P3A.Off), 2)
  F1.Def <- round(100 * (P2M.Def + 1.5 * P3M.Def)/(P2A.Def + P3A.Def), 2)
  F2.Off <- round(100 * TO.Off/POSS.Off, 2)
  F2.Def <- round(100 * TO.Def/POSS.Def, 2)
  F3.Off <- round(100 * OREB.Off/(OREB.Off + DREB.Def), 2)
  F3.Def <- round(100 * DREB.Off/(DREB.Off + OREB.Def), 2)
  F4.Off <- round(100 * FTM.Off/(P2A.Off + P3A.Off), 2)
  F4.Def <- round(100 * FTM.Def/(P2A.Def + P3A.Def), 2)

  FF <- data.frame(Team,
             POSS.Off, POSS.Def,
             PACE.Off, PACE.Def,
             ORtg, DRtg,
             F1.Off, F2.Off, F3.Off, F4.Off,
             F1.Def, F2.Def, F3.Def, F4.Def)
  class(FF) <- append("fourfactors", class(FF))
  return(FF)
}
