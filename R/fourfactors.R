#' Calculates possessions, pace, offensive and defensive rating, and Four Factors
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param TEAM a data frame whose rows are the analyzed teams and with columns referred to the team achievements in the considered games (a box score); required variables: \code{Team}, \code{P2A}, \code{P2M}, \code{P3A}, \code{P3M}, \code{FTA}, \code{FTM}, \code{OREB}, \code{DREB}, \code{TOV}, \code{MIN} (see Details).
#' @param OPP a data frame whose rows are the analyzed teams and with columns referred to the achievements of the opponents of each team in the considered game; required variables: \code{Team}, \code{P2A}, \code{P2M}, \code{P3A}, \code{P3M}, \code{FTA}, \code{FTM}, \code{OREB}, \code{DREB}, \code{TOV}, \code{MIN} (see Details).
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @seealso \code{\link{plot.fourfactors}}
#' @details The rows of the \code{TEAM} and the \code{OPP} data frames must be referred to the same teams in the same order.
#' @details Required columms:
#' @details * \code{Team}, a factor with the name of the analyzed team
#' @details * \code{P2A}, a numeric variable (integer) with the number of 2-points shots attempted
#' @details * \code{P2M}, a numeric variable (integer) with the number of 2-points shots made
#' @details * \code{P3A}, a numeric variable (integer) with the number of 3-points shots attempted
#' @details * \code{P3M}, a numeric variable (integer) with the number of 3-points shots made
#' @details * \code{FTA}, a numeric variable (integer) with the number of free throws attempted
#' @details * \code{FTM}, a numeric variable (integer) with the number of free throws made
#' @details * \code{OREB}, a numeric variable (integer) with the number of offensive rebounds
#' @details * \code{DREB}, a numeric variable (integer) with the number of defensive rebounds
#' @details * \code{TOV}, a numeric variable (integer) with the number of turnovers
#' @details * \code{MIN}, a numeric variable (integer) with the number of minutes played
#' @return An object of class \code{fourfactors}, i.e. a data frame with the following columns:
#' @return * \code{Team}, a factor with the name of the analyzed team
#' @return * \code{POSS.Off}, a numeric variable with the number of possessions of each team calculated with the formula \eqn{POSS=(P2A+P3A)+0.44*FTA-OREB+TOV}
#' @return * \code{POSS.Def}, a numeric variable with the number of possessions of the opponents of each team calculated with the formula \eqn{POSS=(P2A+P3A)+0.44*FTA-OREB+TOV}
#' @return * \code{PACE.Off}, a numeric variable with the pace of each team (number of possessions per minute played)
#' @return * \code{PACE.Def}, a numeric variable with the pace of the opponents of each team (number of possessions per minute played)
#' @return * \code{ORtg}, a numeric variable with the offensive rating (the points scored by each team per 100 possessions)
#' @return * \code{DRtg}, a numeric variable with the defensive rating (the points scored by the opponents of each team per 100 possessions)
#' @return * \code{F1.Off}, a numeric variable with the offensive first factor (effective field goal percentage)
#' @return * \code{F2.Off}, a numeric variable with the offensive second factor (turnovers per possession)
#' @return * \code{F3.Off}, a numeric variable with the offensive third factor (rebouding percentage)
#' @return * \code{F4.Off}, a numeric variable with the offensive fourth factor (free throw rate)
#' @return * \code{F1.Def}, a numeric variable with the defensive first factor (effective field goal percentage)
#' @return * \code{F2.Def}, a numeric variable with the defensive second factor (turnovers per possession)
#' @return * \code{F3.Def}, a numeric variable with the defensive third factor (rebouding percentage)
#' @return * \code{F4.Def}, a numeric variable with the defensive fourth factor (free throw rate)
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
