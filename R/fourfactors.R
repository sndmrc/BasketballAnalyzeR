#' Calculates possession, pace, ratings, and Four Factors
#'
#' @param TEAM A dataframe
#' @param OPP A dataframe
#' @return A data frame with possession, pace, ratings and Four Factors for the analyzed teams and the opponents
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
  P2A.O <- TEAM[["P2A"]]
  P3A.O <- TEAM[["P3A"]]
  FTA.O <- TEAM[["FTA"]]
  OREB.O <- TEAM[["OREB"]]
  TO.O <- TEAM[["TOV"]]
  MIN.O <- TEAM[["MIN"]]
  P2M.O <- TEAM[["P2M"]]
  P3M.O <- TEAM[["P3M"]]
  FTM.O <- TEAM[["FTM"]]
  DREB.O <- TEAM[["DREB"]]

  P2A.D <- OPP[["P2A"]]
  P3A.D <- OPP[["P3A"]]
  FTA.D <- OPP[["FTA"]]
  OREB.D <- OPP[["OREB"]]
  TO.D <- OPP[["TOV"]]
  MIN.D <- OPP[["MIN"]]
  P2M.D <- OPP[["P2M"]]
  P3M.D <- OPP[["P3M"]]
  FTM.D <- OPP[["FTM"]]
  DREB.D <- OPP[["DREB"]]

  # Formula (2) of the Dean Oliver paper
  # Possession and pace of the game
  # Implies that defensive rebounds have no possession value
  POSS.O <- (P2A.O + P3A.O) + 0.44 * FTA.O - OREB.O + TO.O
  POSS.D <- (P2A.D + P3A.D) + 0.44 * FTA.D - OREB.D + TO.D
  PACE.O <- 5 * POSS.O/MIN.O
  PACE.D <- 5 * POSS.D/MIN.D

  # Formulas (5) and (6) of the Dean Oliver paper
  # Offensive (ORtg) and Defensive (DRtg) Ratings
  ORtg <- round(100 * (2 * P2M.O + 3 * P3M.O + FTM.O)/POSS.O, 2)
  DRtg <- round(100 * (2 * P2M.D + 3 * P3M.D + FTM.D)/POSS.D, 2)

  # The four factors
  F1.O <- round(100 * (P2M.O + 1.5 * P3M.O)/(P2A.O + P3A.O), 2)
  F1.D <- round(100 * (P2M.D + 1.5 * P3M.D)/(P2A.D + P3A.D), 2)
  F2.O <- round(100 * TO.O/POSS.O, 2)
  F2.D <- round(100 * TO.D/POSS.D, 2)
  F3.O <- round(100 * OREB.O/(OREB.O + DREB.D), 2)
  F3.D <- round(100 * DREB.O/(DREB.O + OREB.D), 2)
  F4.O <- round(100 * FTM.O/(P2A.O + P3A.O), 2)
  F4.D <- round(100 * FTM.D/(P2A.D + P3A.D), 2)

  FF <- data.frame(Team,
             POSS.O, POSS.D,
             PACE.O, PACE.D,
             ORtg, DRtg,
             F1.O, F2.O, F3.O, F4.O,
             F1.D, F2.D, F3.D, F4.D)
  class(FF) <- append("fourfactors", class(FF))
  return(FF)
}
