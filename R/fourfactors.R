#' Calculates possession, pace, ratings, and Four Factors
#'
#' @param TEAM A dataframe
#' @param OPP A dataframe
#' @param sel Vector of selected elements
#' @return A data frame with possession, pace, ratings and Four Factors for the analyzed teams and the opponents
#' @examples
#' data("Tbox")
#' data("Pbox")
#' selectedTeams <- c(2,6,10,11)
#' fourfactors(Tbox, Obox, sel=selectedTeams)
#' @export

fourfactors <- function(TEAM, OPP, sel = 1:nrow(TEAM)) {

  # Checking errors
  if (nrow(TEAM) != nrow(OPP)) {
    stop("Error: TEAM and OPP dataframe must have the same number of rows.")
  }

  # Subsets
  TEAM <- TEAM[sel, ]
  OPP <- OPP[sel, ]

  # Variables used in the calculation of Pace, Ratings, Four Factors
  Team <- TEAM[["Team"]]
  P2A_T <- TEAM[["P2A"]]
  P3A_T <- TEAM[["P3A"]]
  FTA_T <- TEAM[["FTA"]]
  OREB_T <- TEAM[["OREB"]]
  TO_T <- TEAM[["TOV"]]
  MIN_T <- TEAM[["MIN"]]
  P2M_T <- TEAM[["P2M"]]
  P3M_T <- TEAM[["P3M"]]
  FTM_T <- TEAM[["FTM"]]
  DREB_T <- TEAM[["DREB"]]

  P2A_O <- OPP[["P2A"]]
  P3A_O <- OPP[["P3A"]]
  FTA_O <- OPP[["FTA"]]
  OREB_O <- OPP[["OREB"]]
  TO_O <- OPP[["TOV"]]
  MIN_O <- OPP[["MIN"]]
  P2M_O <- OPP[["P2M"]]
  P3M_O <- OPP[["P3M"]]
  FTM_O <- OPP[["FTM"]]
  DREB_O <- OPP[["DREB"]]

  # Formula (2) of the Dean Oliver paper
  # Possession and pace of the game
  # Implies that defensive rebounds have no possession value
  POSS_T <- (P2A_T + P3A_T) + 0.44 * FTA_T - OREB_T + TO_T
  POSS_O <- (P2A_O + P3A_O) + 0.44 * FTA_O - OREB_O + TO_O
  PACE_T <- 5 * POSS_T/MIN_T
  PACE_O <- 5 * POSS_O/MIN_O

  # Formulas (5) and (6) of the Dean Oliver paper
  # Offensive (ORtg) and Defensive (DRtg) Ratings
  ORT <- round(100 * (2 * P2M_T + 3 * P3M_T + FTM_T)/POSS_T, 2)
  DRT <- round(100 * (2 * P2M_O + 3 * P3M_O + FTM_O)/POSS_O, 2)

  # The four factors
  F1_T <- round(100 * (P2M_T + 1.5 * P3M_T)/(P2A_T + P3A_T), 2)
  F1_O <- round(100 * (P2M_O + 1.5 * P3M_O)/(P2A_O + P3A_O), 2)
  F2_T <- round(100 * TO_T/POSS_T, 2)
  F2_O <- round(100 * TO_O/POSS_O, 2)
  F3_T <- round(100 * OREB_T/(OREB_T + DREB_O), 2)
  F3_O <- round(100 * DREB_T/(DREB_T + OREB_O), 2)
  F4_T <- round(100 * FTM_T/(P2A_T + P3A_T), 2)
  F4_O <- round(100 * FTM_O/(P2A_O + P3A_O), 2)

  data.frame(Team,
             POSS_T, POSS_O,
             PACE_T, PACE_O,
             ORT, DRT,
             F1_T, F2_T, F3_T, F4_T,
             F1_O, F2_O, F3_O, F4_O)
}
