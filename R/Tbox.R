#' Teams box scores dataset - NBA 2017-2018
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @description In this data frame, called Tbox, cases (rows) are teams and variables (columns) are referred to team achievements in the different games
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#'
#' @format A data frame with 30 rows and 23 variables:
#' \describe{
#'   \item{Team}{Analyzed team, character}
#'   \item{GP}{Games Played, numeric}
#'   \item{MIN}{Minutes Played, numeric}
#'   \item{PTS}{Points Made, numeric}
#'   \item{W}{Games won, numeric}
#'   \item{L}{Games lost, numeric}
#'   \item{P2M}{2-Point Field Goals (Made), numeric}
#'   \item{P2A}{2-Point Field Goals (Attempted), numeric}
#'   \item{P2p}{2-Point Field Goals (Percentage), numeric}
#'   \item{P3M}{3-Point Field Goals (Made), numeric}
#'   \item{P3A}{3-Point Field Goals (Attempted), numeric}
#'   \item{P3p}{3-Point Field Goals (Percentage), numeric}
#'   \item{FTM}{Free Throws (Made), numeric}
#'   \item{FTA}{Free Throws (Attempted), numeric}
#'   \item{FTp}{Free Throws (Percentage), numeric}
#'   \item{OREB}{Offensive Rebounds, numeric}
#'   \item{DREB}{Defensive Rebounds, numeric}
#'   \item{AST}{Assists, numeric}
#'   \item{TOV}{Turnovers, numeric}
#'   \item{STL}{Steals, numeric}
#'   \item{BLK}{Blocks, numeric}
#'   \item{PF}{Personal Fouls, numeric}
#'   \item{PM}{Plus/Minus, numeric}
#' }
"Tbox"
