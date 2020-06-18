#' Tadd dataset - NBA 2017-2018
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @description  In this data frame, the cases (rows) are the analyzed teams and the variables (columns) are qualitative information such as Conference, Division, final rank, qualification for Playoffs for the NBA 2017-2018 Championship.
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#'
#' @format A data frame with 30 rows and 6 variables:
#' \describe{
#'   \item{Team}{Analyzed team (long name), factor}
#'   \item{team}{Analyzed team (short name), factor}
#'   \item{Conference}{Conference, factor}
#'   \item{Division}{Division, factor}
#'   \item{Rank}{Rank (end season), numeric}
#'   \item{Playoff}{Playoff qualification (Yes or No), factor}
#' }
"Tadd"
