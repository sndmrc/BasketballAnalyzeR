#' Reports whether x is a 'shotperformance' object
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param x an object to test.
#' @seealso \code{\link{shotperformance}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return Returns TRUE if its argument is of class \code{shotperformance} and FALSE otherwise.
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP <- scoredifference(PbP_data = PbP, team_name = "GSW", player_data=Pbox, team_data = Tadd)
#' PbP <- shotclock(PbP_data = PbP, sec_14_after_oreb = FALSE, team_data = Tadd)
#' shotperf <- shotperformance(PbP_data = PbP, player_data = Pbox, team_data = Tadd,
#'                 shotclock_interval = c(0, 2) , shot_type = "2P"  )
#' is.shotperformance(shotperf)
#' @export

is.shotperformance <- function(x) {
  inherits(x, "shotperformance")
}
