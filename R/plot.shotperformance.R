#' Plots a bubbleplot representing the data contained in the dataframe produced by the function 'shotperformance'
#'
#' @author Andrea Fox
#' @param x  an object of class \code{ashotperformance} obtained using the shotperformance function
#' @param title character, plot title.
#' @param ... other graphical parameters.
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @references P. Zuccolotto, M. Manisera and M. Sandri (2018) Big data analytics for modeling scoring probability in basketball: The effect of shooting under high pressure conditions. International Journal of Sports Science & Coaching.
#' @return A \code{ggplot2} object
#' @examples
#' # Draw the plot for the performances on 2 point shots, when the high pressure situation is
#' # the one regarding shots taken when \code{shotclock} is between 0 and 2
#'
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP <- scoredifference(PbP, team_name = "GSW", player_data=Pbox, team_data = Tadd)
#' PbP <- shotclock(PbP, sec_14_after_oreb = FALSE, team_data = Tadd)
#' players_perf <- shotperformance(PbP, shotclock_interval = c(0, 2),
#'                                 player_data=Pbox, team_data = Tadd,
#'                                 shot_type = "2P", teams = "GSW")
#' plot(players_perf)
#' @method plot shotperformance
#' @export

plot.shotperformance <- function(x, title="Shooting performance", ...) {

  if (!is.shotperformance(x)) {
    stop("Not a 'shotperformance' object")
  }

  if ( colnames(x)[2] == "team" ) {
    team_average <- mean(na.omit(x$overall_performance))
  } else {
    # The input contains data regarding a single team
    team_average <- x[nrow(x), 2]
    x <- x[-nrow(x), ]
  }
  p <- bubbleplot(data = na.omit(x), id = "name", x = "overall_performance", title=title,
            y = "performance_difference_in_S", col = "propensity_to_shoot_in_S", size = "n_shots_high_pressure",
            labels = c("Overall performance", "Difference", "Propension to shoot", "Shots in S"), mx = team_average, my = 0)
  return(p)
}
