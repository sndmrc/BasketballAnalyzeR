#' Computes the probability of scoring certain shot types in certain conditions, by looking at the result of the shots in the PbP provided
#'
#' @param PbP_data  a play-by-play dataframe, previously handled by the PbPmanipulation function
#' @param team_name character, if the play-by-play dataframe given as an input contains data for multiple teams, this parameters filters only the shots of the team we are interested in
#' @param shotclock_interval vector of two numeric values or single numeric value, condition on the value of shotclock of the shots that will be considered
#' @param score_difference vector of two numeric values or single numeric value, condition on the value of shotclock of the shots that will be considered
#' @param totaltime numeric value, condition on the value of totalTime of the shots that will be considered
#' @param shot_type character, the type of shots to be analyzed; available options: "2P", "3P", "FT", "field"
#' @param team_data dataframe, contains several data regarding the teams in the NBA. Inside this function it is used only to check if \code{team_name} corresponds to a team in the NBA. It doesn't have to be changed if the teams in the play-by-play considered are the same as in the 2017-18 season
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @references P. Zuccolotto, M. Manisera and M. Sandri (2018) Big data analytics for modeling scoring probability in basketball: The effect of shooting under high pressure conditions. International Journal of Sports Science & Coaching.
#' @return numeric value, indicating the probability that a shots which respects all the conditions defined is made
#' @examples
#'
#' # probability that a 2 point shot attempted by the Golden State Warriors
#' # in the last two seconds of an action is made
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP <- scoredifference(PbP, team_name = "GSW", player_data=Pbox, team_data=Tadd)
#' PbP <- shotclock(PbP,  sec_14_after_oreb = FALSE, team_data=Tadd)
#' scoringprobability(PbP, team_name = "GSW", shotclock_interval = c(0, 2),
#'                   shot_type = "2P", team_data=Tadd)
#'
#' # probability that a 3 point shot attempted when the score difference is
#' # between -5 and 1 is made
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP <- scoredifference(PbP, team_name = "GSW", player_data=Pbox, team_data=Tadd)
#' PbP <- shotclock(PbP, sec_14_after_oreb = FALSE, team_data=Tadd)
#' scoringprobability(PbP, team_name = "GSW", score_difference = c(-5, 1),
#'                    shot_type = "3P", team_data=Tadd)
#'
#' # probability that a free throw attempted in the last 5 minutes is made
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP <- scoredifference(PbP, team_name = "GSW", player_data=Pbox, team_data=Tadd)
#' PbP <- shotclock(PbP,  sec_14_after_oreb = FALSE, team_data=Tadd)
#' scoringprobability(PbP, team_name = "GSW", totaltime = 43, shot_type = "FT",
#'                   team_data=Tadd)
#' @export


scoringprobability <- function(PbP_data, team_name="" , shotclock_interval=c(0, 24), totaltime=0, score_difference=c(-100, 100), shot_type="field", team_data) {

  shotclock <- NULL; score.diff <- NULL; totalTime <- NULL; ShotType=NULL; team=NULL; result=NULL;
  if (length(shot_type) == 1)
  {
    if ((shotclock_interval[1] != 0 | shotclock_interval[2] != 24) & totaltime == 0 & score_difference[1] == -100 & score_difference[2] == 100 & shot_type == "FT")
    {
      stop("It is not possible to study free throws while taking into consideration the value of shotclock")
    }
  }

  # if the name of the team has been specified, we need it to be in the shortened version (e.g. CLE)
  if (team_name != "")
  {
    if (team_name %in% team_data$team){

    }
    else if (team_name %in% team_data$Team){
      team_name <- toString(team_data$team[team_data$Team == team_name])
    }
    else{
      stop("Team name not valid")
    }
  }

  # shotclock_interval must have two values, where the first has to be above 0 and the second one below 24
  if (length(shotclock_interval) == 1)
  {
    shotclock_interval[1] <- 0
    shotclock_interval[2] <- min(24, shotclock_interval[2])
  }
  else
  {
    shotclock_interval[1] <- max(0, shotclock_interval[1])
    shotclock_interval[2] <- min(24, shotclock_interval[2])
  }

  if (shot_type[1] == "field")
    shot_type <- c("2P", "3P")

  totaltime <- min(max(0, totaltime), 48)

  # we consider only the rows corresponding to the shot_type and the team we are interested in (if no team_name is specified, then we will consider data for all the teams)
  PbP_data <- subset(PbP_data, ShotType %in% shot_type & grepl(team_name, team) )

  # if we don't have the default values, we need to find a subset of PbP_data (in most cases we directly have as an input the PbP_data only of the shots we are interested in)
  if (!(shotclock_interval[1] == 0 & shotclock_interval[2] == 24 & totaltime == 0 & score_difference[1] == -100 & score_difference[2] == 100))
  {
    # we are not using the default values: we need to define the subset of the original PbP_data
    PbP_data <- subset(PbP_data, shotclock>=shotclock_interval[1] & shotclock<=shotclock_interval[2] & score.diff>score_difference[1] & score.diff<=score_difference[2] & totalTime >= 60*totaltime)
  }

  # the probability of making a shot is computed as mde_shots/total_shots
  n_shots_made <- nrow(subset(PbP_data, result=="made"))
  n_shots_total <- nrow(PbP_data)

  return(n_shots_made/n_shots_total)
}
