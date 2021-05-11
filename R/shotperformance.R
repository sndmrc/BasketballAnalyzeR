#' Computes, for each player of a specific team, its performance measure
#'
#' @author Andrea Fox
#' @param PbP_data  a play-by-play dataframe, previously handled by the functions PbPmanipulation, shotclock and scoredifference
#' @param shotclock_interval vector of two numeric values or single numeric value, condition on the value of shotclock of the shots that will be considered
#' @param totaltime vector of two numeric values, condition on the value of score.diff of the shots that will be considered
#' @param score_difference numeric value, condition on the value of totalTime of the shots that will be considered
#' @param shot_type character, the type of shots to be analyzed; available options: "2P", "3P", "FT", "field"
#' @param verbose boolean, if TRUE, adds some comments about the computations
#' @param min_shots minimum value of total shots that a player must have attempted in order to qualify for the computation of the performance statistic
#' @param min_shots_high_pressure minimum value of total shots that a player must have attempted in an high pressure situation in order to qualify for the computation of the performance statistic
#' @param teams character or vector of characters, indicates the teams whose players we want to compute the performance statistics
#' @param player_data dataframe containing the boxscore data of all players of a particula season. We need it to know the players who have played at least one match for a team during the season. This dataframe might be substituted by a dataframe which has a column \code{Player} containing in each row the name of the players and a second columd \code{Team} containing the extended name (e.g. Golden State Warriors) of the team in which the player has played at least one match. If a player has played at least one match for more than one team during the same season, he/she will have a row for each franchise where has played
#' @param team_data dataframe, contains several data regarding the teams in the NBA. Inside this function it is used only to check if \code{team_name} corresponds to a team in the NBA. If the teams in the play-by-play data studied are the same as in the 2017-18 season, \code{Tadd} (the dataframe contained in the \code{BasketballAnalyzeR} package, regarding the 2017-18 season) can be used
#' @return A dataframe containing, for each player which fulfils the conditions on the minimum number of shots, the value of the overall performance, the performance difference in S, the propensity to shoot in S, the total number of shots and the total number of shots in the high pressure situation defined
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @references P. Zuccolotto, M. Manisera and M. Sandri (2018) Big data analytics for modeling scoring probability in basketball: The effect of shooting under high pressure conditions. International Journal of Sports Science & Coaching.
#' @examples
#' # We consider the high pressure situation of all shots attempted
#' # when the shotclock value is below 2 seconds
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP <- scoredifference(PbP_data = PbP, team_name = "GSW", player_data=Pbox, team_data = Tadd)
#' PbP <- shotclock(PbP_data = PbP, sec_14_after_oreb = FALSE, team_data = Tadd)
#' shotperformance(PbP_data = PbP, player_data = Pbox, team_data = Tadd,
#'                 shotclock_interval = c(0, 2) , shot_type = "2P")
#' @export
#' @importFrom stats na.omit

shotperformance <- function(PbP_data, player_data, team_data, shotclock_interval = c(0, 24), totaltime = 0,
                            score_difference = c(-100, 100), shot_type = "field", min_shots = 100,
                            min_shots_high_pressure = 10, verbose = FALSE, teams = "all") {

  team <- NULL; oppTeam <- NULL;
  if (shot_type == "field")  shot_type <- c("2P", "3P")
  if (length(teams) == 1 & teams[1] == "all") teams <- levels(team_data$team)
  if (length(teams) > 1)  {
    players_dataframe <- data.frame(name = character(),
                                    team = character(),
                                    overall_performance = integer(),
                                    performance_difference_in_S = integer(),
                                    propensity_to_shoot_in_S = integer(),
                                    n_shots = integer(),
                                    n_shots_high_pressure = integer())

    if (! (shotclock_interval[1] != 0 | shotclock_interval[2] != 24) ) {
      players_dataframe["minutes_played"] <- integer()
      players_dataframe["minutes_played_high_pressure"] <- integer()
    }
  }
  else
  {
    players_dataframe <- data.frame(name = character(),
                                    overall_performance = integer(),
                                    performance_difference_in_S = integer(),
                                    propensity_to_shoot_in_S = integer(),
                                    n_shots = integer(),
                                    n_shots_high_pressure = integer())

    if ( ! (shotclock_interval[1] != 0 | shotclock_interval[2] != 24) )
    {
      players_dataframe["minutes_played"] <- integer()
      players_dataframe["minutes_played_high_pressure"] <- integer()
    }

  }

  for (i in 1:length(teams))  {
    PbP.team <- subset(PbP_data, team == teams[i] | oppTeam == teams[i])

    # if we haven't computed the score difference, we can do it here
    if (!"score.diff" %in% colnames(PbP.team))
      PbP.team <- scoredifference(PbP_data = PbP.team, team_name = teams[i])

    dataframe_team <- shotperformanceteam(PbP_data = PbP.team, team_name = teams[i], shotclock_interval = shotclock_interval,
                                          totaltime = totaltime, score_difference = score_difference, shot_type = shot_type,
                                          min_shots = min_shots, min_shots_high_pressure = min_shots_high_pressure,
                                          player_data = player_data, team_data = team_data)

    if (length(teams) > 1)  {

      dataframe_team <- dataframe_team[1:(nrow(dataframe_team)-1), ]
      dataframe_team <- na.omit(dataframe_team)
      n_players_team <-nrow(dataframe_team)
      # if we have more than one team, we need to add a columns specifying the team of that player
      new_dataframe_team <- data.frame(name = dataframe_team$name,
                                       team = rep(teams[i], n_players_team),
                                       overall_performance = dataframe_team$overall_performance,
                                       performance_difference_in_S = dataframe_team$performance_difference_in_S,
                                       propensity_to_shoot_in_S = dataframe_team$propensity_to_shoot_in_S,
                                       n_shots = dataframe_team$n_shots,
                                       n_shots_high_pressure = dataframe_team$n_shots_high_pressure )


      if (ncol(dataframe_team)==8) {
        new_dataframe_team["minutes_played"] <- dataframe_team$minutes_played
        new_dataframe_team["minutes_played_high_pressure"] <- dataframe_team$minutes_played_high_pressure
      }
      players_dataframe <-rbind(players_dataframe, new_dataframe_team)
    }
    else
      players_dataframe <-rbind(players_dataframe, dataframe_team)


    if(verbose) {
      print(paste("Team = ", teams[i]))
      print(paste("Players of the team who satisfy the conditions = ", (nrow(dataframe_team))))
    }
  }
  out <- players_dataframe
  class(out) <- append("shotperformance", class(out))
  return(out)
}


#' @noRd
shotperformanceteam <- function(PbP_data, team_name, player_data, team_data, shotclock_interval = c(0, 24), totaltime =  0, score_difference = c(-100, 100), shot_type = "field", verbose = FALSE, min_shots = 100, min_shots_high_pressure = 10) {

  team <- NULL; oppTeam <- NULL; team <- NULL; ShotType <- NULL; player <- NULL; score.diff <- NULL; totalTime <- NULL; totalTime <- NULL; player <- NULL;
  # if the input of the shotcock interval is a single number x, it is tranformed into a pair (0, x). Moreover
  if (length(shotclock_interval) == 1)
  {
    shotclock_interval <- c(0, shotclock_interval)
    if (shotclock_interval[2] > 24)
    {
      warning("The second value of shotclock interval was higher than 24")
      shotclock_interval[2] <- min(24, shotclock_interval[2])
    }
  }
  else
  {
    if (shotclock_interval[1] <0)
    {
      warning("The first value of shotclock interval was smaller than 0")
      shotclock_interval[1] <- max(0, shotclock_interval[1])
    }
    if (shotclock_interval[2] > 24)
    {
      warning("The second value of shotclock interval was higher than 24")
      shotclock_interval[2] <- min(24, shotclock_interval[2])
    }
  }

  totaltime <- min(max(0, totaltime), 48)

  if (shot_type[1] == "field")
    shot_type <- c("2P", "3P")

  # we need both the shortened name (e.g CLE) and the extended name (e.g. Cleveland Cavaliers)
  if (team_name %in% team_data$team){
    team_name_short <- team_name
    team_name <- toString(team_data$Team[team_data$team == team_name_short])
  }
  else if (team_name %in% team_data$Team){
    team_name_short <- toString(team_data$team[team_data$Team == team_name])
  }
  else{
    stop(paste(team_name, "is not a valid team name"))
  }


  team_players <- player_data$Player[player_data$Team == team_name]
  n_players <- length(team_players)

  if (shotclock_interval[1] == 0 & shotclock_interval[2] == 24)
  {
    # we add a column with values of the minutes played (in total and in the high pressure situation)
    players_performances <- data.frame(name = c(team_players, team_name), overall_performance = rep(0,n_players+1), performance_difference_in_S = rep(0, n_players+1), propensity_to_shoot_in_S = rep(0, n_players+1), n_shots = rep(0, n_players+1), n_shots_high_pressure = rep(0, n_players+1), minutes_played = rep(0, n_players+1), minutes_played_high_pressure = rep(0, n_players+1))
  }
  else{
    players_performances <- data.frame(name = c(team_players, team_name), overall_performance = rep(0,n_players+1), performance_difference_in_S = rep(0, n_players+1), propensity_to_shoot_in_S = rep(0, n_players+1), n_shots = rep(0, n_players+1), n_shots_high_pressure = rep(0, n_players+1))
  }

  # PbP.shots is a subset of the original PbP that will contain only the data of the shots (of the correct type) of the team considered
  PbP.shots <- subset(PbP_data, team==team_name_short & ShotType %in% shot_type)
  n_shots_team <- nrow(PbP.shots)

  # average shot probability of all the shots of the correct type of the team considered
  average_shot_probability <- scoringprobability(PbP_data = PbP.shots, shot_type=shot_type)
  players_performances[n_players+1, 2] <- average_shot_probability

  average_shot_probability_high_pressure <- scoringprobability(PbP_data = PbP.shots, shotclock_interval = shotclock_interval, totaltime = totaltime, score_difference = score_difference, shot_type = shot_type)

  players_performances[n_players+1, 3] <- average_shot_probability_high_pressure - average_shot_probability


  if(verbose == TRUE) {
    print(paste("Average shot probability of the team = ", average_shot_probability))
  }

  # we create a subset of PbP.shots which will contain only the shots in the high pressure condition defined
  PbP.shots.high_pressure <- subset(PbP.shots, shotclock >= shotclock_interval[1] & shotclock <= shotclock_interval[2] & score.diff > score_difference[1] & score.diff <= score_difference[2] & totalTime >= 60*totaltime)

  n_shots_team_high_pressure <- nrow(PbP.shots.high_pressure)
  players_performances[n_players+1 , 5:6] <- c(n_shots_team, n_shots_team_high_pressure)

  team_propension_shoot_high_pressure <- shotpropension(PbP_data = PbP_data, player_name = "any", n_shots_team = n_shots_team,
                                                        n_shots_team_high_pressure = n_shots_team_high_pressure,
                                                        PbP.player = PbP.shots, PbP.player.high_pressure = PbP.shots.high_pressure,
                                                        shotclock_interval = shotclock_interval, totaltime = totaltime,
                                                        score_difference = score_difference, team_name = team_name, shot_type = shot_type)

  if (shotclock_interval[1]==0 & shotclock_interval[2] == 24)
  {
    # we have to add also the minutes played
    players_performances[n_players+1, c(4, 7, 8)] <- team_propension_shoot_high_pressure
  }
  else{
    players_performances[n_players+1, 4] <- team_propension_shoot_high_pressure
  }

  for (i in 1:n_players){
    PbP.shots.player <- subset(PbP.shots, player == team_players[i])
    n_shots <- nrow(PbP.shots.player)

    if (n_shots == 0)
    {
      # the player might have a name which is different in player_data and PbP
      for (j in 1:length(levels(as.factor(PbP.shots$player))))
      {
        if (comparenames(team_players[i], levels(as.factor(PbP.shots$player))[j]) )
          team_players[i] <- levels(as.factor(PbP.shots$player))[j]
      }
      PbP.shots.player <- subset(PbP.shots, player == team_players[i])
      n_shots <- nrow(PbP.shots.player)
    }


    if(verbose)
    {
      print(paste("player = ", team_players[i]))
      print(paste("n_shots = ", n_shots))
    }


    if (n_shots >= min_shots)
    {
      # we create a subset of PbP.shots.high_pressure specific for the player
      PbP.shots.player.high_pressure <- subset(PbP.shots.high_pressure, player == team_players[i])
      n_shots_high_pressure <- nrow(PbP.shots.player.high_pressure)

      if (verbose)
        print(paste("number of shots in high pressure situation = ", n_shots_high_pressure))

      if (n_shots_high_pressure >= min_shots_high_pressure)
      {
        # the function shotperformanceplayer will handle all these calculations
        player_performances <- shotperformanceplayer(PbP_data = PbP_data, PbP.shots = PbP.shots.player,
                                                    PbP.shots.high_pressure = PbP.shots.player.high_pressure,
                                                    shot_type = shot_type, shotclock_interval = shotclock_interval,
                                                    totaltime = totaltime, score_difference = score_difference,
                                                    player_name = team_players[i], team_name = team_name,
                                                    n_shots_team = n_shots_team,
                                                    n_shots_team_high_pressure = n_shots_team_high_pressure)
        # computation of the overall performance measure
        players_performances[i, 2:(length(player_performances)+1)] <- player_performances
      }
      else
      {
        if (ncol(players_performances) == 6)
          players_performances[i, 2:6] <- rep(NA, 5)
        else
          players_performances[i, 2:8] <- rep(NA, 7)
      }
    }
    else
    {
      if (ncol(players_performances) == 6)
        players_performances[i, 2:6] <- rep(NA, 5)
      else
        players_performances[i, 2:8] <- rep(NA, 7)
    }
  }
  return(players_performances)
}



#' @noRd
shotperformanceplayer <- function(PbP_data, PbP.shots, PbP.shots.high_pressure, shot_type = "field", shotclock_interval = c(0, 24),
                                  totaltime = 0, score_difference = c(-100, 100) , verbose = FALSE, player_name, team_name,
                                  n_shots_team, n_shots_team_high_pressure) {

  team <- NULL; ShotType <- NULL; player <- NULL; score.diff <- NULL; totalTime <- NULL;
  if(shot_type[1] == "field")
    shot_type <- c("2P", "3P")

  if (missing(PbP.shots))
    PbP.shots <- subset(PbP_data, team == team_name & ShotType %in% shot_type & player == player_name)
  if (missing(PbP.shots.high_pressure))
    PbP.shots.high_pressure <- subset(PbP.shots, shotclock >= shotclock_interval[1] & shotclock <= shotclock_interval[2] & score.diff > score_difference[1] & score.diff <= score_difference[2] & totalTime >= 60*totaltime)

  if(missing(n_shots_team))
    n_shots_team <- nrow(subset(PbP_data, team == team_name & ShotType %in% shot_type))

  if(missing(n_shots_team_high_pressure))
    n_shots_team_high_pressure <- nrow(subset(PbP_data, team == team_name & ShotType %in% shot_type & shotclock >= shotclock_interval[1] & shotclock <= shotclock_interval[2] & score.diff > score_difference[1] & score.diff <= score_difference[2] & totalTime >= 60*totaltime ))


  if (shotclock_interval[1]==0 & shotclock_interval[2] == 24)
    player_performances <- rep(0, 7)
  else
    player_performances <- rep(0, 5)

  n_shots <- nrow(PbP.shots)
  n_shots_high_pressure <- nrow(PbP.shots.high_pressure)


  # computation of the performance measure for shots in high pressure situation
  if (n_shots_high_pressure == 0)
    player_performance_high_pressure <- 0
  else
  {
    player_performance_high_pressure <-scoringprobability(PbP.shots.high_pressure, team_name = "",  shot_type = shot_type, shotclock_interval = shotclock_interval, totaltime = totaltime, score_difference = score_difference)
  }

  overall_performance <- scoringprobability(PbP.shots, team_name = "",  shot_type = shot_type)
  player_performances[1] <- overall_performance

  player_performances[2] <- player_performance_high_pressure - overall_performance

  # propension to shoot under pressure

  shooting_propension_in_high_pressure <- shotpropension(PbP_data = PbP_data, n_shots_team = n_shots_team,
                                                         n_shots_team_high_pressure =  n_shots_team_high_pressure,
                                                         PbP.player = PbP.shots, PbP.player.high_pressure = PbP.shots.high_pressure,
                                                         player_name = player_name, shotclock_interval = shotclock_interval,
                                                         totaltime = totaltime, score_difference = score_difference,
                                                         team_name = team_name)

  if (shotclock_interval[1]==0 & shotclock_interval[2] == 24)
  {
    # we have to add also the minutes played
    player_performances[c(3, 6, 7)] <- shooting_propension_in_high_pressure
  }
  else
  {
    player_performances[3] <- shooting_propension_in_high_pressure
  }
  player_performances[4] <- n_shots
  player_performances[5] <- n_shots_high_pressure

  return(player_performances)
}



#' @noRd
shotpropension <- function(PbP_data, n_shots_team, n_shots_team_high_pressure, PbP.player, PbP.player.high_pressure, player_name,
                           shotclock_interval = c(0, 24), totaltime = 0, score_difference = c(-100, 100), team_name,
                           verbose = FALSE, shot_type = "field") {

  player <- NULL; ShotType <- NULL; totalTime <- NULL; score.diff <- NULL; event_type <- NULL; team=NULL;
  if (length(shotclock_interval) == 1)
  {
    shotclock_interval <- c(shotclock_interval, 24)
    shotclock_interval[1] <- max(0, shotclock_interval[1])
  }
  else
  {
    shotclock_interval[1] <- max(0, shotclock_interval[1])
    shotclock_interval[2] <- min(24, shotclock_interval[2])
  }
  if (shot_type[1] == "field")
    shot_type <- c("2P", "3P")

  if (missing(PbP.player) | missing(PbP.player.high_pressure)){
    # if there is no data for the PbP specific for a player, we build it here
    PbP.player <- subset(PbP_data, player == player_name & ShotType %in% shot_type)

    PbP.player.high_pressure <- subset(PbP_data, player == player_name & shotclock >= shotclock_interval[1] &
                                         shotclock <= shotclock_interval[2] & totalTime >= totaltime &
                                         score.diff >= score_difference[1] & score.diff <= score_difference[2] &
                                         ShotType %in% shot_type)
  }

  # if there is no data for the shots of the team, we compute it here
  if (missing(n_shots_team))
  {
    n_shots_team <- nrow(subset(PbP_data, (event_type == "shot" | event_type == "miss") & team == team_name & ShotType %in% shot_type))
  }
  # if there is no data for the shots of the team in the high pressure situation, we compute it here
  if (missing(n_shots_team_high_pressure))
  {
    n_shots_team_high_pressure <- nrow(subset(PbP_data, (event_type == "shot" | event_type == "miss") &
                                                team == team_name & ShotType %in% shot_type & shotclock >= shotclock_interval[1] &
                                                shotclock <= shotclock_interval[2] & totalTime >= totaltime &
                                                score.diff >= score_difference[1] & score.diff <= score_difference[2]))
  }

  totaltime <- min(max(0, totaltime), 48)

  n_shots_player <- nrow(PbP.player)
  n_shots_player_high_pressure <- nrow(PbP.player.high_pressure)


  if (shotclock_interval[1] != 0 | shotclock_interval[2] != 24)
  {
    # the value of the shotclock is considered in the definition of the high pressure situation: we use formula (3)
    propension_to_shoot <- ( ((n_shots_player_high_pressure/n_shots_player)/(n_shots_team_high_pressure/n_shots_team)) -1) * 100
    return(propension_to_shoot)
  }
  else
  {
    # the value of the shotclock is NOT considered in the definition of the high pressure situation: we use formula (4)
    minutes_player <- minutesplayed(PbP_data = PbP_data, player_name =  player_name)

    minutes_player_high_pressure <- minutesplayed(PbP_data = PbP_data, player_name = player_name, totaltime = totaltime, score_difference = score_difference)


    propension_to_shoot <- ( ((n_shots_player_high_pressure/n_shots_player)/(minutes_player_high_pressure/minutes_player)) -1)*100

    if (verbose == TRUE)
    {
      print(paste("minutes played = ", minutes_player))
      print(paste("minutes played in high pressure = ", minutes_player_high_pressure))
      print(paste("propension = ", propension_to_shoot))
    }

    return(c(propension_to_shoot, minutes_player, minutes_player_high_pressure))
  }
}


#' @noRd
minutesplayed <- function(PbP_data, player_name, totaltime = 0, score_difference = c(-100, 100), verbose = FALSE) {

  game_id <- NULL; totalTime <- NULL; score.diff <- NULL; playeronthefield <- NULL; entered <- NULL; left <- NULL; event_type <- NULL;
  matches_id <- levels(as.factor(PbP_data$game_id))
  minutes_played_total <- 0
  for (match_id in matches_id)
  {
    minutes_played_current_match <- 0

    match <- subset(PbP_data, game_id==match_id & totalTime >= totaltime * 60 & score.diff <= score_difference[2] & score.diff >= score_difference[1])


    # if we have to consider only the action with a difference in points within a certain interval, we need to look at each action
    if (score_difference[1] != -100 | score_difference[2] != 100)
    {
      if (nrow(match) > 0)
      {
        player_on_field <- rep(FALSE, nrow(match))
        if (player_name == "any")
          player_actions <- match
        else
        {
          for (i in 1:nrow(match))
          {
            player_on_field[i] <- playeronthefield(PbP.row = match[i, ], player_name = player_name)
          }
          player_actions <- subset(match, player_on_field)

        }
        minutes_played_current_match <- sum(player_actions$playlength)
      }
      else
      {
        player_actions <- match
        minutes_played_current_match <- 0
      }
      # we want to extract only the data of actions in which the player was on the field


      #player_actions <- subset(match, player_on_field)

      # the minutes played during each match are obtained as the sum of values (expressed in seconds) of the playlength of each action in which the player was on the field

      if (verbose == TRUE)
      {
        print(paste("game id = ", match_id))
        print(paste("number of rows (actions on the PbP) in the match data = ", nrow(player_actions)))
        print(paste("minutes played during the match = ", minutes_played_current_match/60))
      }
      minutes_played_total <- minutes_played_total + minutes_played_current_match/60
    }
    else
    {
      # we don't have to take account of the score difference: we can only look at the substitutions and at the start/end of each period
      if (player_name == "any")
      {
        # we simply have to count the number of minutes in the timespan to consider
        max_period <- max(match$period)
        if (max_period == 4)
          minutes_played_current_match <- 48 - totaltime
        else
          minutes_played_current_match <- (max_period-4)*5 + 48 - totaltime
        minutes_played_total <- minutes_played_total + minutes_played_current_match
      }
      else
      {
        player_subs <- subset(match, entered == player_name | left== player_name | event_type == "end of period" | event_type == "start of period" )
        if (nrow(player_subs) == 1 & player_subs$event_type[1] == "end of period")
        {
          # if the player has never been substituted during the timespan considered (which always starts at some point in the fourth period and ends at the end of it): we only need to understand if he was on the field at the end of the period. If so, he's always been on the field, otherwise he's never played in the timespan considered
          if ( playeronthefield(player_subs[1, ], player_name = player_name) )
          {
            # the player was always on the field in the timespan considered
            minutes_played_current_match <- match$totalTime[nrow(match)]/60 - totaltime
          }
          else
          {
            # the player has never been on the field in the timespan considered
            minutes_played_current_match <- 0
          }
        }
        else
        {
          # the player has been substituted at least once, or the period considered lasts more than one period
          if( (player_subs$left[1] == player_name) | (player_subs$event_type[1] == "end of period" & playeronthefield(player_subs[1, ], player_name)) )
          {
            minutes_played_current_match <- minutes_played_current_match + (player_subs$totalTime[1] - totaltime*60)/60
          }
          for (i in 2:nrow(player_subs))
          {
            if( player_subs$left[i] == player_name | (player_subs$event_type[i] == "end of period" & playeronthefield(player_subs[i, ], player_name) ) )
            {
              minutes_played_current_match <- minutes_played_current_match + (player_subs$totalTime[i] - player_subs$totalTime[i-1])/60
            }
          }
        }
        minutes_played_total <- minutes_played_current_match + minutes_played_total
        if (verbose == TRUE)
        {
          print(paste("number of rows (subs or start/end of the period) in the match data = ", nrow(player_subs)))
          print(paste("minutes played during the match = ", minutes_played_current_match/60))
        }
      }
    }
  }
  return(minutes_played_total)
}



#' @noRd
playeronthefield <- function(PbP.row, player_name)
{
  return( ( (PbP.row$isHome[1] == TRUE) & (player_name %in% c(toString(PbP.row$h1[1]), toString(PbP.row$h2[1]), toString(PbP.row$h3[1]), toString(PbP.row$h4[1]), toString(PbP.row$h5[1]) ) ) ) |  ( (PbP.row$isHome[1] == FALSE) & (player_name %in% c(toString(PbP.row$a1[1]), toString(PbP.row$a2[1]), toString(PbP.row$a3[1]),toString(PbP.row$a4[1]),toString(PbP.row$a5[1]) ) ) ) )
}


