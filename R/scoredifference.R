#' Computes the score difference between the two teams in the match
#'
#' @author Andrea Fox
#' @param PbP_data a play-by-play data frame, previously handled by \code{PbPmanipulation}
#' @param team_name name of the team we are interested in. The name can be either shortened (e.g. CLE) or extended (e.g. Cleveland Cavaliers)
#' @param player_data dataframe containing the boxscore data of all players of a particula season. We need it to know the players who have played at least one match for a team during the season. This dataframe might be substituted by a dataframe which has a column \code{Player} containing in each row the name of the players and a second columd \code{Team} containing the extended name (e.g. Golden State Warriors) of the team in which the player has played at least one match. If a player has played at least one match for more than one team during the same season, he/she will have a row for each franchise where has played
#' @param team_data dataframe, contains several data regarding the teams in the NBA. Inside this function it is used only to check if \code{team_name} corresponds to a team in the NBA. If the teams in the play-by-play data studied are the same as in the 2017-18 season, \code{Tadd} (the dataframe contained in the \code{BasketballAnalyzeR} package, regarding the 2017-18 season) can be used
#' @details The score difference computed by the function can be different from the simple difference between the score of the home team and the one of the away team, as we have to take account of the points scored during an action. Indeed, the value of \code{score.diff} indicates the difference in the score while the action was played
#' @return the initial play-by-play dataframe, with two additional columns:
#' @return * \code{score.diff}: difference between the score of \code{team_name} and the score of the opposite team (see \code{details} for more informations)
#' @return  *\code{isHome}: boolean which indicates if \code{team_name} is the home team in that play-by-play row
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @references P. Zuccolotto, M. Manisera and M. Sandri (2018) Big data analytics for modeling scoring probability in basketball: The effect of shooting under high pressure conditions. International Journal of Sports Science & Coaching.
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP <- scoredifference(PbP, team_name="GSW", player_data=Pbox, team_data=Tadd)
#' @export
#'


scoredifference <- function(PbP_data, team_name, player_data, team_data) {

  team <- NULL; oppTeam <- NULL; event_type <- NULL;
  # we need both the shortened name (e.g CLE) and the extended name (e.g. Cleveland Cavaliers)
  if (team_name %in% team_data$team) {
    team_name_short <- team_name
    team_name <- toString(team_data$Team[team_data$team == team_name_short])
  }  else if (team_name %in% team_data$Team) {
    team_name_short <- toString(team_data$team[team_data$Team == team_name])
  } else {
    stop("Team name not valid")
  }
  PbP_data <- subset(PbP_data, team==team_name_short | oppTeam==team_name_short | event_type=="start of period" | event_type=="end of period")

  # number of match played: we look at the number of levels in our PbP
  match_played <- length(levels(as.factor(PbP_data$game_id)))
  if (!"score.diff" %in% colnames(PbP_data))
    PbP_data <- data.frame(PbP_data, score.diff =  rep(0, nrow(PbP_data)))
  if (!"isHome" %in% colnames(PbP_data))
    PbP_data <- data.frame(PbP_data, isHome = rep(TRUE, nrow(PbP_data)))
  home <- TRUE
  play_index <- 1
  match_index <- 1
  while (match_index <= match_played){
    #print(match_index)
    match_id <- as.numeric(PbP_data$game_id[play_index])
    #print(match_id)
    home <- ishometeam(PbP_data, team_name, play_index, player_data = player_data)
    while( play_index <= nrow(PbP_data) & as.numeric(PbP_data$game_id[play_index]) == match_id){
      if (home){
        # the team we're interested in is playing at home
        if ( !is.na(PbP_data$points[play_index]) & PbP_data$team[play_index] == team_name_short )
        {
          # the team we're interested in (home team) has made a shot
          PbP_data$score.diff[play_index] <- PbP_data$home_score[play_index] - PbP_data$points[play_index] - PbP_data$away_score[play_index]
        }
        else if ( !is.na(PbP_data$points[play_index]) & PbP_data$team[play_index] != team_name_short )
        {
          # opposite team (away) has amde a shot
          PbP_data$score.diff[play_index] <- PbP_data$home_score[play_index] - (PbP_data$away_score[play_index] - PbP_data$points[play_index])
        }
        else
        {
          # no team has made a shot
          PbP_data$score.diff[play_index] <- PbP_data$home_score[play_index] - PbP_data$away_score[play_index]
        }
        PbP_data$isHome[play_index] <- TRUE
      }
      else{
        if ( !is.na(PbP_data$points[play_index]) & PbP_data$team[play_index] == team_name_short )
        {
          # the team we're interested in (away) has made a shot
          PbP_data$score.diff[play_index] <- PbP_data$away_score[play_index] - PbP_data$points[play_index] - PbP_data$home_score[play_index]
        }
        else if ( !is.na(PbP_data$points[play_index]) & PbP_data$team[play_index] != team_name_short )
        {
          # the oppsite team (home) has made a shot
          PbP_data$score.diff[play_index] <- PbP_data$away_score[play_index] - (PbP_data$home_score[play_index] - PbP_data$points[play_index])
        }
        else
        {
          # no team has made a shot
          PbP_data$score.diff[play_index] <- PbP_data$away_score[play_index] - PbP_data$home_score[play_index]
        }
        PbP_data$isHome[play_index] <- FALSE
      }
      play_index <- play_index +1
    }
    match_index <- match_index +1
  }
  return(PbP_data)
}

#' @noRd
comparenames <- function(pbox_name, pbp_name) {
  diff_1 <- setdiff( strsplit(pbox_name, split = "")[[1]], strsplit(pbp_name, split = "")[[1]] ) %in% c(".", "'", "-")
  diff_2 <- setdiff( strsplit(pbp_name, split = "")[[1]], strsplit(pbox_name, split = "")[[1]] ) %in% c(".", "'", "-")
  equal <- TRUE
  if (length(diff_1) > 0)
  {
    for (i in 1:length(diff_1))
    {
      if (!diff_1[i])
        equal <- FALSE
    }
  }
  if (length(diff_2) > 0)
  {
    for (i in 1:length(diff_2))
    {
      if (!diff_2[i])
        equal <- FALSE
    }
  }
  return(equal)
}


#' @noRd
ishometeam <- function(PbP_data, team_name, play_index, player_data) {
  if (missing(play_index))
    play_index <- 1

  if (length(strsplit(team_name, split = "")[[1]]) == 3)
    return(FALSE)
  # players in the columns of the home team players
  home_team <- c(toString(PbP_data$h1[play_index]), toString(PbP_data$h2[play_index]), toString(PbP_data$h3[play_index]), toString(PbP_data$h4[play_index]), toString(PbP_data$h5[play_index]))

  # players of the team
  team_players <- player_data$Player[player_data$Team == team_name]

  # if team_name is the home team, we will find 5 players whose name appears both in home_team and team_players
  same_players <- 0
  for (i in 1:5){
    for (j in 1:length(team_players))
    {
      if (comparenames(home_team[i], team_players[j]))
        same_players <- same_players +1
    }
  }
  return(same_players == 5)
}


