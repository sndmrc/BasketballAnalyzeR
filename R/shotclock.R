#' Computes, for each action, an estimate of the value of the shotclock when the action has ended
#'
#' @author Andrea Fox
#' @param PbP_data  a play-by-play dataframe, previously handled by the function PbPmanipulation
#' @param team_data dataframe, contains several data regarding the teams in the NBA. Inside this function it is used only to check if \code{team_name} corresponds to a team in the NBA. If the teams in the play-by-play data studied are the same as in the 2017-18 season, \code{Tadd} (the dataframe contained in the \code{BasketballAnalyzeR} package, regarding the 2017-18 season) can be used
#' @param sec_14_after_oreb boolean, it indicates if the shotclock has been set to 14 seconds in certain situations. It has to be true if the data have been recorded after the 2018-19 season. The default value is \code{FALSE}
#' @param report boolean, if TRUE, the function prints a few details about some data which have a negative value of shotclock (and therefore have been correceted)
#' @param verbose boolean, if TRUE, adds some comments about the computations
#' @param seconds_added_after_made_shot numeric value, after a shot is made the period clock is not stopped (unless it is in the last minutes of each quarter), hence a certain number of seconds has to be added in order to take account of the seconds taken for the inbound pass
#' @param max_error_threshold numeric value, some errors still occur in the data and some negative values of shotclock are produced (in general due to some delay between the end of the action and its registration). This parameters indicates the maximum absolute value of negative shotclock which is arbitrarily fixed to a positive value; the values of shotclock below this threshold are set as NAs
#' @details It is necessary that the name of the team is contained in the column corresponding to the description
#' @return  The play-by-play data, with the additional data regarding the value of shotclock and the boolean indicating whether the action has started with a value of shotclock equal to 14 seconds
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @references P. Zuccolotto, M. Manisera and M. Sandri (2018) Big data analytics for modeling scoring probability in basketball: The effect of shooting under high pressure conditions. International Journal of Sports Science & Coaching.
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP <- shotclock(PbP_data = PbP, team_data = Tadd)
#' @export


shotclock <- function(PbP_data, team_data, sec_14_after_oreb = FALSE, report = FALSE, verbose = FALSE,
                      seconds_added_after_made_shot = 2, max_error_threshold = 4) {
  event_type <- NULL
  timeout <- c("timeout") # event_type
  substitution <- c("sub")  # event_type
  free_throw <- c("free throw")   # event_type
  rebound_offensive <- c("rebound offensive")
  loose_ball_foul <- c("l.b.foul")
  team_rebound <- c("team rebound")
  made_shot <- c("shot")   # event_type
  shot <- c("shot","miss")   # event_type
  foul <- c("foul")        # event_type
  kicked_ball_violation <- c("kicked ball violation", "violation:kicked ball")
  jump_ball <- c("jump ball") #,"violation:jump ball" )
  turnover <- c("turnover")
  delay_of_game_violation <- c("violation:delay of game violation")
  free_throw_technical <- c("free throw technical")

  shotclock <- c(24, 24, 24 - PbP_data$playlength[3],rep(0, nrow(PbP_data)-3))
  shortened_action <- rep(FALSE, nrow(PbP_data))
  PbP_data <- addteamrebound(PbP_data, team_data)
  PbP_data <- addteamstartend(PbP_data,team_data)

  for (i in 4: nrow(PbP_data)){
    # if the action is a substitution or a timeout, the value of shotclock will be set to -1
    if (PbP_data$event_type[i] %in% timeout | PbP_data$event_type[i] %in% substitution)
    {
      shotclock[i] <- -1
      shortened_action[i] <- FALSE
    }

    else{
      j <- 1
      while(PbP_data$event_type[i-j] %in% timeout | PbP_data$event_type[i-j] %in% substitution){
        j <- j+1
      }
      if (verbose)
        print(paste("i = ", i, " j = ", j))

      if (PbP_data$event_type[i] %in% free_throw){
        # free throws have a shotclock value of 24
        shotclock[i] <- 24
        shortened_action[i] <- FALSE
      }
      else if ( PbP_data$periodTime[i-j] >= 12*60-24  & PbP_data$period[i] <= 4 )
      {
        if (verbose)
          print("the previous action ended when the period clock was already below 24 seconds (during the first 4 periods)")
        shotclock[i] <- 60*12 - PbP_data$periodTime[i-j] - PbP_data$playlength[i]
        shortened_action[i] <- FALSE
      }
      else if ( PbP_data$event_type[i-j] %in% made_shot &  ((PbP_data$periodTime[i-j] <= 11*60 & PbP_data$period[i] <= 3) | (PbP_data$periodTime[i-j] <= 10*60 & PbP_data$period[i] >= 4) ) )
      {
        if (verbose)
          print("the previous action was a made shot and it was made when the period clock is not stopped: we lose a couple of seconds in order to put the ball back in play")
        shotclock[i] <- 24 + seconds_added_after_made_shot - PbP_data$playlength[i]
        shortened_action[i] <- FALSE
      }
      else if ( PbP_data$type[i-j] %in% rebound_offensive & sec_14_after_oreb == TRUE)
      {
        if (verbose)
          print("The offensive team is the first to gain possession after an unsuccessful free throw that remains in play, or an unsuccessful field goal attempt that contacts the basket ring (after 2018-19 season)")
        shotclock[i] <- 14 - PbP_data$playlength[i]
        shortened_action[i] <- TRUE
      }
      else if ( PbP_data$type[i-j] %in% loose_ball_foul & PbP_data$type[i-j-1] %in% team_rebound & PbP_data$team[i-j-2] != PbP_data$team[i] & sec_14_after_oreb )
      {
        if (verbose)
          print("A loose ball foul is called on the defensive team in the sequence immediately following an unsuccessful free throw that remains in play, or an unsuccessful field goal attempt that contacts the basket ring; provided that, as a result of the foul, the offensive team inbounds the ball in the frontcourt (after 2018-19 season) ")
        shotclock[i] <- 14 - PbP_data$playlength[i]
        shortened_action[i] <- TRUE
      }
      else if ( PbP_data$type[i-j] %in% team_rebound & PbP_data$team[i-j-1] == PbP_data$team[i-j] & sec_14_after_oreb & toString(PbP_data$block[i-j-1]) == "" )
      {
        if (verbose)
          print("The offensive team retains possession after the ball goes out of bounds in the sequence immediately following an unsuccessful free throw that remains in play, or an unsuccessful field goal attempt that contacts the basket ring (after 2018-19 season")
        shotclock[i] <- 14 - PbP_data$playlength[i]
        shortened_action[i] <- TRUE
      }
      # The shot clock shall remain the same as when play was interrupted or reset to 14 seconds, whichever is greater, anytime the following occurs:
      else if ( PbP_data$event_type[i] %in% shot & PbP_data$event_type[i-j] %in% foul & PbP_data$team[i] != PbP_data$team[i-j] & shotclock[i-j] < 14 )
      {
        if (verbose)
          print("Personal foul by the defense where ball is being inbounded in frontcourt")
        if (shotclock[i-j] < 14)
        {
          shotclock[i] <- 14 - PbP_data$playlength[i]
          shortened_action[i] <- TRUE
        }
        else
        {
          shotclock[i] <- shotclock[i-j] - PbP_data$playlength[i]
          shortened_action[i] <- FALSE
        }
      }
      else if (PbP_data$type[i-j] %in% kicked_ball_violation)
      {
        if (verbose)
          print("Kicked or punched ball by the defensive team with the ball being inbounded in the offensive team's frontcourt")
        if (shotclock[i-j] < 14)
        {
          shotclock[i] <- 14 - PbP_data$playlength[i]
          shortened_action[i] <- TRUE
        }
        else
        {
          shotclock[i] <- shotclock[i-j] - PbP_data$playlength[i]
          shortened_action[i] <- FALSE
        }
      }
      else if ( PbP_data$type[i-j] %in% jump_ball & PbP_data$type[i-j+1] != turnover )
      {
        if (verbose)
          print("Jump balls retained by the offensive team as the result of any violation by the defensive team during a jump ball which results in a frontcourt throw-in")
        if (shotclock[i-j] < 14)
        {
          shotclock[i] <- 14 - PbP_data$playlength[i]
          shortened_action[i] <- TRUE
        }
        else
        {
          shotclock[i] <- shotclock[i-j] - PbP_data$playlength[i]
          shortened_action[i] <- FALSE
        }
      }
      else if(  PbP_data$type[i-j] %in% delay_of_game_violation)
      {
        if (verbose)
          print("Technical fouls and/or delay-of-game warnings on the defensive team")
        if (shotclock[i-j] < 14)
        {
          shotclock[i] <- 14 - PbP_data$playlength[i]
          shortened_action[i] <- TRUE
        }
        else
        {
          shotclock[i] <- shotclock[i-j] - PbP_data$playlength[i]
          shortened_action[i] <- FALSE
        }
      }
      else if (PbP_data$type[i-j] %in% free_throw_technical)
      {
        if (verbose)
          print("Defensive three-second violation or technical foul")
        if (shotclock[i-j-1] < 14)
        {
          shotclock[i] <- 14 - PbP_data$playlength[i]
          shortened_action[i] <- TRUE
        }
        else
        {
          shotclock[i] <- shotclock[i-j-1] - PbP_data$playlength[i]
          shortened_action[i] <- FALSE
        }
      }
      else if (toString(PbP_data$block[i-j-1]) != "" & PbP_data$type[i-j] %in% team_rebound & PbP_data$team[i-j] == PbP_data$team[i-j-1] )
      {
        if (verbose)
          print("previous action is a shot missed due to a block, the ball goes out of bounds and the possession remains to the offensive team")
        shotclock[i] <- shotclock[i-j-1] - PbP_data$playlength[i]
        shortened_action[i] <- FALSE
      }
      else if ( toString(PbP_data$block[i-j-1]) != "" & PbP_data$type[i-j] %in% rebound_offensive & PbP_data$team[i-j] == PbP_data$team[i-j-1] )
      {
        if (verbose)
          print("previous action is a shot missed due to a block, the ball remains on the field and the possession remains to the offensive team")
        shotclock[i] <- shotclock[i-j-1] - PbP_data$playlength[i]
        shortened_action[i] <- FALSE
      }
      else
      {
        if (verbose)
          print("none of the above")
        shotclock[i] <- 24 - PbP_data$playlength[i]
        shortened_action[i] <- FALSE
      }
    }
  }

  if (!"shotclock" %in% colnames(PbP_data))
    PbP_data <- data.frame(PbP_data, shotclock = shotclock)
  else
    PbP_data$shotclock <- shotclock

  if (!"shortened_action" %in% colnames(PbP_data))
    PbP_data <- data.frame(PbP_data, shortened_action = shortened_action)
  else
    PbP_data$shortened_action <- shortened_action

  if(report)
  {
    negative_values_subset <- subset(PbP_data, (shotclock < 0) & (!event_type %in% timeout) & (!event_type %in% substitution) & (event_type %in% shot) )
    print(table(negative_values_subset$shotclock))

    warning( paste( nrow(negative_values_subset), " shots have a negative value of shotclock") )
  }

  for (i in 1:length(shotclock)){
    if (PbP_data$shotclock[i] >= -max_error_threshold & PbP_data$shotclock[i]<= -1 & (!PbP_data$event_type[i] %in% substitution) & (!PbP_data$event_type[i] %in% timeout))
      PbP_data$shotclock[i] <- 0
    else if (PbP_data$shotclock[i] < -max_error_threshold)
      PbP_data$shotclock[i] <- NA
  }
  return(PbP_data)
}


#' @noRd
addteamrebound <- function(PbP_data, team_data){
  for (i in 1:nrow(PbP_data)){
    if (PbP_data$type[i] == "team rebound"){
      # we need to look for the name of the team in the description and then associate it to the abbreviation in team_data
      team_name <- strsplit(as.character(PbP_data$description[i]), " ")[[1]][1]
      index_team <- 1
      while ( !grepl(tolower(team_name), tolower(team_data$Team[index_team]))) {
        index_team <- index_team+1
      }
      PbP_data$team[i] <- toString(team_data$team[index_team])

      # we now need to find the name of the opposing team by looking at the names of the teams in previous lines
      j <- 1
      while( ((!toString(PbP_data$team[i-j]) %in% team_data$team) | (!toString(PbP_data$oppTeam[i-j]) %in% team_data$team)) )
      {
        j <- j+1
      }


      if( toString(PbP_data$team[i-j]) != toString(PbP_data$team[i]) & toString(PbP_data$oppTeam[i-j]) == toString(PbP_data$team[i]) )
        PbP_data$oppTeam[i] <- toString(PbP_data$team[i-j])
      else if ( toString(PbP_data$team[i-j]) == toString(PbP_data$team[i]) & toString(PbP_data$oppTeam[i-j]) != toString(PbP_data$team[i]) )
        PbP_data$oppTeam[i] <- toString(PbP_data$oppTeam[i-j])
    }
  }
  return(PbP_data)
}

#' @noRd
addteamstartend <- function(PbP_data, team_data) {
  n <- nrow(PbP_data)
  for (i in 1:n)
  {
    if (PbP_data$event_type[i] == "start of period")
    {
      j <- 1
      while( (!toString(PbP_data$team[i+j]) %in% team_data$team) | (!toString(PbP_data$oppTeam[i+j]) %in% team_data$team) )
        j <- j+1
      PbP_data$team[i] <- toString(PbP_data$team[i+j])
      PbP_data$oppTeam[i] <- toString(PbP_data$oppTeam[i+j])
    }
    else if (PbP_data$event_type[i] == "end of period")
    {
      j <- 1
      while( (!toString(PbP_data$team[i-j]) %in% team_data$team) | (!toString(PbP_data$oppTeam[i-j]) %in% team_data$team) )
        j <- j+1
      PbP_data$team[i] <- toString(PbP_data$team[i-j])
      PbP_data$oppTeam[i] <- toString(PbP_data$oppTeam[i-j])
    }
    else if (PbP_data$team[i] == "" | PbP_data$oppTeam[i] == "")
    {
      j <- 1
      while( (!toString(PbP_data$team[i-j]) %in% team_data$team) | (!toString(PbP_data$oppTeam[i-j]) %in% team_data$team) )
        j <- j+1
      PbP_data$team[i] <- toString(PbP_data$team[i-j])
      PbP_data$oppTeam[i] <- toString(PbP_data$oppTeam[i-j])
    }
  }
  return(PbP_data)
}

