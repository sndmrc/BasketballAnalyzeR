#' Calculate Tbox, Obox and Pbox
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param data a play-by-play  data frame
#' @param team character, team
#' @seealso \code{\link{PbPmanipulation}}
#' @return A list with the following elements
#' @return * \code{Tbox}, (completare descrizione)
#' @return * \code{Obox}, (completare descrizione)
#' @return * \code{Pbox}, (completare descrizione)
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @examples
#' library(operators)
#' library(dplyr)
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP <- PbP %>%
#'   mutate(oreb = type %~% "rebound offensive",
#'          dreb = type %~% "rebound defensive",
#'                 turnover = event_type=="turnover",
#'                 PF = (event_type == "foul") & !(type %~% "technical") ) %>%
#'         mutate(across(c(player, assist, steal, block, h1:h5, a1:a5), as.character)) %>%
#'  as.data.frame()
#' out <- TOPboxes(PbP, team="GSW")
#' @export
#' @importFrom dplyr is.tbl
#' @importFrom dplyr sym
#' @importFrom dplyr filter
#' @importFrom dplyr summarize
#' @importFrom dplyr right_join
#' @importFrom dplyr rename
#' @importFrom data.table `:=`

TOPboxes <- function(data, team) {

  ShotType <- shottype <- result <- P2M <- P2A <- P3M <- P3A <-
    FTM <- FTA <- oreb <- dreb <- turnover <- assist01 <- PF <-
    h1 <- h2 <- h3 <- h4 <- h5 <- a1 <- a2 <- a3 <- a4 <- a5 <-
    assist <- player <- hometeam <- game_id <- playlength <-
    name <- PTS <- P2p <- P3p <- FTp <- OREB <- DREB <- TOV <-
    PF <- PM <- BLK <- AST <- block <- steal <- NULL
  if (dplyr::is.tbl(data)) {
    data <- as.data.frame(data)
  }

  # Check if required variables are all available in data
  if (!any(c("shottype", "ShotType") %in% names(data))) {
    stop("Attention. Variable ShotType (or shottype) is missing")
  }
  required_vars <- c("game_id", "playlength", "points", "result", "oreb", "dreb",
                     "turnover", "assist", "steal", "block", "PF")
  if (!all(required_vars %in% names(data))) {
    missing_vars <- required_vars[!(required_vars %in% names(data))]
    stop(paste0("Attention. One or more variables are missing: ", paste0(missing_vars, collapse=", ")))
  }

  # Change 'ShotType' name into 'shottype'
  if (any("ShotType" %in% names(data))) {
    data <- data %>% dplyr::rename(shottype=ShotType)
  }

  recod_vars <- c("assist", "steal", "block")
  for (x in recod_vars) {
    var01 <- paste0(x,"01")
    if (is.character(data[, x])) {
      data <- data %>% mutate(!!var01 := !!sym(x)!="" )
    } else if (is.factor(data[, x])) {
      data <- data %>% mutate(!!var01 := as.character(!!sym(x))!="" )
    } else {
      data <- data %>% mutate(!!var01 := !!sym(x) )
    }
  }

  subdataT <- data %>% filter(team == !!team)
  subdataO <- data %>% filter(team != !!team)

  ### Tbox
  Tbox <- subdataT %>%
    summarize(GP  = length(unique(data$game_id)),
              MIN = ceiling(sum(data$playlength)/60),
              PTS = sum(points, na.rm=TRUE),
              P2M = sum(shottype=="2P" & result=="made", na.rm=TRUE),
              P2A = sum(shottype=="2P", na.rm=TRUE),
              P2p = 100*P2M/P2A,
              P3M = sum(shottype=="3P" & result=="made", na.rm=TRUE),
              P3A = sum(shottype=="3P", na.rm=TRUE),
              P3p = 100*P3M/P3A,
              FTM = sum(shottype=="FT" & result=="made", na.rm=TRUE),
              FTA = sum(shottype=="FT", na.rm=TRUE),
              FTp = 100*FTM/FTA,
              OREB = sum(oreb, na.rm=TRUE),
              DREB = sum(dreb, na.rm=TRUE),
              TOV = sum(turnover, na.rm=TRUE),
              AST = sum(assist01, na.rm=TRUE),
              STL = sum(subdataO$steal01, na.rm=TRUE),
              BLK = sum(subdataO$block01, na.rm=TRUE),
              PF  = sum(PF, na.rm=TRUE),
              PM  = sum(points, na.rm=TRUE) - sum(subdataO$points, na.rm=TRUE)
    )

  ### Obox
  Obox <- subdataO %>%
    summarize(GP  = length(unique(data$game_id)),
              MIN = ceiling(sum(data$playlength)/60),
              PTS = sum(points, na.rm=TRUE),
              P2M = sum(shottype=="2P" & result=="made", na.rm=TRUE),
              P2A = sum(shottype=="2P", na.rm=TRUE),
              P2p = 100*P2M/P2A,
              P3M = sum(shottype=="3P" & result=="made", na.rm=TRUE),
              P3A = sum(shottype=="3P", na.rm=TRUE),
              P3p = 100*P3M/P3A,
              FTM = sum(shottype=="FT" & result=="made", na.rm=TRUE),
              FTA = sum(shottype=="FT", na.rm=TRUE),
              FTp = 100*FTM/FTA,
              OREB = sum(oreb, na.rm=TRUE),
              DREB = sum(dreb, na.rm=TRUE),
              TOV = sum(turnover, na.rm=TRUE),
              AST = sum(assist01, na.rm=TRUE),
              STL = sum(subdataT$steal01, na.rm=TRUE),
              BLK = sum(subdataT$block01, na.rm=TRUE),
              PF  = sum(PF, na.rm=TRUE),
              PM  = sum(points, na.rm=TRUE) - sum(subdataT$points, na.rm=TRUE)
    )
  output_list <- list(Tbox=Tbox, Obox=Obox)


  if (any(names(data) %in% "player") & any(names(data) %in% "hometeam") &
      all(c("h1", "h2", "h3", "h4", "h5", "a1", "a2", "a3", "a4", "a5") %in% names(data))) {

    ### Pbox
    # Find the set of players in "team"
    home_players <- data %>%
      filter(hometeam == !!team) %>%
      select(hometeam, h1:h5) %>%
      tidyr::pivot_longer(cols=h1:h5, names_to=NULL, values_to="name") %>%
      group_by(name) %>% summarize()
    away_players <- data %>%
      filter(hometeam != !!team) %>%
      select(hometeam, a1:a5) %>%
      tidyr::pivot_longer(cols=a1:a5, names_to=NULL, values_to="name") %>%
      group_by(name) %>% summarize()

    team_players <- unique(as.character(c(home_players$name,
                                          away_players$name)))

    Pbox <- subdataT %>%
      filter(player %in% team_players) %>%
      group_by(player) %>%
      summarize(
        PTS = sum(points, na.rm=T),
        P2M = sum(shottype=="2P" & result=="made", na.rm=T),
        P2A = sum(shottype=="2P", na.rm=T),
        P2p = 100*P2M/P2A,
        P3M = sum(shottype=="3P" & result=="made", na.rm=T),
        P3A = sum(shottype=="3P", na.rm=T),
        P3p = 100*P3M/P3A,
        FTM = sum(shottype=="FT" & result=="made", na.rm=T),
        FTA = sum(shottype=="FT", na.rm=T),
        FTp = 100*FTM/FTA,
        OREB = sum(oreb, na.rm=TRUE),
        DREB = sum(dreb, na.rm=TRUE),
        TOV = sum(turnover, na.rm=T),
        PF  = sum(PF, na.rm=T),
      ) %>%
      as.data.frame()

    plyrs <- sort(unique(Pbox$player))
    GP <- lapply(plyrs, function(plrk) {
      dtsk <- data %>%
        filter( h1==plrk | h2==plrk | h3==plrk | h4==plrk | h5==plrk |
                  a1==plrk | a2==plrk | a3==plrk | a4==plrk | a5==plrk) %>%
        summarize(GP=length(unique(game_id))) %>%
        as.data.frame()
      df <- data.frame(player=plrk, GP=dtsk$GP)
      return(df)
    })
    GP <- do.call(rbind, GP)
    Pbox <- Pbox %>% left_join(GP, by="player")

    MIN <- lapply(plyrs, function(plrk) {
      dtsk <- data %>%
        filter( h1==plrk | h2==plrk | h3==plrk | h4==plrk | h5==plrk |
                  a1==plrk | a2==plrk | a3==plrk | a4==plrk | a5==plrk) %>%
        summarize(MIN=floor(sum(playlength, na.rm=T)/60)) %>%
        as.data.frame()
      df <- data.frame(player=plrk, MIN=dtsk$MIN)
      return(df)
    })
    MIN <- do.call(rbind, MIN)
    Pbox <- Pbox %>% left_join(MIN, by="player")


    PTS_T <- lapply(plyrs, function(plrk) {
      PTS <- subdataT %>%
        filter(h1==plrk | h2==plrk | h3==plrk | h4==plrk | h5==plrk |
                 a1==plrk | a2==plrk | a3==plrk | a4==plrk | a5==plrk) %>%
        summarize(PTS=sum(points, na.rm=T)) %>%
        as.data.frame()
      df <- data.frame(player=plrk, PTS_T=PTS$PTS)
      return(df)
    })
    PTS_T <- do.call(rbind, PTS_T)

    PTS_O <- lapply(plyrs, function(plrk) {
      PTS <- subdataO %>%
        filter(h1==plrk | h2==plrk | h3==plrk | h4==plrk | h5==plrk |
                 a1==plrk | a2==plrk | a3==plrk | a4==plrk | a5==plrk) %>%
        summarize(PTS=sum(points, na.rm=T)) %>%
        as.data.frame()
      df <- data.frame(player=plrk, PTS_O=PTS$PTS)
      return(df)
    })
    PTS_O <- do.call(rbind, PTS_O)
    Pbox <- left_join(PTS_T, PTS_O, by="player") %>%
      mutate(PM = PTS_T - PTS_O) %>%
      right_join(Pbox, by="player")

  }

  if (exists("Pbox", envir = environment(), inherits = FALSE) &
      all(sapply(data[,c("assist", "steal", "block")], is.character))) {
    AST <- subdataT %>%
      group_by(assist) %>%
      summarize(AST=sum(assist!="", na.rm=TRUE)) %>%
      rename(player=assist) %>%
      filter(player!="")

    STL <- subdataO %>%
      group_by(steal) %>%
      summarize(STL=sum(steal!="", na.rm=TRUE)) %>%
      rename(player=steal) %>%
      filter(player!="")

    BLK <- subdataO %>%
      group_by(block) %>%
      summarize(BLK=sum(block!="", na.rm=TRUE)) %>%
      rename(player=block) %>%
      filter(player!="")

    Pbox <- Pbox %>%
      left_join(AST, by="player") %>%
      left_join(STL, by="player") %>%
      left_join(BLK, by="player")
  }

  if (exists("Pbox", envir = environment(), inherits = FALSE)){
    if(all(c("AST", "STL", "BLK") %in% names(Pbox))) {
      Pbox <- Pbox %>%
        mutate(AST=ifelse(is.na(AST),0,AST),
               STL=ifelse(is.na(STL),0,STL),
               BLK=ifelse(is.na(BLK),0,BLK)) %>%
        select(player, GP, MIN, PTS, P2M, P2A, P2p, P3M, P3A,
               P3p, FTM, FTA, FTp, OREB, DREB, AST, TOV, STL,
               BLK, PF, PM)
    } else {
      Pbox <- Pbox %>%
        select(player, GP, MIN, PTS, P2M, P2A, P2p, P3M, P3A,
               P3p, FTM, FTA, FTp, OREB, DREB, TOV, PF, PM)
    }
    output_list$Pbox <- Pbox
  }

  return(output_list)
}
