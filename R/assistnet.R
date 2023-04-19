#' Investigates the network of assists-shots in a team
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param data a data frame whose rows are field shots and columns are variables to be specified in \code{assist}, \code{player}, \code{points}, \code{event.type} (see Details).
#' @param assist character, indicating the name of the variable with players who made the assists, if any.
#' @param player character, indicating the name of the variable with players who made the shot.
#' @param points character, indicating the name of the variable with points.
#' @param event.type character, indicating the name of the variable with type of event (mandatory categories are \code{"miss"} for missed field shots and \code{"shot"} for field goals).
#' @param normalize logical, if \code{TRUE} normalize the number of assist: 4 period.length (number of assists)/(minutes played in attack by each couple of players) (default \code{normalize=FALSE}).
#' @param period.length numerical, the length of a quarter in minutes (default: 12 minutes as in NBA)
#' @param time.thr numerical, (default \code{time.thr=0})
#' @details The \code{data} data frame could also be a play-by-play dataset provided that rows corresponding to events different from field shots are not coded as \code{"shot"} in the \code{event.type} variable. (To be completed)
#' @return A \code{list} with 3 elements, \code{assistTable} (a table), \code{nodeStats} (a data frame), and \code{assistNet} (a network object). See Details.
#' @return \code{assistTable}, the cross-table of assists made and received by the players.
#' @return \code{nodeStats}, a data frame with the following variables:
#' @return * \code{FGM} (fields goals made),
#' @return * \code{FGM_AST} (field goals made thanks to a teammate's assist),
#' @return * \code{FGM_ASTp} (percentage of \code{FGM_AST} over \code{FGM}),
#' @return * \code{FGPTS} (points scored with field goals),
#' @return * \code{FGPTS_AST} (points scored thanks to a teammate's assist),
#' @return * \code{FGPTS_ASTp} (percentage of \code{FGPTS_AST} over \code{FGPTS}),
#' @return * \code{AST} (assists made),
#' @return * \code{ASTPTS} (point scored by assist's teammates).
#' @return \code{assistNet}, an object of class \code{network} that can be used for further network analysis with specific R packages (see \code{\link[network]{network}})
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP.GSW <- subset(PbP, team=="GSW")
#' out <- assistnet(PbP.GSW)
#' @export
#' @importFrom network set.vertex.attribute
#' @importFrom tidyr replace_na
#' @importFrom dplyr across
#' @importFrom dplyr left_join
#' @importFrom utils txtProgressBar
#' @importFrom utils combn
#' @importFrom utils setTxtProgressBar
#' @importFrom gtools permutations

assistnet <- function(data, assist="assist", player="player", points="points",
                      event.type="event_type", normalize=FALSE, period.length=12, time.thr=0) {

  nr <- MIN <- FGM <- FGM_AST <- FGM_ASTp <- FGPTS <- FGPTS_AST <- FGPTS_ASTp <- player1 <- player2 <- NULL
  data <- data %>%
    dplyr::rename(assist=!!assist, player=!!player, points=!!points, event.type=!!event.type) %>%
    dplyr::mutate(across(c(assist, player,event.type), as.character))

  assist_player <- data %>%
    dplyr::filter(assist!="") %>%
    dplyr::select(assist, player)

  all_ast_plr <- sort(unique(unlist(assist_player)))
  assist_player$assist <- factor(assist_player$assist, levels=all_ast_plr)
  assist_player$player <- factor(assist_player$player, levels=all_ast_plr)

  mtx_num_assists <- as.matrix(table(assist_player, useNA="no"))
  if (nrow(mtx_num_assists)!=ncol(mtx_num_assists)) {
    stop("The number of players in 'assist' and 'player' variables are not the same.")
  }

  if (normalize) {
    num_min <- player_assist(data, all_ast_plr, 2, calc_min=TRUE) %>%
      dplyr::arrange(player1, player2)
    mtx_nr  <- matrix(num_min %>% dplyr::select(nr) %>% unlist(),  nrow=length(all_ast_plr))
    if (all(mtx_nr==mtx_num_assists)) {
      vec_MIN <- num_min %>% dplyr::select(MIN) %>% dplyr::mutate(MIN=ifelse(is.na(MIN),0,MIN)) %>% unlist()
      mtx_MIN <- matrix(vec_MIN, nrow=length(all_ast_plr))
      colnames(mtx_MIN) <- rownames(mtx_MIN) <- all_ast_plr
      #if (is.null(time.thr)) time.thr <- 0
      mtx_MIN_thr <- apply(mtx_MIN, c(1, 2), function(x) ifelse(is.na(x) | x<=time.thr, NA, x))
      mtx_assist_MIN_thr <- 4*period.length*mtx_num_assists/mtx_MIN_thr
      mtx_for_net <- apply(mtx_assist_MIN_thr, c(1, 2), function(x) ifelse(is.na(x), 0, as.integer(round(x))))
    } else {
      stop("Error. Something wrong when normalizing number of assists by time played")
    }
  } else {
    mtx_for_net <- mtx_num_assists
  }

  # Calculate some player/node statistics
  nodeData1 <- data %>%
    dplyr::group_by(player) %>%
    dplyr::filter(event.type=="shot") %>%
    dplyr::summarise(FGM=dplyr::n(),
                     FGM_AST=sum(assist!=""),
                     FGM_ASTp=100*FGM_AST/FGM,
                     FGPTS=sum(points, na.rm=TRUE),
                     FGPTS_AST=sum(points*(assist!=""), na.rm=TRUE),
                     FGPTS_ASTp=FGPTS_AST/FGPTS) %>%
    as.data.frame()
  nodeData2 <- data %>%
    dplyr::filter(assist!="") %>%
    dplyr::group_by(assist) %>%
    dplyr::summarise(AST=dplyr::n(), ASTPTS=sum(points, na.rm=TRUE)) %>%
    as.data.frame()
  nodeData <- merge(nodeData1, nodeData2, by.x="player", by.y="assist", all=T)

  net <- network::network(mtx_for_net, matrix.type="adjacency", directed=TRUE,
                          ignore.eval=FALSE,  names.eval="N")

  if (normalize) {
    out <- list(assistTable=mtx_num_assists, nodeStats=nodeData, assistNet=net,
                minTable=mtx_MIN, assistminTable=mtx_assist_MIN_thr,
                normalize=normalize, period.length=period.length)
  } else {
    out <- list(assistTable=mtx_num_assists, nodeStats=nodeData, assistNet=net,
                normalize=normalize)
  }
  class(out) <- append("assistnet", class(out))
  return(out)
}



#' @noRd
player_assist <- function(dts, players_set, k, calc_min=FALSE) {
  MIN.x <- MIN.y <- NULL
  # Couples of assist-player: calculating number of assists
  ktuples1 <- gtools::permutations(length(players_set), 2, players_set, repeats.allowed=TRUE)
  n1 <- nrow(ktuples1)
  vars_ply_nm1 <- c("player", "assist")
  plyrs1 <- dts[, vars_ply_nm1]
  pb1 <- utils::txtProgressBar(min = 0, max = n1, style = 3)
  nr <- rep(NA, n1)
  for (cnt in 1:n1) {
    lineupk <- ktuples1[cnt, ]
    filt1 <- (dts$player==lineupk[1]) & (dts$assist==lineupk[2])
    nr[cnt] <- sum(filt1)
    utils::setTxtProgressBar(pb1, cnt)
  }
  close(pb1)
  dts_ktuples1 <- data.frame(player=ktuples1, nr=unlist(nr))

  # Couples of players: calculaing play time
  if (calc_min) {
    ktuples2 <- t(utils::combn(players_set,k))
    n2 <- nrow(ktuples2)
    vars_ply_nm2 <-  c(paste0("h",1:5), paste0("a",1:5))
    plyrs2 <- dts[, vars_ply_nm2]

    pb2 <- utils::txtProgressBar(min = 0, max = n2, style = 3)
    MIN <- rep(NA, n2)
    for (cnt in 1:n2) {
      lineupk <- ktuples2[cnt, ]
      filt2 <- apply(plyrs2, 1, function(x) {
        all(lineupk %in% x)
      })
      MIN[cnt] <- sum(dts$playlength[filt2])/60
      utils::setTxtProgressBar(pb2, cnt)
    }
    close(pb2)
    dts_ktuples2 <- data.frame(player=ktuples2, MIN=unlist(MIN))

    dts_ktuples <- dts_ktuples1 %>%
      dplyr::left_join(dts_ktuples2, by=c("player.1"="player.1", "player.2"="player.2")) %>%
      dplyr::left_join(dts_ktuples2, by=c("player.1"="player.2", "player.2"="player.1")) %>%
      dplyr::mutate(MIN=ifelse(is.na(MIN.x), MIN.y, MIN.x)) %>%
      dplyr::select(-MIN.x, -MIN.y) %>%
      dplyr::rename("player1"="player.1", "player2"="player.2")
  } else {
    dts_ktuples <- dts_ktuples1 %>%
      dplyr::rename("player1"="player.1", "player2"="player.2")
  }
  set_plyrs <- sapply(dts_ktuples[, paste0("player",1:2)], function(x) gsub("(\\b[A-Z])[^A-Z]+", "\\1", x))
  dts_ktuples$label <- apply(set_plyrs, 1, function(x) paste(x,collapse="+"))

  return(dts_ktuples)
}
