#' Calculate table of assists and some useful player statistics
#'
#' @param data A dataframe
#' @param assist assist
#' @param player player
#' @return A list with two elements: assistTable (a table) and stats (a dataframe)
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP.GSW <- subset(PbP, team=="GSW" & player!="")
#' out <- netcalc(PbP.GSW)
#' plot(out, layout="circle", edge.thr=30)
#' @export
#' @importFrom network set.vertex.attribute
#' @importFrom tidyr replace_na

netcalc <- function(data, assist="assist", player="player") {

  event_type <- N2P3P_made_with_assist <- N2P3P_made <- pctN2P3P_made_with_assist <- pctN2P3P_made_with_assist <- NULL
  data <- droplev_by_col(data)
  data <- data %>%
          dplyr::rename(assist=!!assist, player=!!player)
  data_no_assist <- data %>%
                    dplyr::filter(assist!="")
  data_no_assist <- droplev_by_col(data_no_assist)
  assist_player <- data_no_assist %>%
                   dplyr::select(assist, player)

  tbl <- as.matrix(table(assist_player, useNA="no"))
  if (nrow(tbl)!=ncol(tbl)) {
    stop("The number of players in 'assist' and 'player' variables are not the same.")
  }

  # Calculate some player statistics
  nodes1 <- data %>%
    dplyr::group_by(player) %>%
    dplyr::summarise(N2P3P_made=sum(event_type=="shot"),
                     N2P3P_made_with_assist=sum(event_type=="shot" & assist!=""),
                     pctN2P3P_made_with_assist=100*N2P3P_made_with_assist/N2P3P_made) %>%
    as.data.frame()
  nodes2 <- data %>%
    dplyr::filter(assist!="") %>%
    dplyr::group_by(assist) %>%
    dplyr::summarise(PTS_assisted_shots=sum(points)) %>%
    as.data.frame()
  nodes <- merge(nodes1, nodes2, by.x="player", by.y="assist", all=T) %>%
    tidyr::replace_na(list(pctN2P3P_made_with_assist=0, PTS_assisted_shots=0))

  out <- list(assistTable=tbl, stats=nodes)
  class(out) <- append("networkdata", class(out))
  return(out)
}

