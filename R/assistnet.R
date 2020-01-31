#' Investigates the network of assists-shots in a team
#'
#' @param data A data frame whose rows are field shots and columns are variables to be specified in \code{assist}, \code{player}, \code{points}, \code{event.type} (see Details).
#' @param assist character, indicating the name of the variable with players who made the assists.
#' @param player character, indicating the name of the variable with players who made the shot.
#' @param points character, indicating the name of the variable with points.
#' @param event.type character, indicating the name of the variable with type of event (mandatory categories are \code{"miss"} for missed field shots and \code{"shot"} for field goals).
#' @return A list with 3 elements, \code{assistTable} (a table), \code{nodeStats} (a dataframe), and a \code{assistNet} (a network object). See Details.
#' @details The \code{data} dataframe could also be a play-by-play dataset provided that rows corresponding to events different from field shots are not coded as \code{"shot"} in the \code{event.type} variable.
#' @details \code{assistTable} is the cross-table of assists made and received by the players.
#' @details \code{nodeStats} is a dataframe with the variables
#' @details   - \code{FGM} (fields goals made),
#' @details   - \code{FGM_AST} (field goals made thanks to a teammate's assist),
#' @details   - \code{FGM_ASTp} (percentage of \code{FGM_AST} over \code{FGM}),
#' @details   - \code{FGPTS} (points scored with field goals),
#' @details   - \code{FGPTS_AST} (points scored thanks to a teammate's assist),
#' @details   - \code{FGPTS_ASTp} (percentage of \code{FGPTS_AST} over \code{FGPTS}),
#' @details   - \code{AST} (assists made),
#' @details   - \code{ASTPTS} (point scored by assist's teammates).
#' @details \code{assistNet} is a object of class \code{network} that can be used for further network analysis with specific R packages.
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP.GSW <- subset(PbP, team=="GSW" & player!="")
#' out <- assistnet(PbP.GSW)
#' plot(out, layout="circle", edge.thr=30, node.col="FGM_ASTp", node.size="ASTPTS")
#' @export
#' @importFrom network set.vertex.attribute
#' @importFrom tidyr replace_na

assistnet <- function(data, assist="assist", player="player", points="points", event.type="event_type") {

  FGM <- FGM_AST <- FGM_ASTp <- FGPTS <- FGPTS_AST <- FGPTS_ASTp <- NULL
  data <- droplev_by_col(data)
  data <- data %>%
    dplyr::rename(assist=!!assist, player=!!player, points=!!points, event.type=!!event.type)
  data_no_assist <- data %>%
    dplyr::filter(assist!="")
  data_no_assist <- droplev_by_col(data_no_assist)
  assist_player <- data_no_assist %>%
    dplyr::select(assist, player)
  all_ast_plr <- sort(unique(unlist(assist_player)))
  assist_player$assist <- factor(assist_player$assist, levels=all_ast_plr)
  assist_player$player <- factor(assist_player$player, levels=all_ast_plr)

  tbl <- as.matrix(table(assist_player, useNA="no"))
  if (nrow(tbl)!=ncol(tbl)) {
    stop("The number of players in 'assist' and 'player' variables are not the same.")
  }

  # Calculate some player/node statistics
  nodeData1 <- data %>%
    dplyr::group_by(player) %>%
    dplyr::filter(event.type=="shot") %>%
    dplyr::summarise(FGM=dplyr::n(),
                     FGM_AST=sum(assist!=""),
                     FGM_ASTp=100*FGM_AST/FGM,
                     FGPTS=sum(points),
                     FGPTS_AST=sum(points*(assist!="")),
                     FGPTS_ASTp=FGPTS_AST/FGPTS) %>%
    as.data.frame()
  nodeData2 <- data %>%
    dplyr::filter(assist!="") %>%
    dplyr::group_by(assist) %>%
    dplyr::summarise(AST=dplyr::n(), ASTPTS=sum(points)) %>%
    as.data.frame()
  nodeData <- merge(nodeData1, nodeData2, by.x="player", by.y="assist", all=T)

  net <- network::network(tbl, matrix.type="adjacency", directed=TRUE,
                          ignore.eval=FALSE,  names.eval="N")

  out <- list(assistTable=tbl, nodeStats=nodeData, assistNet=net)
  class(out) <- append("assistnet", class(out))
  return(out)
}
