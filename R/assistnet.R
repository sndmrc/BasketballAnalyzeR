#' Calculate table of assists and some useful player statistics
#'
#' @param data A dataframe
#' @param assist assist
#' @param player player
#' @return A list with 3 elements: assistTable (a table), nodeStats (a dataframe), and a net (a network object)
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP.GSW <- subset(PbP, team=="GSW" & player!="")
#' out <- assistnet(PbP.GSW)
#' plot(out, layout="circle", edge.thr=30, node.col="FGM_ASTp", node.size="ASTPTS")
#' @export
#' @importFrom network set.vertex.attribute
#' @importFrom tidyr replace_na

assistnet <- function(data, assist="assist", player="player") {

  points <- event_type <- FGM <- FGM_AST <- FGM_ASTp <- FGPTS <- FGPTS_AST <- FGPTS_ASTp <- NULL
  data <- droplev_by_col(data)
  data <- data %>%
    dplyr::rename(assist=!!assist, player=!!player)
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
    dplyr::filter(event_type=="shot") %>%
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

