#' Create a network plot
#'
#' @param x An object of class 'networkdata'
#' @param layout Network vertex layout algorithms
#' @param layout.par Paramater for network vertex layout algorithms
#' @param edge.thr Threshold for edge values; values below the threshold are set to 0
#' @param edge.col.lim A numeric vector of length two providing limits of the scale for edge color
#' @param edge.col.lab Label for edge color legend
#' @param node.data Data with variables for node size and node color
#' @param node.size A string with the variable name for node size
#' @param node.size.lab Label for node size legend
#' @param node.col A string with the variable name for node color
#' @param node.col.lim A numeric vector of length two providing limits of the scale for node color
#' @param node.col.lab Label for node color legend
#' @param node.pal Palette for node colors
#' @param edge.pal Palette for edge colors
#' @param ... other graphical parameters
#' @return A ggplot2 object
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP.GSW <- subset(PbP, team=="GSW" & player!="")
#' out <- netcalc(PbP.GSW)
#' plot(out, layout="circle", edge.thr=30)
#' @method plot networkdata
#' @export
#' @importFrom ggnetwork ggnetwork
#' @importFrom ggnetwork geom_nodes
#' @importFrom ggnetwork geom_nodetext_repel
#' @importFrom ggnetwork geom_edges
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 arrow
#' @importFrom ggplot2 scale_alpha
#' @importFrom ggplot2 scale_size_continuous
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 guide_colorbar

plot.networkdata <- function(x, layout="kamadakawai", layout.par=list(),
                             edge.thr=0, edge.col.lim=NULL, edge.col.lab=NULL,
                             node.data=NULL, node.size=NULL, node.size.lab=NULL,
                             node.col=NULL, node.col.lim=NULL, node.col.lab=NULL,
                             node.pal=colorRampPalette(c("white","blue", "red")),
                             edge.pal=colorRampPalette(c("white","blue", "red")), ...) {

  if (!is.networkdata(x)) {
    stop("Not a 'networkdata' object")
  }

  y <- xend <- yend <- N <- player <- vertex.names <- NULL

  tbl <- x[["assistTable"]]
  #tbl[tbl < edge.thr] <- 0
  net <- network::network(tbl, matrix.type="adjacency", directed=TRUE,
                          ignore.eval=FALSE,  names.eval="N")

  if (is.null(node.size) & is.null(node.col)) {
    if (is.null(node.col.lab)) {
      node.col.lab <- "Node color:\n% assisted shots\nmade by the player"
    }
    if (is.null(node.size.lab)) {
      node.size.lab <- "Node size:\nPTS assisted by\nthe player"
    }
    nodes <- x[["stats"]] %>%
      dplyr::rename(node.size=!!"PTS_assisted_shots", node.col=!!"pctN2P3P_made_with_assist")

  } else if (!is.null(node.size) & !is.null(node.col)) {
    if (is.null(node.col.lab)) {
      node.col.lab <- paste0("Node color:\n",node.col)
    }
    if (is.null(node.size.lab)) {
      node.size.lab <- paste0("Node size:\n",node.size)
    }
    nodes <- node.data %>%
      dplyr::select(player, node.size, node.col) %>%
      dplyr::rename(node.size=!!node.size, node.col=!!node.col)
  }

  plyrs1 <- dimnames(tbl)[[1]]
  plyrs2 <- as.character(nodes$player)
  idx <- match(plyrs1, plyrs2)
  if (any(plyrs1!=plyrs2[idx])) {
    stop("Players in 'data' and 'node.data' are not exactly the same.")
  }
  nodes <- nodes[idx,]

  network::set.vertex.attribute(net, "node.size", nodes$node.size)
  network::set.vertex.attribute(net, "node.col", nodes$node.col)
  if (is.null(node.col.lim)) {
    node.col.lim <- range(nodes$node.col)
  }
  if (is.null(edge.col.lim)) {
    edge.col.lim <- range(tbl)
  }
  if (is.null(edge.col.lab)) {
    edge.col.lab <- "Edge color:\nnumber of assists"
  }

  datanet <- ggnetwork::ggnetwork(net, layout=layout, layout.par=layout.par) %>%
    dplyr::mutate(N = replace(N, N < edge.thr, NA))
  p <- ggplot(datanet, aes(x = x, y = y, xend = xend, yend = yend)) +
    ggnetwork::geom_nodes(aes(size=node.size, fill=node.col), shape=21, color="gray") +
    ggnetwork::geom_edges(aes(color=N, alpha=N), size=1.5, curvature=0.1, arrow=arrow(length=unit(6, "pt"), type="closed")) +
    ggnetwork::geom_nodetext_repel(aes(label=vertex.names)) +
    scale_size_continuous(node.size.lab, breaks=pretty(nodes$node.size,n=7)) +
    scale_fill_gradientn(node.col.lab, limits=node.col.lim, colors=node.pal(100)) +
    scale_colour_gradientn(edge.col.lab, limits=edge.col.lim, colors=edge.pal(100), na.value="transparent") +
    scale_alpha(guide=FALSE) +
    ggnetwork::theme_blank() +
    guides(fill=guide_colorbar(order=1), size=guide_legend(order=3))

  return(p)
}
