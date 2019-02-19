#' Create a network plot
#'
#' @param x An object of class 'networkdata'
#' @param layout Network vertex layout algorithms
#' @param layout.par Paramater for network vertex layout algorithms
#' @param edge.thr Threshold for edge values; values below the threshold are set to 0
#' @param edge.col.lim A numeric vector of length two providing limits of the scale for edge color
#' @param edge.col.lab Label for edge color legend
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
#' out <- assistnet(PbP.GSW)
#' plot(out, layout="circle", edge.thr=30, node.col="FGM_ASTp", node.size="ASTPTS")
#' @method plot assistnet
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

plot.assistnet <- function(x, layout="kamadakawai", layout.par=list(),
                           edge.thr=0, edge.col.lim=NULL, edge.col.lab=NULL,
                           node.size=NULL, node.size.lab=NULL,
                           node.col=NULL, node.col.lim=NULL, node.col.lab=NULL,
                           node.pal=colorRampPalette(c("white","blue", "red")),
                           edge.pal=colorRampPalette(c("white","blue", "red")), ...) {

  if (!is.assistnet(x)) {
    stop("Not a 'assistnet' object")
  }
  y <- xend <- yend <- N <- player <- vertex.names <- NULL
  net <- x[["assistNet"]]

  if (!is.null(node.size) & !is.null(node.col)) { ####
    if (is.null(node.col.lab)) {
      node.col.lab <- paste0("Node color:\n", node.col)
    }
    if (is.null(node.size.lab)) {
      node.size.lab <- paste0("Node size:\n", node.size)
    }
    nodeData <- x[["nodeStats"]] %>%
      dplyr::rename(node.size=!!node.size, node.col=!!node.col) %>%
      dplyr::select(player, node.size, node.col)
    tbl <- x[["assistTable"]]
    plyrs1 <- dimnames(tbl)[[1]]
    plyrs2 <- as.character(nodeData$player)
    idx <- match(plyrs1, plyrs2)
    if (any(plyrs1!=plyrs2[idx])) {
      stop("Players in 'data' and 'node.data' are not exactly the same.")
    }
    nodeData <- nodeData[idx,]
    network::set.vertex.attribute(net, "node.size", nodeData$node.size)
    network::set.vertex.attribute(net, "node.col", nodeData$node.col)
    if (is.null(node.col.lim)) {
      node.col.lim <- range(nodeData$node.col)
    }
  }

  if (is.null(edge.col.lim)) {
    edge.col.lim <- range(tbl)
  }
  if (is.null(edge.col.lab)) {
    edge.col.lab <- "Edge color:\nnumber of assists"
  }

  datanet <- ggnetwork::ggnetwork(net, layout=layout, layout.par=layout.par) %>%
    dplyr::mutate(N = replace(N, N <= edge.thr, NA))
  p <- ggplot(datanet, aes(x = x, y = y, xend = xend, yend = yend)) +
    ggnetwork::geom_edges(aes(color=N, alpha=N), size=1.5, curvature=0.1,
                          arrow=arrow(length=unit(6, "pt"), type="closed")) +
    scale_colour_gradientn(edge.col.lab, limits=edge.col.lim, colors=edge.pal(100),
                           na.value="transparent")

  if (!is.null(node.size) & !is.null(node.col)) { ####
    p <- p +
      ggnetwork::geom_nodes(aes(size=node.size, fill=node.col), shape=21, color="gray") +
      scale_size_continuous(node.size.lab, breaks=pretty(nodeData$node.size,n=7)) +
      scale_fill_gradientn(node.col.lab, limits=node.col.lim, colors=node.pal(100)) +
      guides(fill=guide_colorbar(order=1), size=guide_legend(order=3))
  } else {
    p <- p +
      ggnetwork::geom_nodes(size=5, shape=21, alpha=0.5, color="gray", fill="gray")
  }

  p <- p +
    ggnetwork::geom_nodetext_repel(aes(label=vertex.names)) +
    scale_alpha(guide=FALSE) +
    ggnetwork::theme_blank()

  return(p)
}
