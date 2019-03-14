#' Plot variability
#'
#' @param x A 'variability' objects
#' @param title Plot title
#' @param ylim Set y-axis limits
#' @param ylab Set y-axis legend
#' @param size.lim Set limits of 'size' legend
#' @param max.circle  Maximum size of the 'size' plotting symbol (circles)
#' @param n.circle  Set a sequence of about n.circle+1 equally spaced 'round' values which cover the range of the values in 'size'
#' @param leg.brk Breaks for 'size' legend
#' @param leg.pos The position of legend ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param leg.just Anchor point for positioning legend inside plot ("center" or two-element numeric vector) or the justification according to the plot area when positioned outside the plot
#' @param leg.nrow The desired number of rows of 'size' legend
#' @param leg.title Title of 'size' legend
#' @param leg.title.pos A character string indicating the position of a title. One of "top" (default for a vertical legend), "bottom", "left" (default for a horizontal legend), or "right."
#' @param ... other graphical parameters
#' @return A ggplot2 object
#' @examples
#' Pbox.BC <- subset(Pbox, Team=="Oklahoma City Thunder" & MIN >= 500,
#'                     select=c("P2p","P3p","FTp","P2A","P3A","FTA"))
#' out <- variability(data=Pbox.BC, data.var=c("P2p","P3p","FTp"),
#'                    size.var=c("P2A","P3A","FTA"), weight=TRUE)
#' plot(out, leg.brk=c(10,25,50,100,500,1000), max.circle=30)
#' @method plot variability
#' @export

plot.variability <- function(x, title="Variability diagram", ylim=NULL, ylab=NULL,
                             size.lim=NULL, max.circle=25, n.circle=4,
                             leg.brk=NULL, leg.pos="right", leg.just="left",
                             leg.nrow=NULL, leg.title=NULL, leg.title.pos="top", ...) {

  if (!is.variability(x)) {
    stop("Not an object of class 'variability'")
  }
  id <- V1 <- V2 <- NULL
  df.data <- x[["data"]]
  df.size <- x[["size"]]

  weight <- x[["weight"]]
  rg <- x[["range"]]

  if (!is.null(x$VC)) {
    vc <- x[["VC"]]
    VC <- TRUE
  } else {
    VC <- FALSE
  }
  nc.data <- ncol(df.data)
  nc.size <- ncol(df.size)
  nr <- nrow(df.data)
  if (nc.data == nc.size) {
    df3 <- data.frame(id = rep(1:nc.data, each = nr),
                      V1 = utils::stack(df.data)$values,
                      V2 = utils::stack(df.size)$values)
  } else if (nc.size == 1) {
    df3 <- data.frame(id = rep(1:nc.data, each = nr),
                      V1 = utils::stack(df.data)$values,
                      V2 = rep(df.size, nc.data))
  }
  names(df3) <- c("id", "V1", "V2")
  if (is.null(ylim)) {
    ylim <- range(df3$V1)
  }
  if (is.null(size.lim)) {
    size.lim <- range(df3$V2)
  }
  if (is.null(leg.brk)) {
    leg.brk <- pretty(df3$V2, n=n.circle)
  }
  if (is.null(leg.title)) {
    leg.title <- paste(names(df.size), collapse="\n")
  }

  p <- ggplot(df3, aes(x=id, y=V1)) +
    geom_point(aes(size=V2), shape = 21, colour = "dodgerblue") +
    scale_x_continuous(name = "", limits = c(0, nc.data + 1),
                       breaks = 0:(nc.data + 1),
                       labels = c("", names(df.data), "")) +
    annotate("text", x = 0:nc.data, y = rep(9*ylim[2]/8, nc.data+1),
             label = c(" Range: ", round(rg, 2)), size = 4)

  if (VC) {
    p <- p + annotate("text", x = 0:nc.data, y = rep(10*ylim[2]/8, nc.data+1),
                      label = c("    VC: ", round(vc, 2)), size = 4)
  }

  p <- p +
    scale_size_continuous(breaks=sort(leg.brk, decreasing=FALSE), limits=size.lim, range=c(1,max.circle)) +
    theme(legend.position=leg.pos, legend.justification=leg.just,
          #panel.background=element_rect(fill="transparent", colour="gray50"),
          #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
          legend.key = element_rect(fill="transparent", colour="transparent")) +
    labs(title=title, y=ylab) +
    guides(size=guide_legend(title=leg.title, nrow=leg.nrow, title.position=leg.title.pos))
  return(p)
}
