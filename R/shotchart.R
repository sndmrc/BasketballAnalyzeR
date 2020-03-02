#' Plots different kinds of charts based on shot coordinates
#'
#' @param data A data frame whose rows are field shots and columns are half-court shot coordinates x and y, and optionally additional variables to be specified in \code{z} and/or \code{result} (see Details).
#' @param x character, indicating the variable name of the x coordinate.
#' @param y character, indicating the variable name of the y coordinate.
#' @param z character, indicating the name of the variable used to color the points (if \code{type=NULL}) or the sectors (if \code{type="sectors"}, in this case \code{z} must be a numeric variable).
#' @param z.fun function (active when \code{type="sectors"}), used to summarize the values of \code{z} variable within each sector (recommended: \code{mean}, \code{median}).
#' @param result character (active when \code{type="sectors"} and \code{scatter=FALSE}), indicating the name of the factor with the shot result (allowed categories \code{made} and \code{missed}).
#' @param type character, indicating the plot type; available option are \code{NULL}, \code{"sectors"}, \code{"density-polygon"}, \code{"density-raster"}, \code{"density-hexbin"}.
#' @param scatter logical, if TRUE a scatter plot of the shots is added to the plot.
#' @param num.sect integer (active when \code{type="sectors"}), number of sectors.
#' @param n integer (active when \code{type="sectors"}), number of points used to draw arcs (must be > 500).
#' @param col.limits numeric vector, (active  when \code{z} is a numeric variable), limits \code{c(min, max)} for the gradient color scale of \code{z} variable.
#' @param courtline.col color of court lines.
#' @param sectline.col color of sector lines (active when \code{type="sectors"}).
#' @param text.col color of text annotation within sectors (active when \code{type="sectors"}).
#' @param pt.col color of points in the scatter plot.
#' @param bg.col background color.
#' @param legend logical, if TRUE a legend for \code{z} is plotted.
#' @param drop.levels logical, if TRUE unused levels of the \code{z} variable are dropped.
#' @param palette color palette; available options \code{"main"}, \code{"cool"}, \code{"hot"}, \code{"mixed"}, \code{"grey"}, \code{"bwr"} (blue, white, red).
#' @param pt.alpha numeric, transparency of points in the scatter plot.
#' @param nbins integer (active when \code{type="density-hexbin"}), number of bins.
#' @return A ggplot2 object.
#' @details The \code{data} dataframe could also be a play-by-play dataset provided that rows corresponding to events different from field shots have missing \code{x} and \code{y} coordinates.
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketball.analyzer.help@gmail.com})
#' @seealso \code{\link{drawNBAcourt}}, \code{\link[ggplot2]{geom_density_2d}}, \code{\link[ggplot2]{geom_hex}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' subdata <- subset(PbP, player=="Kevin Durant")
#' subdata$xx <- subdata$original_x/10
#' subdata$yy <- subdata$original_y/10-41.75
#' shotchart(data=subdata, x="xx", y="yy", scatter=TRUE)
#' shotchart(data=subdata, x="xx", y="yy", scatter=TRUE, z="result")
#' shotchart(data=subdata, x="xx", y="yy", scatter=TRUE, z="result",
#'           bg.col="black", courtline.col="white", palette="hot")
#' shotchart(data=subdata, x="xx", y="yy", result="result",
#'           type="sectors", sectline.col="gray", text.col="red")
#' shotchart(data=subdata, x="xx", y="yy", z="playlength", result="result",
#'           type="sectors",  num.sect=5)
#' shotchart(data=subdata, x="xx", y="yy", type="density-polygons", palette="bwr")
#' shotchart(data=subdata, x="xx", y="yy", type="density-raster",
#'           scatter=TRUE, pt.col="tomato", pt.alpha=0.1)
#' shotchart(data=subdata, x="xx", y="yy", type="density-hexbin", nbins=30)
#' @export
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_color_gradientn
#' @importFrom ggplot2 scale_fill_distiller
#' @importFrom ggplot2 stat_density_2d
#' @importFrom ggplot2 geom_hex
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 ggplot_build
#' @importFrom PBSmapping as.PolySet
#' @importFrom PBSmapping calcCentroid
#' @importFrom sp point.in.polygon
#' @importFrom stats median

shotchart <- function(data, x, y, z=NULL, z.fun=median,  result=NULL,
     type=NULL, scatter=FALSE, num.sect=7, n=1000, col.limits=c(NA,NA),
     courtline.col = "black", bg.col="white", sectline.col="white", text.col="white",
     legend=FALSE, drop.levels=TRUE,
     pt.col="black", pt.alpha=0.5,  nbins=25, palette="mixed") {

  if (num.sect<4) {
    stop("The number of sectors 'num.sect' must be >=4")
  }
  if (n<500) {
    stop("The number of points 'n' must be >=500")
  }

  fancy_scientific <- function(l) {
    l <- format(l, digits=3, scientific = TRUE)
    l <- gsub("^(.*)e", "'\\1'e", l)
    l <- gsub("e", "%*%10^", l)
    parse(text=l)
  }

  X <- Y <- angle <- nsegm <- sector <- ..density.. <- ..level.. <- NULL
  pal <- BbA_pal(palette=palette)

  df1 <- data.frame(x=data[,x], y=data[,y], z=data[,z], result=data[,result])
  filt.na <- !apply(df1, 1, function(x) any(is.na(x)))
  df1 <- subset(df1, filt.na & y<=0)

  list_sects <- generateSectors(num.sect, npts=n)
  sects <- list_sects[[1]]

  if (is.null(type) & !scatter) { ##################
    p <- ggplot(data=data.frame(x=0,y=0), aes(x,y))
    p <- drawNBAcourt(p, full=FALSE, size=0.75, col=courtline.col) +
      coord_fixed() + themeBbA(plot.bgcolor=bg.col, legend.bgcolor=bg.col)

  } else if (is.null(type) & scatter) { ##################
    p <- ggplot(data=data.frame(x=0,y=0), aes(x,y))
    p <- drawNBAcourt(p, full=FALSE, size=0.75, col=courtline.col)
    if (is.null(z)) {
      p <- p + geom_point(data=df1, aes(x=x, y=y), fill=pt.col, color=pt.col, alpha=pt.alpha,
                          shape=21, size=3, inherit.aes=FALSE)
    } else {
      p <- p + geom_point(data=df1, aes(x=x, y=y, fill=z, color=z), alpha=pt.alpha,
                          shape=21, size=3, inherit.aes=FALSE)
      zvar <- df1$z
      if (is.factor(zvar)) {
        if (drop.levels) {
          ncols <- length(unique(droplevels(zvar)))
          cols <- rev(pal(ncols))
          p <- p +
            scale_fill_manual(name=z, values=cols, drop=TRUE) +
            scale_color_manual(name=z, values=cols, drop=TRUE)
        } else {
          ncols <- length(table(zvar))
          cols <- rev(pal(ncols))
          p <- p +
            scale_fill_manual(name=z, values=cols, drop=FALSE) +
            scale_color_manual(name=z, values=cols, drop=FALSE)
        }
      } else {
        p <- p +
          scale_fill_gradientn(name=z, colours = pal(256), limits=col.limits) +
          scale_color_gradientn(name=z, colours = pal(256), limits=col.limits)
      }
    }
    p <- p + coord_fixed() + themeBbA(plot.bgcolor=bg.col, legend.bgcolor=bg.col)

  } else if (type=="sectors") { ##################

    stats_by_sect <- sapply(sort(unique(sects$sector)), function(k) {
      sectk <- subset(sects, sector==k)
      filtk <- sp::point.in.polygon(point.x=df1$x, point.y=df1$y, pol.x=sectk$x, pol.y=sectk$y)==1
      mnk <- if (!is.null(z)) z.fun(df1$z[filtk], na.rm=T) else NA
      totk <- sum(filtk)
      madek <- sum(df1$result[filtk]=="made")
      pctk <- round(100*madek/totk)
      c(mnk,madek,totk,pctk)
    })
    sects$z <- stats_by_sect[1,][sects$sector+1]

    sects$pos <- unlist(sapply(unique(sects$sector), function(k) {
      x <- subset(sects, sector==k)
      return(1:nrow(x))
    }))
    s <- PBSmapping::as.PolySet(data.frame(X=sects$x, Y=sects$y, POS=sects$pos, PID=sects$sector))
    centroids <- data.frame(PBSmapping::calcCentroid(s),
                            text=paste0(stats_by_sect[4,],"%\n (",stats_by_sect[2,],"/",stats_by_sect[3,],")"))

    centroids$angle <- rep(0, nrow(centroids))
    centroids$angle[4] <- 90
    centroids$angle[nrow(centroids)] <- -90
    p <- ggplot(data=data.frame(x=0,y=0), aes(x,y))
    if (!is.null(z)) {
      p <- p + geom_polygon(data=sects, aes(x=x, y=y, group=sector, fill=z)) +
        scale_fill_gradientn(name=z, colours = pal(256), limits=col.limits)
    } else {
      if (bg.col==text.col | bg.col==sectline.col) warning("Using this color setting, sector lines and/or text annotations are not visible. Set different bg.col, sectline.col, text.col.")
    }
    p <- drawNBAcourt(p, full=FALSE, size=1, col=courtline.col) +
      themeBbA(plot.bgcolor=bg.col, legend.bgcolor=bg.col)

    if (scatter) {
      p <- p + geom_point(data=df1, aes(x=x, y=y), color=pt.col, alpha=pt.alpha,
                          shape=21, size=3, fill=pt.col, inherit.aes=FALSE)
    } else if (!scatter & !is.null(result)) {
      p <- p + geom_text(data=centroids, aes(x=X,y=Y, label=text, angle=angle), col=text.col)
    }
    p <- p +
      geom_line(data=list_sects[[2]], aes(x=x, y=y, group=nsegm), size=0.8,
                color=sectline.col, alpha=0.75, inherit.aes=FALSE)+
      geom_line(data=list_sects[[3]], aes(x=x, y=y), size=0.8,
                color=sectline.col, alpha=0.75, inherit.aes=FALSE) +
      coord_fixed() + themeBbA(plot.bgcolor=bg.col, legend.bgcolor=bg.col)

  } else if (type=="density-polygons") { ##################
    p <- ggplot(data=df1, aes(x=x, y=y)) +
      stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white") +
      scale_fill_gradientn(name="Density\n(log)", colours = pal(256), trans='log', labels=fancy_scientific)
    if (scatter) {
      p <- p + geom_point(data=df1, aes(x=x, y=y), fill=pt.col,
                          color=pt.col, alpha=pt.alpha, shape=21, size=3, inherit.aes=FALSE)
    }
    p <- drawNBAcourt(p, full=FALSE, size=1, col=courtline.col)
    p <- p + themeBbA(plot.bgcolor=bg.col, legend.bgcolor=bg.col)
    if (!legend) {
      p <- p + theme(legend.position = 'none')
    }
    plot_xrange <- ggplot_build(p)$layout$panel_params[[1]]$x.range
    plot_yrange <- ggplot_build(p)$layout$panel_params[[1]]$y.range
    p <- p + scale_x_continuous(limits = plot_xrange * 1.25) +
      scale_y_continuous(limits = plot_yrange * 1.25) +
      coord_fixed(xlim=plot_xrange, ylim=plot_yrange)

  } else if (type=="density-raster") { ##################
    p <- ggplot(data=df1, aes(x=x, y=y)) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = F) +
      scale_fill_distiller(palette="Spectral", direction=-1, labels=fancy_scientific)
    if (scatter) {
      p <- p + geom_point(data=df1, aes(x=x, y=y), fill=pt.col, color=pt.col,
                          alpha=pt.alpha, shape=21, size=3, inherit.aes=FALSE)
    }
    p <- drawNBAcourt(p, full=FALSE, size=1, col=courtline.col)
    p <- p +  coord_fixed()+ themeBbA(plot.bgcolor=bg.col, legend.bgcolor=bg.col)
    if (!legend) {
      p <- p + theme(legend.position = 'none')
    }

  } else if (type=="density-hexbin") { ##################
    p <- ggplot(data=df1, aes(x=x, y=y)) +
      geom_hex(bins=nbins) +
      scale_fill_gradientn(name="Density\n(log)", colours = pal(256), trans='log', labels=fancy_scientific)
    if (scatter) {
      p <- p + geom_point(data=df1, aes(x=x, y=y), fill=pt.col, color=pt.col,
                          alpha=pt.alpha, shape=21, size=3, inherit.aes=FALSE)
    }
    p <- drawNBAcourt(p, full=FALSE, size=1, col=courtline.col)
    p <- p + coord_fixed() + themeBbA(plot.bgcolor=bg.col, legend.bgcolor=bg.col)
    if (!legend) {
      p <- p + theme(legend.position = 'none')
    }

  } else { ##############
    stop("Please, select a valid plot type and/or a scatter plot")
  }

  return(p)
}


