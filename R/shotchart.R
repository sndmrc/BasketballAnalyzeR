#' Plot shortcharts
#'
#' @param data A data frame
#' @param x Variable name for the x coordinate
#' @param y Variable name for the y coordinate
#' @param z Name of 'data' variable to be used as a third variable
#' @param result Name of 'data' variable containing the type of shot; this variable is used to generate shot statistics; allowed categories: 'made' and 'missed'
#' @param type A string with the plot type: 'sectors', 'density-polygon', 'density-raster', 'density-hexbin'
#' @param scatter Logical. If TRUE a scatter plot was added to the plot
#' @param num.sect Number of sectors in shotchart plot split by sectors
#' @param n Number of point used when drawing arcs in shotchart plot split by sectors
#' @param courtline.col Color of court lines
#' @param pt.col Color of points in the scatter plot
#' @param bg.col Background color
#' @param palette Palette color for the scatter plot: 'main', 'cool', 'hot', 'mixed', 'grey'
#' @param pt.alpha Transparency of points in the scatter plot
#' @param nbins Number of bins in the 'density-hexbin' plot
#' @return A ggplot2 object
#'
#' @examples
#' data(PbP)
#' PbP <- PbPmanipulation(PbP.BDB)
#' subdata <- subset(PbP, player=="Kevin Durant")
#' subdata$xx <- subdata$original_x/10
#' subdata$yy <- subdata$original_y/10-42
#' # Shotchart with colored sectors and statistics for a 3rd variable
#' shotchart(data=subdata, x="xx", y="yy", z="playlength", result="result",
#'           type="sectors",  num.sect=7)
#' # Shotchart with scatter plot and a stratification variable
#' shotchart(data=subdata, x="xx", y="yy", z="result",
#'           type=NULL, scatter=TRUE, palette="main")
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
#' @importFrom PBSmapping as.PolySet
#' @importFrom PBSmapping calcCentroid
#' @importFrom sp point.in.polygon
#' @importFrom stats median

shotchart <- function(data, x=NULL, y=NULL, z=NULL, result=NULL,
     type=NULL, scatter=FALSE, num.sect=7, n=1000,
     courtline.col = "black", bg.col="white",
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
        ncols <- length(unique(droplevels(zvar)))
        cols <- rev(pal(ncols))
        p <- p +
          scale_fill_manual(name=z, values=cols) +
          scale_color_manual(name=z, values=cols)
      } else {
        p <- p +
          scale_fill_gradientn(name=z, colours = pal(256)) +
          scale_color_gradientn(name=z, colours = pal(256))
      }
    }
    p <- p + coord_fixed() + themeBbA(plot.bgcolor=bg.col, legend.bgcolor=bg.col)

  } else if (type=="sectors") { ##################
    stats_by_sect <- sapply(sort(unique(sects$sector)), function(k) {
      sectk <- subset(sects, sector==k)
      filtk <- sp::point.in.polygon(point.x=df1$x, point.y=df1$y, pol.x=sectk$x, pol.y=sectk$y)==1
      mnk <- median(df1$z[filtk], na.rm=T)
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
    p <- p + geom_polygon(data=sects, aes(x=x, y=y, group=sector, fill=z)) +
      scale_fill_gradientn(name=z, colours = pal(256))
    p <- drawNBAcourt(p, full=FALSE, size=1, col=courtline.col)

    if (scatter) {
      p <- p + geom_point(data=df1, aes(x=x, y=y), color=pt.col, alpha=pt.alpha,
                          shape=21, size=3, fill=pt.col, inherit.aes=FALSE)
    } else {
      p <- p + geom_text(data=centroids, aes(x=X,y=Y, label=text, angle=angle), col="white")
    }
    p <- p +
      geom_line(data=list_sects[[2]], aes(x=x, y=y, group=nsegm), size=0.8,
                color="white", alpha=0.75, inherit.aes=FALSE)+
      geom_line(data=list_sects[[3]], aes(x=x, y=y), size=0.8,
                color="white", alpha=0.75, inherit.aes=FALSE) +
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
    p <- p + coord_fixed() + themeBbA(plot.bgcolor=bg.col, legend.bgcolor=bg.col) +
             theme(legend.position = 'none')

  } else if (type=="density-raster") { ##################
    p <- ggplot(data=df1, aes(x=x, y=y)) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = F) +
      scale_fill_distiller(palette="Spectral", direction=-1, labels=fancy_scientific)
    if (scatter) {
      p <- p + geom_point(data=df1, aes(x=x, y=y), fill=pt.col, color=pt.col,
                          alpha=pt.alpha, shape=21, size=3, inherit.aes=FALSE)
    }
    p <- drawNBAcourt(p, full=FALSE, size=1, col=courtline.col)
    p <- p +  coord_fixed()+ themeBbA(plot.bgcolor=bg.col, legend.bgcolor=bg.col) +
              theme(legend.position = 'none')

  } else if (type=="density-hexbin") { ##################
    p <- ggplot(data=df1, aes(x=x, y=y)) +
      geom_hex(bins=nbins) +
      scale_fill_gradientn(name="Density\n(log)", colours = pal(256), trans='log', labels=fancy_scientific)
    if (scatter) {
      p <- p + geom_point(data=df1, aes(x=x, y=y), fill=pt.col, color=pt.col,
                          alpha=pt.alpha, shape=21, size=3, inherit.aes=FALSE)
    }
    p <- drawNBAcourt(p, full=FALSE, size=1, col=courtline.col)
    p <- p + coord_fixed() + themeBbA(plot.bgcolor=bg.col, legend.bgcolor=bg.col) +
      theme(legend.position = 'none')

  } else { ##############
    stop("Please, select a valid plot type and/or a scatter plot")
  }
  print(p)
  invisible(p)
}


