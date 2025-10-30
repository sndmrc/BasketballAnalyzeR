#' Add lines of NBA court to an existing ggplot2 plot
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param p a ggplot2 object.
#' @param size numeric, line size.
#' @param col line color.
#' @param full logical; if TRUE draws a complete NBA court; if FALSE draws a half court.
#' @return A ggplot2 object
#' @examples
#' library(ggplot2)
#' p <- ggplot(data.frame(x=0, y=0), aes(x,y)) + coord_fixed()
#' drawNBAcourt(p)
#' @export

drawNBAcourt <- function(p, size=1.5, col="black", full=FALSE) {

  crcl <- function(x0, y0, r, span=r, nsteps=100) {
    x <- seq(x0-span,x0+span,length.out=nsteps)
    ylo <- y0-sqrt(r^2-(x-x0)^2)
    yup <- y0+sqrt(r^2-(x-x0)^2)
    data.frame(x=x, ylo=ylo, yup=yup)
  }

  x <- y <- ylo <- yup  <- NULL
  outbox <- data.frame(x=c(-25,-25,25,25,-25),
                       y=c(-47,0,0,-47,-47))
  FT <- crcl(0,-28,6)
  halfcourt <- crcl(0,0,6)
  key <- data.frame(x=-c(-8,-8,8,8,-8),y=-c(47,28,28,47,47))
  keyins <- data.frame(x=c(-6,-6,6,6,-6),y=-c(47,28,28,47,47))
  restr <- crcl(x0=0, y0=-41.25, r=4, nsteps=200)
  rim <- crcl(x0=0, y0=-41.75, r=0.75)
  backboard <- data.frame(x=c(-3,3), y=-c(43,43))
  crcl3pt <- crcl(x0=0, y0=-41.75, r=23.75, span=22)
  ln3pt <- data.frame(x=c(-22,-22,crcl3pt$x,22,22),
                      ylo=c(-47,-47+169/12,crcl3pt$ylo,-47+169/12,-47),
                      yup=c(-47,-47+169/12,crcl3pt$yup,-47+169/12,-47))

  p <- p +
    ###outside box:
    geom_path(data=outbox, aes(x, y), linewidth=size, color=col) +
    ###halfcourt semicircle:
    geom_path(data=halfcourt, aes(x=x, y=ylo), linewidth=size, color=col) +
    ###solid FT semicircle above FT line:
    geom_path(data=FT, aes(x=x, y=yup), linewidth=size, color=col) +
    ###dashed FT semicircle below FT line:
    geom_path(data=FT, aes(x=x, y=ylo), linetype='dashed', linewidth=size, color=col) +
    ###key:
    geom_path(data=key, aes(x, y), linewidth=size, color=col) +
    ###box inside the key:
    geom_path(data=keyins, aes(x, y), linewidth=size, color=col) +
    ###restricted area semicircle:
    geom_path(data=restr, aes(x=x, y=yup), linewidth=size, color=col) +
    ###rim:
    geom_path(data=rim, aes(x=x, y=ylo), linewidth=size, color=col) +
    geom_path(data=rim, aes(x=x, y=yup), linewidth=size, color=col) +
    ###backboard:
    geom_path(data=backboard, aes(x, y), lineend='butt', linewidth=size, color=col) +
    ###three-point line:
    geom_path(data=ln3pt, aes(x=x, y=yup), linewidth=size, color=col)

  if (full) {
    p <- p +
      ###outside box:
      geom_path(data=outbox, aes(x,-y), linewidth=size, color=col) +
      ###halfcourt semicircle:
      geom_path(data=halfcourt, aes(x=x, y=-ylo), linewidth=size, color=col) +
      ###solid FT semicircle above FT line:
      geom_path(data=FT, aes(x=x, y=-yup), linewidth=size, color=col) +
      ###dashed FT semicircle below FT line:
      geom_path(data=FT, aes(x=x, y=-ylo), linetype='dashed', linewidth=size, color=col) +
      ###key:
      geom_path(data=key, aes(x, -y), linewidth=size, color=col) +
      ###box inside the key:
      geom_path(data=keyins, aes(x, -y), linewidth=size, color=col) +
      ###restricted area semicircle:
      geom_path(data=restr, aes(x=x, y=-yup), linewidth=size, color=col) +
      ###rim:
      geom_path(data=rim, aes(x=x, y=-ylo), linewidth=size, color=col) +
      geom_path(data=rim, aes(x=x, y=-yup), linewidth=size, color=col) +
      ###backboard:
      geom_path(data=backboard, aes(x, -y), lineend='butt', linewidth=size, color=col) +
      ###three-point line:
      geom_path(data=ln3pt, aes(x=x, y=-yup), linewidth=size, color=col)
  }
  return(p)
}
