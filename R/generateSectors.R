#' @noRd

generateSectors <- function(nsect = 7, npts = 1000) {

  x <- NULL
  ang_start <- atan(106/(12 * 22))

  # Upper circle (three-point line)
  r1 <- 23.75; span1 <- 22
  crcl1 <- crcl(x0 = 0, y0 = -41.75, r = r1, nsteps = npts)
  # Middle circle (passing through the corner P=(8,-28) )
  r2 <- sqrt(8^2 + (41.75 - 28)^2); span2 <- r2 * cos(ang_start)
  crcl2 <- crcl(x0 = 0, y0 = -41.75, r = r2, nsteps = npts)
  # Lower circle (restricted area semicircle)
  r3 <- 4; span3 <- 4
  crcl3 <- crcl(x0 = 0, y0 = -41.25, r = r3, nsteps = npts)

  # Abscissa of the corners for A, B, C sectors
  ang_step <- (pi - 2 * ang_start)/(nsect - 2)
  angs <- ang_start + (0:(nsect - 2)) * ang_step
  xpts1 <- r1 * cos(angs)
  xpts2 <- r2 * cos(angs)
  a <- tan(angs)
  xpts3 <- ifelse(a > 0, (a + sqrt(63 + 64 * a^2))/(2 * (1 + a^2)),
                  (a - sqrt(63 + 64 * a^2))/(2 * (1 + a^2)))


  ### First three sectors at the bottom right of the court
  cnt <- 1
  sub_crcl4 <- subset(crcl3, x >= xpts3[1])
  sec0A <- data.frame(x = c(0, 0, 4, 4, rev(sub_crcl4$x), r2 * cos(ang_start), r2 * cos(ang_start)),
                      y = c(-47, -43, -43, -41.25, rev(sub_crcl4$yup), -41.75 + r2 * sin(ang_start), -47))
  sec0A$sector <- cnt
  cnt <- cnt + 1
  sec0B <- data.frame(x = c(r2 * cos(ang_start), r2 * cos(ang_start), 22, r1 * cos(ang_start)),
                      y = c(-47, -41.75 + r2 * sin(ang_start), -41.75 + r1 * sin(ang_start), -47))
  sec0B$sector <- cnt
  cnt <- cnt + 1
  sec0C <- data.frame(x = c(22, 22, 25, 25),
                      y = c(-47, -41.75 + 22 * tan(ang_start), -41.75 + 25 * tan(ang_start), -47))
  sec0C$sector <- cnt
  cnt <- cnt + 1
  sec0_left <- rbind(sec0A, sec0B, sec0C)

  # Coordinates for drawing sector boundaries (lines)
  segms <- matrix(0,nsect*2+2,3)
  nsegm <- 1
  segms[nsegm,] <- c(sub_crcl4$x[1], sub_crcl4$yup[1], nsegm)
  segms[nsegm+1,] <- c(sec0C$x[3], sec0C$y[3], nsegm)
  nsegm <- nsegm+2
  segms[nsegm,] <- c(0, -43, nsegm)
  segms[nsegm+1,] <- c(0, -47, nsegm)
  nsegm <- nsegm+2

  # Coordinates for drawing sector boundaries (arcs)
  sub_crcl2 <- subset(crcl2, x <= xpts2[1] & x >= -xpts2[1])
  arc <- data.frame(x=c(sub_crcl2$x[1], sub_crcl2$x, -sub_crcl2$x[1]),
                    y=c(-47, sub_crcl2$yup, -47))

  ##############################
  sectsABC_list <- vector(nsect - 2, mode = "list")

  for (k in 1:(nsect - 2)) {
    sub_crcl1 <- subset(crcl1, x <= xpts1[k] & x >= xpts1[k + 1])
    sub_crcl2 <- subset(crcl2, x <= xpts2[k] & x >= xpts2[k + 1])
    sub_crcl3 <- subset(crcl3, x <= xpts3[k] & x >= xpts3[k + 1])
    # Lower sector (A)
    secA <- rbind(sub_crcl3[, c(1, 3)], sub_crcl2[nrow(sub_crcl2):1, c(1, 3)])
    secA$sector <- cnt
    cnt <- cnt + 1
    # Middle sector (B)
    secB <- rbind(sub_crcl2[, c(1, 3)], sub_crcl1[nrow(sub_crcl1):1, c(1, 3)])
    secB$sector <- cnt
    cnt <- cnt + 1
    # Upper sector (C)
    tan_angs <- tan(angs)
    xs <- ifelse(tan_angs > 0, 25, -25)
    ys <- -41.75 + 25 * abs(tan_angs)
    xs <- ifelse(ys < 0, xs, 41.75/tan_angs)
    ys <- ifelse(ys < 0, ys, 0)
    pt1 <- data.frame(x = xs[k], yup = ys[k])
    pt2 <- data.frame(x = xs[k + 1], yup = ys[k + 1])
    if (xs[k] == xs[k + 1] | ys[k] == ys[k + 1]) {
      secC <- rbind(sub_crcl1[, c(1, 3)], pt1, pt2)
    } else {
      vertex <- c(sign(xs[k + 1]) * 25, 0)
      secC <- rbind(sub_crcl1[, c(1, 3)], pt1, vertex, pt2)
    }
    secC$sector <- cnt
    cnt <- cnt + 1

    sectsABC_list[[k]] <- rbind(secA, secB, secC)
    segms[nsegm,] <- c(sub_crcl3$x[1], sub_crcl3$yup[1], nsegm)
    segms[nsegm+1,] <- c(secC$x[nrow(secC)], secC$y[nrow(secC)], nsegm)
    nsegm <- nsegm+2
  }
  sectsABC <- do.call(rbind, sectsABC_list)
  names(sectsABC)[2] <- "y"
  # sectsABC$sector <- factor(sectsABC$sector)

  #############################

  sec0A$x <- -sec0A$x
  sec0A$sector <- cnt
  cnt <- cnt + 1
  sec0B$x <- -sec0B$x
  sec0B$sector <- cnt
  cnt <- cnt + 1
  sec0C$x <- -sec0C$x
  sec0C$sector <- cnt
  cnt <- cnt + 1
  sec0_right <- rbind(sec0A, sec0B, sec0C)
  #############################
  restr <- crcl(0, -41.25, 4, nsteps = 200)
  pol_restric <- data.frame(x = c(-4, -4, restr$x, 4, 4),
                            y = c(-43, -41.25, restr$yup, -41.25, -43))
  pol_restric$sector <- 0

  sects <- rbind(pol_restric, sec0_left, sectsABC, sec0_right)
  segms <- as.data.frame(segms)
  names(segms) <- c("x","y","nsegm")
  return(list(sects=sects, segms=segms, arc=arc))
}

#' @noRd
crcl <- function(x0, y0, r, span=r, nsteps=100) {
  x <- seq(x0-span,x0+span,length.out=nsteps)
  ylo <- y0-sqrt(r^2-(x-x0)^2)
  yup <- y0+sqrt(r^2-(x-x0)^2)
  data.frame(x=x, ylo=ylo, yup=yup)
}
