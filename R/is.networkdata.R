#' Reports whether x is a networkdata object
#'
#' @param x An object to test
#' @return returns TRUE if its argument is of class 'networkdata' and FALSE otherwise
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP.GSW <- subset(PbP, team=="GSW" & player!="")
#' out <- netcalc(PbP.GSW)
#' is.networkdata(out)
#' @export

is.networkdata <- function(x) {
  inherits(x, "networkdata")
}
