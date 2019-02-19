#' Reports whether x is a networkdata object
#'
#' @param x An object to test
#' @return returns TRUE if its argument is of class 'networkdata' and FALSE otherwise
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP.GSW <- subset(PbP, team=="GSW" & player!="")
#' out <- assistnet(PbP.GSW)
#' is.assistnet(out)
#' @export

is.assistnet <- function(x) {
  inherits(x, "assistnet")
}
