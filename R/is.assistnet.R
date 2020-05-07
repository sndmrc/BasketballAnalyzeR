#' Reports whether x is a 'networkdata' object
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param x an object to test.
#' @seealso \code{\link{assistnet}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return Returns TRUE if its argument is of class \code{networkdata} and FALSE otherwise.
#' @examples
#' PbP <- PbPmanipulation(PbP.BDB)
#' PbP.GSW <- subset(PbP, team=="GSW" & player!="")
#' out <- assistnet(PbP.GSW)
#' is.assistnet(out)
#' @export

is.assistnet <- function(x) {
  inherits(x, "assistnet")
}
