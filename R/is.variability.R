#' Reports whether x is a 'variability' object
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param x an object to test.
#' @seealso \code{\link{variability}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return Returns TRUE if its argument is of class \code{variability} and FALSE otherwise.
#' @examples
#' Pbox.BC <- subset(Pbox, Team=="Oklahoma City Thunder" & MIN >= 500,
#'                     select=c("P2p","P3p","FTp","P2A","P3A","FTA"))
#' out <- variability(data=Pbox.BC, data.var=c("P2p","P3p","FTp"),
#'                    size.var=c("P2A","P3A","FTA"), weight=TRUE)
#' is.variability(out)
#' @export


is.variability <- function(x) {
  inherits(x, "variability")
}
