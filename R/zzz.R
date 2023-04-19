.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    '\nIf you wish to replicate the figures included in the book of\nZuccolotto and Manisera (2020) and\nif your R version is 3.6.0 or higher, you need to enter\nRNGkind(sample.kind = "Rounding")\nat the beginning of your working session')
}
