.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    '\nIf you want to reproduce the figures contained in the book of\nZuccolotto and Manisera (2020) and\nif the version of your R machine is >= 3.6.0, you need to type\nRNGkind(sample.kind = "Rounding")\nat the beginning of your working session')
}
