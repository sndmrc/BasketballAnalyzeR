#' K-means  cluster analysis
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param data numeric data frame
#' @param k integer, number of clusters
#' @param labels character, row labels
#' @param nclumax integer, maximum number of clusters (when \code{k=NULL}) used for calculating the explained variance as function of the number of clusters
#' @param nruns integer, run the k-means algorithm \code{nruns} times and chooses the best solution according to a maximum explained variance criterion
#' @param iter.max integer, maximum number of iterations allowed in k-means clustering (see \link[stats]{kmeans})
#' @param algorithm character, the algorithm used in k-means clustering (see \link[stats]{kmeans})
#' @details The \code{kclustering} function performs a preliminary standardization of columns in \code{data}.
#' @seealso \code{\link{plot.kclustering}}, \code{\link[stats]{kmeans}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return A \code{kclustering} object.
#' @return If \code{k} is \code{NULL}, the \code{kclustering} object is a list of 3 elements:
#' @return * \code{k} \code{NULL}
#' @return * \code{clusterRange} integer vector, values of \code{k} (from 1 to \code{nclumax}) at which the \emph{variance between} of the clusterization is evaluated
#' @return * \code{VarianceBetween} numeric vector, values of the \emph{variance between} evaluated for \code{k} in \code{clusterRange}
#' @return If \code{k} is not \code{NULL}, the \code{kclustering} object is a list of 4 elements:
#' @return * \code{k} integer, number of clusters
#' @return * \code{Subjects} data frame, subjects' cluster identifiers
#' @return * \code{ClusterList} list, clusters' composition
#' @return * \code{Profiles} data frame, clusters' profiles, i.e. the average of the variables within clusters and the cluster eterogeineity index (\code{CHI})
#' @examples
#' FF <- fourfactors(Tbox,Obox)
#' X <- with(FF, data.frame(OD.Rtg=ORtg/DRtg,
#'                F1.r=F1.Def/F1.Off, F2.r=F2.Off/F2.Def,
#'                F3.O=F3.Def, F3.D=F3.Off))
#' X$P3M <- Tbox$P3M
#' X$STL.r <- Tbox$STL/Obox$STL
#' kclu1 <- kclustering(X)
#' plot(kclu1)
#' kclu2 <- kclustering(X, k=9)
#' plot(kclu2)
#' @export


kclustering <- function(data, k=NULL, labels=NULL, nclumax=10, nruns=10, iter.max=50, algorithm="Hartigan-Wong") {

  varb <- function(x) {
    x <- stats::na.omit(x)
    varb <- sum((x - mean(x))^2)/(length(x))
    return(varb)
  }

  if (is.null(labels)) {
    labels <- c(1:nrow(data))
  }

  if (!is.null(k)) {
     k <- ceiling(k)
     if (k<=1) stop('Number of clusters k not above 1')
  }

  namesvars <- names(data)
  data <- scale(data)
  nunits <- nrow(data)
  nvars <- ncol(data)

  if (is.null(k)) {
    varfra.nclu <- array(0, nclumax)
    clu.k <- matrix(1, nunits, nclumax)
    clumean <- matrix(0, nclumax, nvars * nclumax)
    for (nclu in 2:nclumax) {
      clu.r <- matrix(0, nunits, nruns)
      clumean.r <- array(0, c(nclu, nvars, nruns))
      varfra.tot.nruns <- array(0, nruns)
      for (r in 1:nruns) {
        ca.k <- stats::kmeans(data, nclu, algorithm=algorithm, iter.max=iter.max)
        clu.r[, r] <- ca.k$cluster
        clumean.r[, , r] <- ca.k$centers
        tbl_clust <- table(ca.k$cluster)/nunits
        varfra <- mapply(function(x, y) sum((x - mean(y))^2 * tbl_clust), as.data.frame(ca.k$centers), as.data.frame(data))
        varnei <- apply(data, 2, function(x) sum(tapply(x, ca.k$cluster, varb) * tbl_clust))
        varfra.tot.nruns[r] <- sum(varfra)/sum(varfra + varnei)
      }
      rm <- which.max(varfra.tot.nruns)
      varfra.nclu[nclu] <- varfra.tot.nruns[rm]
      clu.k[, nclu] <- clu.r[, rm]
      clumean[1:nclu, ((nclu - 1) * nvars + 1):(nclu * nvars)] <- clumean.r[, , rm]
    }
    out <- list(k = k, ClusterRange = 1:nclumax, VarianceBetween = varfra.nclu)
  } else {
    nclu <- k
    clu.r <- matrix(0, nunits, nruns)
    clumean.r <- array(0, c(nclu, nvars, nruns))
    varfra.tot.nruns <- array(0, nruns)
    for (r in 1:nruns) {
      ca.k <- stats::kmeans(data, nclu, algorithm = "Hartigan-Wong")
      clu.r[, r] <- ca.k$cluster
      clumean.r[, , r] <- ca.k$centers
      tbl_clust <- table(ca.k$cluster)/nunits
      varfra <- mapply(function(x, y) sum((x - mean(y))^2 * tbl_clust), as.data.frame(ca.k$centers), as.data.frame(data))
      varnei <- apply(data, 2, function(x) sum(tapply(x, ca.k$cluster, varb) * tbl_clust))
      varfra.tot.nruns[r] <- sum(varfra)/sum(varfra + varnei)  #in pratica una media degli eta ponderata con la varianza tot di ogni dimensione
    }
    rm <- which.max(varfra.tot.nruns)
    varfra.nclu <- varfra.tot.nruns[rm]
    clu.k <- clu.r[, rm]
    clumean <- clumean.r[, , rm]
    profiles <- as.data.frame(clumean)
    names(profiles) <- namesvars
    subjects.cluster <- data.frame(Label = labels, Cluster = clu.k)
    vars <- t(apply(data, 2, function(x) tapply(x, subjects.cluster$Cluster, varb)))
    #clustnames <- paste("Cluster", 1:k, "- CHI =", round(colMeans(vars), 2))
    #profiles <- data.frame(profiles, clustnames)
    profiles <- data.frame(ID=1:k, profiles, CHI=round(colMeans(vars), 2))
    cluster.list <- by(subjects.cluster[, 1], subjects.cluster[, 2], list)
    out <- list(k = k, Subjects = subjects.cluster, ClusterList = cluster.list, Profiles = profiles)
  }
  class(out) <- append("kclustering", class(out))
  return(out)
}
