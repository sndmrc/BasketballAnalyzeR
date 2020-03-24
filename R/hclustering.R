#' Agglomerative hierarchical clustering
#'
#' @author Marco Sandri, Paola Zuccolotto, Marica Manisera (\email{basketballanalyzer.help@unibs.it})
#' @param data numeric data frame
#' @param k integer, number of clusters
#' @param nclumax integer, maximum number of clusters (when \code{k=NULL})
#' @param labels character, row labels
#' @param linkage character, the agglomeration method to be used in \code{hclust} (see \code{method} in \link[stats]{hclust})
#' @details The \code{hclustering} function performs a preliminary standardization of columns in \code{data}.
#' @seealso \code{\link{plot.hclustering}}, \code{\link[stats]{hclust}}
#' @references P. Zuccolotto and M. Manisera (2020) Basketball Data Science: With Applications in R. CRC Press.
#' @return A \code{hclustering} object.
#' @return If \code{k} is \code{NULL}, the \code{hclustering} object is a list of 3 elements:
#' @return * \code{k} \code{NULL}
#' @return * \code{clusterRange} integer vector, values of \code{k} (from 1 to \code{nclumax}) at which the \emph{variance between} of the clusterization is evaluated
#' @return * \code{VarianceBetween} numeric vector, values of the \emph{variance between} evaluated for \code{k} in \code{clusterRange}
#' @return If \code{k} is not \code{NULL}, the \code{hclustering} object is a list of 5 elements:
#' @return * \code{k} integer, number of clusters
#' @return * \code{Subjects} data frame, subjects' cluster identifiers
#' @return * \code{ClusterList} list, clusters' composition
#' @return * \code{Profiles} data frame, clusters' profiles, i.e. the average of the variables within clusters and the cluster eterogeineity index (\code{CHI})
#' @return * \code{Hclust} an object of class \code{hclust}, see \code{\link[stats]{hclust}}
#' @examples
#' data <- with(Pbox, data.frame(PTS, P3M, REB=OREB+DREB, AST, TOV, STL, BLK, PF))
#' data <- subset(data, Pbox$MIN >= 1500)
#' ID <- Pbox$Player[Pbox$MIN >= 1500]
#' hclu1 <- hclustering(data)
#' plot(hclu1)
#' hclu2 <- hclustering(data, labels=ID, k=7)
#' plot(hclu2)
#' @export
#' @importFrom stats cutree

hclustering <- function(data, k = NULL, nclumax = 10, labels = NULL, linkage='ward.D') {

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
  rownames(data) <- labels
  hcl <- stats::hclust(dist(data), method=linkage)

  if (is.null(k)) {
    varfra.nclu <- array(0, nclumax)
    clu.k <- matrix(1, nunits, nclumax)
    clumean <- matrix(0, nclumax, nvars * nclumax)
    for (nclu in 2:nclumax) {
      clu <- stats::cutree(hcl, k = nclu)
      clumean <- matrix(unlist(by(data,clu,function(x) apply(x,2,mean))),ncol=nvars, byrow=TRUE)
      tbl_clust <- table(clu)/nunits
      varfra <- mapply(function(x, y) sum((x - mean(y))^2 * tbl_clust), as.data.frame(clumean), as.data.frame(data))
      varnei <- apply(data, 2, function(x) sum(tapply(x, clu, varb) * tbl_clust))
      varfra.nclu[nclu] <- sum(varfra)/sum(varfra + varnei)
      clu.k[, nclu] <- clu
    }
    out <- list(k = k, ClusterRange = 1:nclumax, VarianceBetween = varfra.nclu)
  } else {
    nclu <- k
    clu <- cutree(hcl, k = nclu)
    clumean <- matrix(unlist(by(data,clu,function(x) apply(x,2,mean))),ncol=nvars, byrow=TRUE)
    tbl_clust <- table(clu)/nunits
    varfra <- mapply(function(x, y) sum((x - mean(y))^2 * tbl_clust), as.data.frame(clumean), as.data.frame(data))
    varnei <- apply(data, 2, function(x) sum(tapply(x, clu, varb) * tbl_clust))
    varfra.nclu <- sum(varfra)/sum(varfra + varnei)  #in pratica una media degli eta ponderata con la varianza tot di ogni dimensione
    clu.k <- clu
    profiles <- as.data.frame(clumean)
    names(profiles) <- namesvars
    subjects.cluster <- data.frame(Label = labels, Cluster = clu)
    vars <- t(apply(data, 2, function(x) tapply(x, subjects.cluster$Cluster, varb)))
    #clustnames <- paste("Cluster", 1:k, "- CHI =", round(colMeans(vars), 2))
    #profiles <- data.frame(profiles, clustnames)
    profiles <- data.frame(ID=1:k, profiles, CHI=round(colMeans(vars), 2))
    cluster.list <- by(subjects.cluster[, 1], subjects.cluster[, 2], list)
    out <- list(k = k, Subjects = subjects.cluster, ClusterList = cluster.list, Profiles = profiles, Hclust=hcl)
  }
  class(out) <- append("hclustering", class(out))
  return(out)
}
