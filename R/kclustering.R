#' k-means clustering of NBA teams
#'
#' @param data A dataframe
#' @param k Numer of clusters
#' @param labels labels
#' @param nclumax Max number of clusters (when k=NULL)
#' @param nruns nruns
#' @return A 'klust' object, i.e. a list of ...(specificare)
#' @examples
#' FF <- fourfactors(Tbox,Obox)
#' OD_Rtg <- FF$ORT/FF$DRT
#' F1_r <- FF$F1_O/FF$F1_T
#' F2_r <- FF$F2_T/FF$F2_O
#' F3_O <- FF$F3_O
#' F3_D <- FF$F3_T
#' P3M <- Tbox$P3M
#' STL_r <- Tbox$STL/Obox$STL
#' X <- data.frame(OD_Rtg,F1_r,F2_r,F3_O,F3_D,P3M,STL_r)
#' kclu1 <- kclustering(X)
#' plot(kclu1)
#' kclu2 <- kclustering(X, k=9)
#' plot(kclu2)
#' @export


kclustering <- function(data, k = NULL, labels = NULL, nclumax = 10, nruns = 10) {

  varb <- function(x) {
    x <- stats::na.omit(x)
    varb <- sum((x - mean(x))^2)/(length(x))
    return(varb)
  }

  if (is.null(labels)) {
    labels <- c(1:nrow(data))
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
        ca.k <- stats::kmeans(data, nclu, algorithm = "Hartigan-Wong")
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
    clustnames <- paste("Cluster", 1:k, "- CHI =", round(colMeans(vars), 2))
    profiles <- data.frame(profiles, clustnames)
    cluster.list <- by(subjects.cluster[, 1], subjects.cluster[, 2], list)
    out <- list(k = k, Subjects = subjects.cluster, ClusterList = cluster.list, Profiles = profiles)
  }
  class(out) <- append("kclust", class(out))
  return(out)
}
