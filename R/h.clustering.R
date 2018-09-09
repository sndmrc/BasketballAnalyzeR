#' Hierarchical clustering of NBA teams
#'
#' @param data A dataframe
#' @param k Numer of clusters
#' @param nclumax Max number of clusters (when k=NULL)
#' @param labels labels
#' @param linkage linkage
#' @return A 'hclustering' object, i.e. a list of ...(specificare)
#' @examples
#' data <- data.frame(Pbox$PTS,Pbox$P3M,
#'                    Pbox$OREB + Pbox$DREB, Pbox$AST,
#'                    Pbox$TOV, Pbox$STL, Pbox$BLK,Pbox$PF)
#' names(data) <- c("PTS","P3M","REB","AST","TOV","STL","BLK","PF")
#' data <- subset(data, Pbox$MIN >= 1500)
#' ID <- Pbox$Player[Pbox$MIN >= 1500]
#' out <- h.clustering(data, labels=ID, k=7)
#' plot(out)
#' @export
#' @importFrom stats cutree

h.clustering <- function(data, k = NULL, nclumax = 10, labels = NULL, linkage='ward.D') {

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
    clustnames <- paste("Cluster", 1:k, "- CHI =", round(colMeans(vars), 2))
    profiles <- data.frame(profiles, clustnames)
    cluster.list <- by(subjects.cluster[, 1], subjects.cluster[, 2], list)
    out <- list(k = k, Subjects = subjects.cluster, ClusterList = cluster.list, Profiles = profiles, Hclust=hcl)
  }
  class(out) <- append("hclustering", class(out))
  return(out)
}
