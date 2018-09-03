#' Plot variability
#'
#' @param x A 'variability' objects
#' @param title Plot title
#' @param ... other graphical parameters
#' @return A ggplot2 object
#' @examples
#' Pbox.BC <- subset(Pbox, Team=="Oklahoma City Thunder" & MIN >= 500,
#'                     select=c("P2p","P3p","FTp","P2A","P3A","FTA"))
#' out <- variability(data=Pbox.BC, size=c("P2A","P3A","FTA"), weight=TRUE)
#' plot(out)
#' @method plot variability
#' @export

plot.variability <- function(x, title = NULL, ...) {

  if (!is.variability(x)) {
    stop("Not an object of class 'variability'")
  }

  id <- V1 <- V2 <- NULL
  if (is.null(title)) {
    title <- "Variability diagram"
  }

  df1 <- x[["data"]]
  df2 <- x[["size"]]
  weight <- x[["weight"]]
  vc <- x[["VC"]]
  rg <- x[["range"]]
  nc1 <- ncol(df1)
  nc2 <- ncol(df2)
  nr <- nrow(df1)
  if (nc1 == nc2) {
    df3 <- data.frame(id = rep(1:nc1, each = nr), V1 = utils::stack(df1)$values, V2 = utils::stack(df2)$values)
  } else if (nc2 == 1) {
    df3 <- data.frame(id = rep(1:nc1, each = nr), V1 = utils::stack(df1)$values, V2 = rep(df2, nc1))
  }
  names(df3) <- c("id", "V1", "V2")

  p <- ggplot(df3, aes(x = id, y = V1)) +
       geom_point(aes(size = V2), shape = 21, colour = "dodgerblue") +
       scale_size_area(max_size = 20) +
       labs(title = title) + labs(size = "") +
       scale_x_continuous(name = "", limits = c(0, nc1 + 1),
                                     breaks = 0:(nc1 + 1),
                                     labels = c("", names(df1), "")) +
       scale_y_continuous(name = "", limits = c(0, 4 * max(df3$V1)/3)) +
       annotate("text", x = 1:nc1, y = rep( 9 * max(df3$V1)/8, nc1),
                        label = paste0("vc = ", round(vc, 2), "%"), size = 4) +
       annotate("text", x = 1:nc1, y = rep(10 * max(df3$V1)/8, nc1),
                        label = paste0("range = ", round(rg, 2)), size = 4)

  print(p)
  invisible(p)
}

