#' Plot variability
#'
#' @param x A 'variability' objects
#' @param title Plot title
#' @param ... other graphical parameters
#' @return A ggplot2 object
#' @examples
#' Pbox.BC <- subset(Pbox, Team=="Oklahoma City Thunder" & MIN >= 500,
#'                     select=c("P2p","P3p","FTp","P2A","P3A","FTA"))
#' out <- variability(data=Pbox.BC, data.var=c("P2p","P3p","FTp"),
#'                    size.var=c("P2A","P3A","FTA"), weight=TRUE)
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

  df.data <- x[["data"]]
  df.size <- x[["size"]]
  weight <- x[["weight"]]
  vc <- x[["VC"]]
  rg <- x[["range"]]
  nc.data <- ncol(df.data)
  nc.size <- ncol(df.size)
  nr <- nrow(df.data)
  if (nc.data == nc.size) {
    df3 <- data.frame(id = rep(1:nc.data, each = nr), V1 = utils::stack(df.data)$values, V2 = utils::stack(df.size)$values)
  } else if (nc.size == 1) {
    df3 <- data.frame(id = rep(1:nc.data, each = nr), V1 = utils::stack(df.data)$values, V2 = rep(df.size, nc.data))
  }
  names(df3) <- c("id", "V1", "V2")

  p <- ggplot(df3, aes(x = id, y = V1)) +
       geom_point(aes(size = V2), shape = 21, colour = "dodgerblue") +
       scale_size_area(max_size = 20) +
       labs(title = title) + labs(size = "") +
       scale_x_continuous(name = "", limits = c(0, nc.data + 1),
                                     breaks = 0:(nc.data + 1),
                                     labels = c("", names(df.data), "")) +
       scale_y_continuous(name = "", limits = c(0, 4 * max(df3$V1)/3)) +
       annotate("text", x = 1:nc.data, y = rep( 9 * max(df3$V1)/8, nc.data),
                        label = paste0("VC = ", round(vc, 2), "%"), size = 4) +
       annotate("text", x = 1:nc.data, y = rep(10 * max(df3$V1)/8, nc.data),
                        label = paste0("Range = ", round(rg, 2)), size = 4) +
       guides(size=guide_legend(title=paste(names(df.size), collapse="\n")))

  return(p)
}

