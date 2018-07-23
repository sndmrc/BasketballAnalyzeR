#' Create a graph with a barplot and a line plot
#'
#' @param data A dataframe
#' @param id The name of the ID variable
#' @param bar Vector of names of the bar variables
#' @param line The name of the line variable
#' @param order.by The name of the variable used to order name players (on the x axis)
#' @param labels.bars Vector of labels for the bar variables
#' @param label.line The label for the line variable on the second axis (on the right)
#' @param title Title for the plot
#' @return A ggplot2 object
#' @export
#' @examples
#' data("NBA1718Players")
#' dts <- subset(NBA1718Players, Team=="Houston Rockets" & MIN>=500)
#' barline(data=dts, id="Player", bars=c("P2p","P3p","FTp"),
#'         line="MIN", order.by="Player",
#'         labels.bars=c("2P","3P","FT"), title="Graph title")
barline <- function(data, id, bars, line, order.by=id, labels.bars=NULL, label.line=NULL, title="") {

  if (is.null(labels.bars)) {
    labels.bars=bars
  }
  if (is.null(label.line)) {
    label.line=line
  }

  df1 <- data %>%
    dplyr::select(id, bars, line) %>%
    tidyr::gather(key="Variables", value="Value", bars) %>%
    dplyr::rename(ID=!!id) %>%
    dplyr::mutate(Variables=factor(Variables, levels=bars))

  var_ord <- data[,order.by]
  if (class(var_ord)=="factor") {
    ord_df1 <- order(levels(var_ord))
    ord_df2 <- match(levels(df1$ID)[ord_df1], data[,id])
  } else {
    ord_df2 <- order(var_ord, decreasing=T)
    ord_df1 <- match(data[ord_df2,id], levels(df1$ID))
  }

  df1 <- df1 %>%
    dplyr::mutate(ID=factor(ID,levels=levels(ID)[ord_df1]))

  df2 <- data %>%
    dplyr::select(id, bars, line) %>%
    dplyr::mutate(Line = !!rlang::sym(line)) %>%
    dplyr::rename(ID=!!id) %>%
    dplyr::mutate(rsum=rowSums(dplyr::select(., bars))) %>%
    dplyr::mutate(y=Line*max(rsum)/max(Line)) %>%
    dplyr::slice(ord_df2) %>%
    dplyr::mutate(x=1:n())

  ggplot(data=df1, aes(x=ID, y=Value, fill=Variables)) +
    geom_bar(stat="identity") +
    scale_fill_brewer(labels=labels.bars, palette="Paired") +
    theme(legend.position="top") + ggtitle(title) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.25)) +
    geom_line(data=df2, mapping=aes(x=x, y=y), lwd=1.5, col='grey', inherit.aes=F) +
    scale_y_continuous(name="Variables", limits=c(0,NA),
                       sec.axis=sec_axis(~.*max(df2$Line)/max(df2$rsum), name=label.line))
}
