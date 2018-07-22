# You can learn more about package authoring with RStudio at:
#
#   devtools::use_package("dplyr")
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

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

  print(df1)
  print(ord_df1)
  print(ord_df2)
  print(cbind(levels(df1$ID), as.character(data[,order.by]), ord_df2))

  df2 <- data %>%
    dplyr::select(id, bars, line) %>%
    dplyr::mutate(Line = !!sym(line)) %>%
    dplyr::rename(ID=!!id) %>%
    dplyr::mutate(rsum=rowSums(dplyr::select(., bars))) %>%
    dplyr::mutate(y=Line*max(rsum)/max(Line)) %>%
    dplyr::slice(ord_df2) %>%
    dplyr::mutate(x=1:n())

  ggplot(data=df1, aes(x=ID, y=Value, fill=Variables)) +
    geom_bar(stat="identity") +
    scale_fill_brewer(labels=labels.bars, palette="Paired") +
    theme(legend.position="top") +
    ggtitle(title) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.25)) +
    geom_line(data=df2, mapping=aes(x=x, y=y), lwd=1.5, col='grey', inherit.aes=F) +
    scale_y_continuous(name="Variables", limits=c(0,NA),
                       sec.axis=sec_axis(~.*max(df2$Line)/max(df2$rsum), name=label.line))

}
