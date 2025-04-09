
plotCounts <- function(counts_output, experiment_name){
  bead_counts <- counts_output
  bead_counts$Plate <- factor(bead_counts$Plate, levels = unique(bead_counts$Plate[order(as.numeric(str_extract(bead_counts$Plate, "\\d+")))])) # reorder by plate number
  bead_counts %>%
    ggplot2::ggplot(mapping = aes(x = Col, y = fct_rev(Row), fill = Repeat), fill = summary) +
    ggplot2::geom_tile(aes(height = 0.90, width = 0.90)) +
    ggplot2::scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), position = "top") +
    ggplot2::scale_fill_manual(values = c("sufficient beads" = "#91bfdb", "repeat" = "#d73027"), drop=FALSE) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "", title = experiment_name , fill = "") +
    ggplot2::facet_wrap( ~ Plate, ncol = 3, scales = "free_y")  # This will create separate facets for each level of 'Plate'
}
