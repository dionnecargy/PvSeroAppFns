
plotBeadCounts <- function(antigens_counts_output){

  antigens_counts_output$Plate <- factor(antigens_counts_output$Plate, levels = unique(antigens_counts_output$Plate[order(as.numeric(str_extract(antigens_counts_output$Plate, "\\d+")))])) # reorder by plate number
  antigens_counts_output %>%
    ggplot(aes(Plate, Count, colour = Repeat, alpha = Repeat, size = Repeat,
               text = paste("Sample:", SampleID, "<br>Bead Count:", Count, "<br>Location:", Location,"<br>Plate:", Plate))) +
    geom_hline(yintercept = 15, linetype = "dashed", colour = "#861e18") +
    geom_point() +
    scale_y_continuous(breaks = c(0, 15, 50, 100, 150, 200)) +
    scale_colour_manual(values = c("sufficient beads" = "#91bfdb", "repeat" = "#d73027"), drop=FALSE) +
    scale_alpha_manual(values = c("sufficient beads" = 0.5, "repeat" = 1)) +
    scale_size_manual(values = c("sufficient beads" = 1, "repeat" = 3)) +
    labs(x = "Plate", y = "Bead Counts", alpha = "", colour = "", size = "") +  # Add legend title
    facet_grid(~ Antigen) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right") + # Show legend
    guides(alpha = "none") +
    guides(size = "none")

}
