
plotBlanks <- function(antigen_output, experiment_name){
  master_file <- antigen_output
  blanks <- master_file$blanks
  blanks %>%
    dplyr::select(-Location) %>%
    tidyr::pivot_longer(-c(Sample, Plate), names_to = "Antigen", values_to = "MFI") %>%
    dplyr::mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))]))) %>% # Reorder by plate number
    ggplot2::ggplot(aes(x = factor(Antigen), y = as.numeric(MFI), fill = Sample)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::geom_hline(yintercept = 50, linetype = "dashed", color = "grey") +
    ggplot2::labs(x = "Antigen",
                  y = "MFI",
                  title = experiment_name) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggplot2::facet_wrap(~ Plate)  # Create separate facets for each 'plate'
}
