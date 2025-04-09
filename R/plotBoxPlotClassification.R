
plotBoxPlotClassification <- function(all_classifications, selected_threshold){

  all_classifications %>%
    dplyr::filter(Sens_Spec == selected_threshold) %>%
    tidyr::pivot_longer(-c(SampleID, Plate, pred_class_max, Sens_Spec), names_to = "Antigen", values_to = "RAU") %>%
    dplyr::mutate(pred_class_max = factor(pred_class_max, levels = c("seronegative", "seropositive"))) %>%
    ggplot2::ggplot(aes(x = pred_class_max, y = RAU, fill = pred_class_max)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_y_log10() +
    ggplot2::scale_fill_manual(values = c(seronegative = "#878787", seropositive = "#d6604d")) +
    ggplot2::labs(title = paste0("Threshold Chosen: "), selected_threshold,
                  x = "Classification", y = "RAU", fill = "Classification") +
    ggplot2::facet_grid(~Antigen) +
    ggplot2:: theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

}
