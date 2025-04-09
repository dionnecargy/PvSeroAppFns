
plotMFI <- function(mfi_to_rau_output, location){

  df_results <- mfi_to_rau_output[[2]]
  df_results <- df_results %>%
    dplyr::select(SampleID, Plate, ends_with("_MFI")) %>%
    dplyr::rename_with(~str_replace(., "_MFI", ""), ends_with("_MFI")) %>%
    tidyr::pivot_longer(-c(SampleID, Plate), names_to = "Antigen", values_to = "MFI") %>%
    dplyr::mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # Reorder by plate number
                  MFI = as.numeric(MFI))

  devtools::load_all()

  if (location == "PNG"){

    df_wehi <- longitudinal_MFI

    plot <- df_results %>%
      ggplot2::ggplot(aes(x= Antigen, y = MFI)) +
      ggplot2::geom_boxplot(data = df_wehi, aes(x = Antigen, y = MFI), fill = "grey", colour = "darkgrey") +
      ggplot2::geom_boxplot(aes(fill = Antigen)) +
      ggplot2::scale_y_log10(breaks = c(10, 100, 1000, 10000), limits = c(10, 10000), labels = c("10", "100", "1,000", "10,000")) +
      ggplot2::scale_fill_brewer(palette = "Paired", type = "qual") +
      ggplot2::labs(x = "Antigen", y = "Antibody log(MFI)") +
      ggplot2::facet_wrap( ~ Plate) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

  } else if (location == "ETH") {

    plot <- df_results %>%
      ggplot2::ggplot(aes(x= Antigen, y = MFI, fill = Antigen)) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_y_log10() +
      ggplot2::scale_fill_brewer(palette = "Paired", type = "qual") +
      ggplot2::labs(x = "Antigen", y = "Antibody log(MFI)") +
      ggplot2::facet_wrap( ~ Plate) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

  }

  return(plot)

}
