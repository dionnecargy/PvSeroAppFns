
plotRAU <- function(mfi_to_rau_output, location){

  devtools::load_all()

  df_results <- mfi_to_rau_output[[2]]
  df_results <- df_results %>%
    dplyr::select(SampleID, Plate, ends_with("_Dilution")) %>%
    dplyr::rename_with(~str_replace(., "_Dilution", ""), ends_with("_Dilution")) %>%
    tidyr::pivot_longer(-c(SampleID, Plate), names_to = "Antigen", values_to = "RAU") %>%
    dplyr::mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # Reorder by plate number
                  RAU = as.numeric(RAU))

  if (location == "PNG"){

    df_wehi <- longitudinal_RAU

    plot <- df_results %>%
      ggplot2::ggplot(aes(x= Antigen, y = RAU, fill = Antigen)) +
      ggplot2::geom_boxplot(data = df_wehi, aes(x = Antigen, y = RAU), fill = "grey", colour = "darkgrey") +
      ggplot2::geom_boxplot() +
      ggplot2::scale_y_log10(breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 0.03),
                             labels = c("0.00001", "0.0001", "0.001", "0.01", "0.03")) +
      ggplot2::scale_fill_brewer(palette = "Paired", type = "qual") +
      ggplot2::labs(x = "Antigen", y = "Antibody RAU") +
      ggplot2::facet_wrap( ~ Plate) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

  } else if (location == "ETH") {

    plot <- df_results %>%
      ggplot2::ggplot(aes(x= Antigen, y = RAU, fill = Antigen)) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_y_log10() +
      ggplot2::scale_fill_brewer(palette = "Paired", type = "qual") +
      ggplot2::labs(x = "Antigen", y = "Antibody RAU") +
      ggplot2::facet_wrap( ~ Plate) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

  }

  return(plot)

}
