
plotStds <- function(antigen_output, location, experiment_name){
  devtools::load_all()

  master_file <- antigen_output
  stds <- master_file$stds

  stds_1 <- stds %>%
    dplyr::select(-Location) %>%
    tidyr::pivot_longer(-c(Sample, Plate), names_to = "Antigen", values_to = "MFI") %>%
    dplyr::mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # reorder by plate number
                  Sample = factor(Sample, c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")),
                  MFI = as.numeric(MFI))

  location_1 <- ifelse(location == "ETH", "ETH", "PNG")

  wehi_stds <- all_stds_MFI
  wehi_stds <- wehi_stds %>% dplyr::filter(Location==location_1)

  gg <-
    ggplot2::ggplot() +
    ggplot2::geom_point(data = wehi_stds, aes(x = Sample, y = MFI), colour = "grey", alpha = 0.25) +
    ggplot2::geom_point(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate,
                                           text = paste("Sample:", Sample, "<br>MFI:", MFI, "<br>Plate:", Plate))) +
    ggplot2::geom_line(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate)) +
    ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
    ggplot2::labs(x = "Standard Curve",
                  y = "log(MFI)",
                  title = experiment_name) +
    ggplot2:: facet_wrap(~Antigen) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

}
