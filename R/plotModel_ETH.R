
plotModel_ETH <- function(mfi_to_rau_output, antigens_output){

  # Load model results
  model_results <- mfi_to_rau_output[[3]]

  # Convert the list of data frames into a single data frame
  combined_data <- model_results %>%
    dplyr::bind_rows(.id = "Plate")

  # Generate plots for each plate, grouping antigens together
  plots_model <- lapply(unique(combined_data$Plate), function(plate_name) {
    ggplot2::ggplot(data = subset(combined_data, Plate == plate_name),
                    aes(x = dilution, y = mfi_pred, color = antigen)) +  # Use 'Antigen' to differentiate lines
      ggplot2::geom_line() +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
      ggplot2::geom_point(data = subset(combined_data, Plate == plate_name), aes(x = dilution, y = mfi, color = antigen)) +
      ggplot2::labs(x = "Antibody Dilution",
                    y = "Standard Curve (log(MFI))",
                    fill = "Antigen",
                    title = paste("Standard Curves for Plate:", plate_name)) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~ antigen, scales = "free_y")  # Create a separate plot for each Antigen
  })

  # Assign names to the list of plots for clarity
  names(plots_model) <- unique(combined_data$Plate)

  return(plots_model)
}
