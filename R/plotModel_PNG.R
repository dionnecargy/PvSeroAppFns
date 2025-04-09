plotModel_PNG <- function(mfi_to_rau_output, antigens_output){

  model_results <- mfi_to_rau_output[[3]]

  # Create a combined data frame with plate and protein
  combined_data <- do.call(rbind, lapply(names(model_results), function(file_name) {
    # Get the list of antigens for this file
    antigens <- model_results[[file_name]]
    # For each antigen in the file
    lapply(names(antigens), function(antigen_name) {
      data <- antigens[[antigen_name]]  # Get the antigen's data frame
      data$Plate <- file_name  # Add plate column will be the file name
      data$Antigen <- antigen_name  # Add antigen column will be the antigen name
      return(data)
    })
  }))

  # Convert the list of data frames into a single data frame
  combined_data <- do.call(rbind, combined_data)

  ### Get Standards for points
  stds_file <- antigens_output$stds
  stds_log <-
    stds_file %>%
    dplyr::mutate(across(-c(Location, Sample, Plate), ~ as.numeric(.))) %>%
    tidyr::pivot_longer(-c(Location, Sample, Plate), names_to = "Antigen", values_to = "stdcurve") %>%
    dplyr::mutate(dilution = ifelse(
      Sample == "S1", 1/50,
      ifelse(Sample == "S2", 1/100,
             ifelse(Sample == "S3", 1/200,
                    ifelse(Sample == "S4", 1/400,
                           ifelse(Sample == "S5", 1/800,
                                  ifelse(Sample == "S6", 1/1600,
                                         ifelse(Sample == "S7", 1/3200,
                                                ifelse(Sample == "S8", 1/6400,
                                                       ifelse(Sample == "S9", 1/12800,
                                                              ifelse(Sample == "S10", 1/25600, NA)))))))))))

  # Generate plots for each plate, grouping proteins together
  plots_model <- lapply(unique(combined_data$Plate), function(plate_name) {
    ggplot2::ggplot() +  # Use 'protein' to differentiate lines
      ggplot2::geom_line(data = subset(combined_data, Plate == plate_name), aes(x = dilution, y = exp(`1`), color = Antigen)) +
      ggplot2::geom_point(data = subset(stds_log, Plate == plate_name), aes(x = dilution, y = stdcurve, color = Antigen)) +
      ggplot2::scale_x_log10(breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 0.03),
                             labels = c("0.00001", "0.0001", "0.001", "0.01", "0.03")) +
      ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
      ggplot2::labs(x = "Antibody Dilution",
                    y = "Standard Curve (log(MFI))",
                    title = paste("Standard Curves for Plate:", plate_name)) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~ Antigen, scales = "free")  # Create a separate plot for each antigen
  })

  # Assign names to the list of plots for clarity
  names(plots_model) <- unique(combined_data$Plate)

  # Arrange all plots using grid.arrange
  # arranged_plots <- do.call(grid.arrange, c(plots_model, ncol = 1))  # Stack plots vertically

  return(plots_model)
}
