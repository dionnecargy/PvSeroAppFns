
readAntigens <- function(serodata_output){

  # Function to relabel column names
  relabel_columns <- function(df) {
    colnames(df) <- dplyr::case_when(
      stringr::str_detect(colnames(df), regex("EBP", ignore_case = TRUE)) ~ "EBP",
      stringr::str_detect(colnames(df), regex("LF005", ignore_case = TRUE)) ~ "LF005",
      stringr::str_detect(colnames(df), regex("LF010", ignore_case = TRUE)) ~ "LF010",
      stringr::str_detect(colnames(df), regex("LF016", ignore_case = TRUE)) ~ "LF016",
      stringr::str_detect(colnames(df), regex("(MSP8|L34)", ignore_case = TRUE)) ~ "MSP8",
      stringr::str_detect(colnames(df), regex("(P87|RBP2b-P87)", ignore_case = TRUE)) ~ "RBP2b.P87",
      stringr::str_detect(colnames(df), regex("(PTEX|PTEX150|L18)", ignore_case = TRUE)) ~ "PTEX150",
      stringr::str_detect(colnames(df), regex("CSS", ignore_case = TRUE)) ~ "PvCSS",
      TRUE ~ colnames(df) # Keep unmatched names as-is
    )
    return(df)
  }

  # Step 1: Read and process `master_file`
  master_file <- serodata_output

  # Step 2: Process `master_file$results`
  results_df <- master_file$results %>%
    as.data.frame() %>%
    relabel_columns() %>%
    dplyr::select(dplyr::any_of(c("Location", "Sample",  "Plate", "EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")))
  master_file$results <- results_df

  # Step 3: Loop through and process specific data frames in `master_file`
  dataframes_to_process <- c("results", "counts", "blanks", "stds")

  master_file <- lapply(names(master_file), function(df_name) {
    if (df_name %in% dataframes_to_process) {
      master_file[[df_name]] <- relabel_columns(master_file[[df_name]])
    }
    return(master_file[[df_name]])
  }) %>% setNames(names(master_file)) # Preserve list names

}
