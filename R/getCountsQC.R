
getCountsQC <- function(antigen_counts_output, counts_output){

  #############################################################################
  # Data Wrangling
  #############################################################################

  # 1. Data Wrangling to store counts per antigen output
  antigen_counts_only <- antigen_counts_output %>%
    pivot_wider(id_cols = c(SampleID, Location, Plate), names_from = "Antigen", values_from = "Count") %>%
    dplyr::select(Location, SampleID, Plate, everything()) %>%
    dplyr::rename_with(~ paste0(., "_Count"), .cols = where(is.numeric))

  # 2. Data Wrangling to store QC pass/fail per antigen output
  antigen_QC_only <- antigen_counts_output %>%
    pivot_wider(id_cols = c(SampleID, Location, Plate), names_from = "Antigen", values_from = "QC_antigen") %>%
    dplyr::rename_with(~ paste0(., "_QC"), .cols = -c(SampleID, Location, Plate))

  # 3. Join both antigen-specific data frames together
  joined_antigen_counts <- antigen_counts_only %>%
    left_join(antigen_QC_only, by = c("SampleID", "Location", "Plate"))

  #############################################################################
  # Re-arrange data
  #############################################################################

  # Get all base marker names by stripping _Count
  marker_bases <- names(joined_antigen_counts) %>%
    grep("_Count$", ., value = TRUE) %>%
    sub("_Count$", "", .)

  # Create the desired column order
  new_order <- c(
    "Location", "SampleID", "Plate",
    unlist(lapply(marker_bases, function(x) c(paste0(x, "_Count"), paste0(x, "_QC"))))
  )

  # Reordered data frame
  joined_antigen_counts <- joined_antigen_counts %>%
    dplyr::select(all_of(new_order))

  #############################################################################
  # Add total counts
  #############################################################################

  total_counts_only <- counts_output %>%
    dplyr::select(Location, Plate, QC_total)

  total_counts_final_output <- joined_antigen_counts %>%
    left_join(total_counts_only, by = c("Location", "Plate"))

  return(total_counts_final_output)

}
