
process_counts <- function(antigens_output){

  # 1. Store Counts Data
  counts_data <- antigens_output$counts

  # 2. Data Wrangling
  counts_data <- counts_data %>%
    dplyr::mutate(Location=gsub(".*,", "", Location)) %>%
    dplyr::mutate(Location=substr(Location, 1, nchar(Location)-1))  %>%
    tidyr::pivot_longer(-c(Sample, Location, Plate), names_to = "Antigen", values_to = "Count") %>%
    dplyr::mutate(Warning = case_when(
      as.numeric(Count)<15~1,
      as.numeric(Count)>=15~0
    ))

  return(counts_data)
}
