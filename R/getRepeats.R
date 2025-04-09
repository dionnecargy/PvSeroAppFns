
getRepeats <- function(counts_output, plate_list) {

  # 1. Filter "Repeats" in Counts Output
  repeats <- counts_output %>% dplyr::filter(Repeat == "repeat")

  # 2. If zero "Repeats" found, then write text. If "Repeats" found, then output table.
  if (nrow(repeats) == 0) {
    return("No repeats necessary.")
  } else {
    table <- getSampleID(counts_output, plate_list)
    table <- table %>%
      dplyr::select(Sample, Location, Plate, Repeat) %>%
      dplyr::filter(Repeat == "repeat")
    return(table)
  }
}
