
getAntigenCounts <- function(process_counts, plate_list){

  #############################################################################
  # Data Wrangling
  #############################################################################

  antigen_specific_df <- process_counts %>%
    dplyr::select(Location, Antigen, Warning, Count, Plate) %>%
    dplyr::group_by(Location, Antigen, Count, Plate) %>%
    dplyr::summarise(Sum = sum(Warning)) %>%
    dplyr::mutate(Repeat = case_when(
      Sum>=1 ~ "repeat",
      Sum<1 ~ "sufficient beads"
    )) %>%
    dplyr::mutate(
      Row = as.factor(substr(Location, 1, nchar(Location)-1)),
      Row = gsub("1", "", Row),
      Col = as.numeric(substr(Location, 2, nchar(Location))),
      Count = as.numeric(Count),
      Repeat = factor(Repeat, levels = c("sufficient beads", "repeat")),
      QC_antigen = ifelse(Repeat == "sufficient beads", "pass", "fail")
    )

  #############################################################################
  # Create Table Output
  #############################################################################

  table <- getSampleID(antigen_specific_df, plate_list) %>%
    ungroup() %>%
    dplyr::select(SampleID = Sample, Location, Antigen, Plate, Repeat, Count)
  antigen_specific_df_final <- antigen_specific_df %>%
    dplyr::left_join(table, by = c("Plate", "Count", "Repeat", "Antigen", "Location")) %>%
    dplyr::select(-c(Row, Col, Sum)) %>%
    arrange(Location, Antigen, Plate)

  return(antigen_specific_df_final)

}
