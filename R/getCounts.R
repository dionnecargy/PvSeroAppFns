
getCounts <- function(process_counts){

  counts <- process_counts %>%
    dplyr::select(Location, Warning, Plate) %>%
    dplyr::group_by(Location, Plate) %>%
    dplyr::summarise(Sum = sum(Warning)) %>%
    dplyr::mutate(Repeat = case_when(
      Sum>=1 ~ "repeat",
      Sum<1 ~ "sufficient beads"
    )) %>%
    dplyr::mutate(
      Row = as.factor(substr(Location, 1, nchar(Location)-1)),
      Row = gsub("1", "", Row),
      Col = as.numeric(substr(Location, 2, nchar(Location))),
      QC_total = ifelse(Repeat == "sufficient beads", "pass", "fail")
    )

  return(counts)
}
