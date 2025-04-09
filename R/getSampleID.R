
getSampleID <- function(counts_data, plate_list) {
  table <- counts_data
  layout <- plate_list

  # Extract row and column info
  table$Row <- substr(table$Location, 1, 1)
  table$Col <- substr(table$Location, 2, 2)

  # Apply logic to extract Sample directly
  table$Sample <- mapply(function(Plate, Row, Col) {
    platelayout_df <- layout[[Plate]]
    row_index <- which(platelayout_df$Plate == Row)
    col_index <- as.integer(Col) + 1  # Adjust for 1-based indexing
    platelayout_df[row_index, col_index]
  }, table$Plate, table$Row, table$Col)

  return(table)
}
