
readPlateLayout <- function(plate_layout, antigen_output) {

  if (is.null(plate_layout) || !file.exists(plate_layout)) {
    stop("ERROR: Invalid plate layout file provided.")
  }

  sheet_names <- tryCatch({
    openxlsx::getSheetNames(plate_layout)
  }, error = function(e) {
    stop("ERROR: Failed to read sheet names. Ensure the file is a valid Excel file.")
  })

  # Step 1: Get the sheet names to confirm
  sheet_names <- openxlsx::getSheetNames(plate_layout)

  # Step 2: Read all sheets into plate_layout_list using indices
  plate_layout_list <- lapply(1:length(sheet_names), function(i) {
    openxlsx::read.xlsx(plate_layout, sheet = i)
  })

  # Step 3: Name each element in the list after the corresponding sheet name
  names(plate_layout_list) <- sheet_names

  # Step 4: Check if 'Plate' column exists in antigen_output$results
  antigen_output_results <- antigen_output$results

  if (!"Plate" %in% colnames(antigen_output_results)) {
    stop("ERROR: 'Plate' column is missing from antigen_output$results.")
  }

  # Step 5: Extract levels from 'Plate' column
  antigen_output_levels <- unique(as.character(antigen_output$results$Plate))  # Convert factor to character

  # Step 6: Compare plate names
  if (all(antigen_output_levels %in% sheet_names)) {
    message("Plate layouts correctly identified!")
  } else {
    stop("Plate layout sheets and plates labeled in raw data file names do not match. Ensure plate sheets are correctly labeled.")
  }

  return(plate_layout_list)
}
