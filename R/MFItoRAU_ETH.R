MFItoRAU_ETH <- function(antigen_output, plate_list, counts_QC_output){

  devtools::load_all()

  master_file <- antigen_output$results
  L <- master_file %>% mutate(across(-c(Location, Sample, Plate), as.numeric))
  layout <- plate_list

  ##########################################################################################################
  #### Reference Fit
  ##########################################################################################################

  refs <- png_eth_stds
  # MAGIC PARAMETERS FOR THIS SECTION
  s1_concentration <- 1/50
  s10_relative_dilution <- 2^-9
  current_min_relative_dilution <- s10_relative_dilution
  # END MAGIC PARAMETER DEFINITIONS

  control = list(maxit = 10000,
                 abstol = 1e-10,
                 reltol = 1e-8)

  initial_solution = c(-1.0, 0.0, 10, 0.0, 0.0)

  ref_fit <- refs %>%
    dplyr::group_by(.data$std_plate, .data$antigen) %>%
    tidyr::nest()  %>%
    dplyr::mutate(
      .keep = "none",
      eth_fit = purrr::map(data, ~ {
        fit_standard_curve(.x$eth_mfi, .x$dilution, control)
      }),
      png_fit = purrr::map(data, ~ {
        fit_standard_curve(.x$png_mfi, .x$dilution, control)
      })
    )

  reference_antigens = unique(ref_fit$antigen)

  excluded_cols <- c("Location", "Sample", "Plate")
  remaining_cols <- setdiff(colnames(L), excluded_cols)
  antigens <- remaining_cols[remaining_cols != ""]

  L$type.letter <- substr(L$Sample, start=1, stop=1) # Categorises into "B" = "Blank", "S" = "Standards", "U" or "X" = "Samples"

  ##########################################################################################################
  #### Initialise outputs and prepare function by plate
  ##########################################################################################################

  # Iterate over each level in L$Plate and corresponding layout data frame
  results_all <- list()  # To store results for all plates
  model_results_all <- list()  # To store model results for all plates
  MFI_RAU_results_all <- list() # To store MFI to RAU conversion results for all plates

  for (plate_idx in seq_along(unique(L$Plate))) {
    plate_level <- unique(L$Plate)[plate_idx]
    subset_data <- L[L$Plate == plate_level, ]

    ##########################################################################################################
    #### Apply conversion
    ##########################################################################################################

    eth_qa_sc <- subset_data %>%
      dplyr::filter(type.letter == "S") %>%
      tidyr::pivot_longer(-c(Sample, Location, Plate, type.letter), names_to = "antigen", values_to = "mfi") %>%
      dplyr::mutate(dilution = 2 ^ (-as.numeric(gsub( # 2 = dilution factor
        "\\D", "", .data$`Sample`
      )) + 1))  %>%
      dplyr::group_by(.data$antigen) %>%
      tidyr::nest()

    eth_qa_mfi <- subset_data %>%
      dplyr::filter(type.letter == "U") %>%
      tidyr::pivot_longer(-c(Sample, Location, Plate, type.letter), names_to = "antigen", values_to = "mfi") %>%
      dplyr::group_by(.data$antigen) %>%
      tidyr::nest()

    qa_fit <- eth_qa_sc %>%
      dplyr::mutate(.keep = "none", new_fit = purrr::map(data, ~ {
        fit_standard_curve(.x$mfi, .x$dilution, control)
      }))

    # We have the fit for each antigen.
    eth_converted = dplyr::inner_join(ref_fit, qa_fit) %>%
      dplyr::inner_join(eth_qa_mfi) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(.keep = "none", data = list(
        data |> dplyr::mutate(
          .keep = "none",
          mfi = .data$mfi,
          Sample = .data$Sample,
          # dilution = convert_between_curves(.data$mfi, new_fit, eth_fit, png_fit)
          dilution = convert_mfi_to_dilution_no_lower_bound(mfi,new_fit, current_min_relative_dilution*2^-1), #This makes it s11 not s10 - Eamon
          ref_mfi = convert_dilution_to_mfi(dilution,eth_fit),
          dilution = convert_mfi_to_dilution(ref_mfi,png_fit, s10_relative_dilution)
        )
      )) %>%
      tidyr::unnest(cols = data)

    # Take MEAN of these 10 repeats
    estimate_eth <- eth_converted %>%
      dplyr::group_by(antigen, Sample) %>%
      dplyr::summarise(dilution = mean(dilution) * s1_concentration,
                       mfi = mean(mfi))

    ##########################################################################################################
    #### MODEL RESULTS AND PLOTS
    ##########################################################################################################

    sc_fit <- eth_qa_sc %>%
      dplyr::mutate(.keep = "none", new_fit = purrr::map(data, ~ {
        fit_standard_curve(.x$mfi, .x$dilution, control)
      }))

    qa_converted <- dplyr::inner_join(sc_fit, eth_qa_sc) |>
      dplyr::rowwise() |>
      dplyr::mutate(.keep = "none", data = list(
        data |> dplyr::mutate(
          .keep = "none",
          Sample = .data$Sample,
          dilution = .data$dilution,
          mfi = .data$mfi,
          mfi_pred = convert_dilution_to_mfi(.data$dilution, new_fit)
        )
      )) |>
      tidyr::unnest(cols = data)

    model_results <- qa_converted

    ##########################################################################################################
    #### MERGE DATA: Relabel Sample Names with Plate Layout
    ##########################################################################################################

    # Bind to location
    eth_converted_locations <- subset_data %>%
      dplyr::select(Location, Sample, Plate) %>%
      dplyr::right_join(estimate_eth, by = "Sample")

    results.location <- matrix(unlist(strsplit(as.character(eth_converted_locations$Location), ",")), ncol = 2, byrow = TRUE)[, 2]
    results.location <- substr(results.location, 1, nchar(results.location) - 1)
    eth_converted_locations <- cbind(Location.2 = results.location, eth_converted_locations)

    ## Matching SampleID from plate layout to corresponding sample.
    location.1 <- matrix(unlist(strsplit(subset_data$Location, ",")), ncol=2, byrow=T)[,2]
    location.1 <- substr(location.1, 1, nchar(location.1)-1)
    location.2 <- data.frame(Location.2=location.1, alpha=gsub("[[:digit:]]", "", location.1), numeric=gsub("[^[:digit:]]", "", location.1), SampleID=NA, stringsAsFactors = FALSE)
    for (i in location.2[, "Location.2"]){
      plate_layout_current <- layout[[plate_level]]
      names(plate_layout_current)[1] <- "Plate" # Relabel first column to be "Plate"
      location.2[location.2$Location.2==i, "SampleID"] <- plate_layout_current[
        plate_layout_current$Plate == unique(location.2[location.2$Location.2 == i, "alpha"]),
        colnames(plate_layout_current) == unique(location.2[location.2$Location.2 == i, "numeric"])
      ]
    }
    row_to_match <- location.2[,c("Location.2", "SampleID")]
    row_to_match <- row_to_match %>% dplyr::distinct(SampleID, Location.2, .keep_all = T) %>% na.omit()

    ## Using join() from plyr package to add SampleID information to results.df.wide. (default or given folder location and unique name)
    eth_converted_locations <- plyr::join(eth_converted_locations, row_to_match, by="Location.2", type="left")

    ## Move SampleID to first column
    eth_converted_locations <- eth_converted_locations[, c("SampleID", colnames(eth_converted_locations)[!(colnames(eth_converted_locations) %in% "SampleID")])]

    # Define column names to remain as characters
    character_columns <- c("SampleID", "Location", "Location.2", "Sample", "antigen", "Plate")

    # Convert specified columns to character
    eth_converted_locations[character_columns] <- lapply(eth_converted_locations[character_columns], as.character)

    # Convert all other columns (not in the specified list) to numeric
    numeric_columns <- setdiff(names(eth_converted_locations), character_columns)
    eth_converted_locations[numeric_columns] <- lapply(eth_converted_locations[numeric_columns], as.numeric)

    # Make long data frame wide
    eth_converted_locations_mfi <-eth_converted_locations %>%
      dplyr::select(-dilution) %>%
      tidyr::pivot_wider(names_from = "antigen", values_from = "mfi") %>%
      dplyr::rename_with(~paste0(.x, "_MFI"), -c(SampleID, Location.2, Location, Sample, Plate))
    eth_converted_locations_dilutions <- eth_converted_locations %>%
      dplyr::select(-mfi) %>%
      tidyr::pivot_wider(names_from = "antigen", values_from = "dilution") %>%
      dplyr::rename_with(~paste0(.x, "_Dilution"), -c(SampleID, Location.2, Location, Sample, Plate))
    eth_converted_wide <- eth_converted_locations_mfi %>%
      dplyr::left_join(eth_converted_locations_dilutions, by = c("SampleID", "Location.2", "Location", "Sample", "Plate"))

    ##########################################################################################################
    #### Create output dataframes
    ##########################################################################################################
    # Save just MFI and RAU for downstream analyses
    col_selection <- grepl("SampleID|Plate|_MFI|\\_Dilution$", colnames(eth_converted_wide))
    MFI_RAU_results <- eth_converted_wide[, col_selection]

    # Store results and models for current plate: `results_all` and `model_results_all` store all results and model plots for each plate.
    results_all[[plate_level]] <- eth_converted_wide
    model_results_all[[plate_level]] <- model_results
    MFI_RAU_results_all[[plate_level]] <- MFI_RAU_results

  }

  ##########################################################################################################
  #### Joining all plate data
  ##########################################################################################################

  counts_data <- counts_QC_output %>%
    ungroup() %>%
    dplyr::select(SampleID, Plate, QC_total)

  final_results <- dplyr::bind_rows(results_all) %>%
    inner_join(counts_data, by = c("SampleID", "Plate"))

  final_MFI_RAU_results <- dplyr::bind_rows(MFI_RAU_results_all) %>%
    inner_join(counts_data, by = c("SampleID", "Plate"))


  #############################################################################
  # Re-arrange data for final outputs
  #############################################################################

  # Get all base marker names by stripping _Count
  marker_bases <- names(final_results) %>%
    grep("_MFI$", ., value = TRUE) %>%
    sub("_MFI$", "", .)

  # Create the desired column order
  final_results_order <- c(
    "SampleID", "Location.2", "Location", "Sample", "Plate", "QC_total",
    unlist(lapply(marker_bases, function(x) c(paste0(x, "_MFI"), paste0(x, "_Dilution"))))
  )
  final_MFI_RAU_order <- c(
    "SampleID", "Plate", "QC_total",
    unlist(lapply(marker_bases, function(x) c(paste0(x, "_MFI"), paste0(x, "_Dilution"))))
  )

  # Reordered data frame
  final_results <- final_results %>%
    dplyr::select(all_of(final_results_order))

  final_MFI_RAU_results <- final_MFI_RAU_results %>%
    dplyr::select(all_of(final_MFI_RAU_order))

  return(list(final_results, final_MFI_RAU_results, model_results_all))
}
