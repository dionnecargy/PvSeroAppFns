
classify_final_results <- function(mfi_to_rau_output, algorithm_type, Sens_Spec, counts_output) {

  devtools::load_all()

  #############################################################################
  # Data wrangling
  #############################################################################

  rau_data <- mfi_to_rau_output[[2]]
  rau_data <- rau_data %>%
    dplyr::select(SampleID, Plate, ends_with("_Dilution")) %>%
    mutate(across(ends_with("_Dilution"), as.numeric)) %>%    # Convert only "_Dilution" columns to numeric
    rename_with(~ str_replace(., "_Dilution$", ""), ends_with("_Dilution")) # Remove the "_Dilution" suffix

  #############################################################################
  # Model-specific functions
  #############################################################################

  # Step 1. Reads in serostatus using the trained random forest
  # antibody_model # Model 1: All top 8
  # antibody_model_excLF016 # Model 2: w/o LF016

  # Step 2: Read in the random forest votes threshold values
  threshold_table <- if(algorithm_type == "antibody_model"){
    antibody_model_threshold
  } else if (algorithm_type == "antibody_model_excLF016"){
    antibody_model_excLF016_threshold
  } else {
    stop("Invalid model provided")
  }

  # Step 3: Determine random forest votes threshold based on the algorithm_type string
  threshold <- if (Sens_Spec == "maximised") {
    threshold_table %>% filter(sens_spec == "max_sens_spec") %>% pull(threshold)
  } else if (Sens_Spec == "85% sensitivity") {
    threshold_table %>% filter(sens_spec == "85_sens") %>% pull(threshold)
  } else if (Sens_Spec == "90% sensitivity") {
    threshold_table %>% filter(sens_spec == "90_sens") %>% pull(threshold)
  } else if (Sens_Spec == "95% sensitivity") {
    threshold_table %>% filter(sens_spec == "95_sens") %>% pull(threshold)
  } else if (Sens_Spec == "85% specificity") {
    threshold_table %>% filter(sens_spec == "85_spec") %>% pull(threshold)
  } else if (Sens_Spec == "90% specificity") {
    threshold_table %>% filter(sens_spec == "90_spec") %>% pull(threshold)
  } else if (Sens_Spec == "95% specificity") {
    threshold_table %>% filter(sens_spec == "95_spec") %>% pull(threshold)
  } else {
    stop("Invalid sensitivity/specificity type provided.")
  }

  # Step 4: Run the model
  # Retrieve the model based on the algorithm_type string
  model <- get(algorithm_type)

  #############################################################################
  # Model outputs
  #############################################################################

  # Classify rau_data using the specified model
  class_preds <- predict(model, new_data = rau_data)
  prob_preds <- predict(model, new_data = rau_data, type = "prob")
  # Binds predictions to rau_data
  results <- rau_data %>% bind_cols(class_preds, prob_preds)
  # Classify new (seropositive) / old (seronegative) based on selected threshold
  results <- results %>%
    mutate(pred_class_max = ifelse(.pred_new > threshold, "new", "old"),
           pred_class_max = as.factor(pred_class_max))
  # Final processing and renaming
  final_results <- results %>%
    dplyr::select(-c(.pred_class, .pred_new, .pred_old)) %>%
    mutate(pred_class_max = recode(pred_class_max, "new" = "seropositive", "old" = "seronegative"))

  #############################################################################
  # Return the table of prediction classes and QC pass/fail
  #############################################################################

  final_classification_qc <- counts_output %>%
    ungroup() %>%
    dplyr::select(SampleID, Plate, QC_total) %>%
    inner_join(final_results, by = c("SampleID", "Plate"))

  return(final_classification_qc)
}
