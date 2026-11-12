
# ============================================================
# 01_edi_calculation.R
# Calculate the Eating Diversity Index (EDI)
# ============================================================
#
# Optimal input files are tab-delimited .txt files.
# Files must contain a header row and tab-separated columns.
#
# Run this script from the project's root directory.
#
# ============================================================
# PART A. WEEKLY FOOD CONSUMPTION
# ============================================================
#
# Weekly consumption is calculated in four steps:
#
# 1. Convert questionnaire frequency responses into
#    weekly frequencies.
#
# 2. Convert portion responses into numeric quantities.
#
# 3. Calculate weekly consumption for each individual food:
#
#    Weekly consumption =
#      Frequency per week x Portion quantity per occasion
#
# 4. Aggregate individual food consumption into the
#    following 12 EDI categories:
#
#    - whole_grains
#    - refined_grains
#    - vegetables
#    - fruits
#    - dairy
#    - lean_protein
#    - processed_meat
#    - added_sugars
#    - fats_healthy
#    - fats_unhealthy
#    - sodium_rich
#    - alcohol
#
# IMPORTANT:
# The weekly consumption values must be calculated and
# aggregated before proceeding to the EDI calculation.
#
# The current script imports the output of these steps:
# data/processed/weekly_consumption.txt
#
# The input must contain one row per participant and
# weekly consumption values for all 12 categories.
#
# Units depend on the portion measurements used in the
# questionnaire (e.g. portions/week, g/week or ml/week).
#
# ============================================================
# PART B. EDI CALCULATION
# ============================================================
#
# 1. Import weekly consumption data.
# 2. Validate the 12 EDI categories.
# 3. Convert category values to numeric.
# 4. Normalise category values within the study cohort.
# 5. Reverse-score negative categories.
# 6. Calculate category scores (0-10).
# 7. Calculate the overall EDI (0-10).
# 8. Export EDI results.
#
# CAUTION:
# Cohort min-max normalisation makes scores dependent on
# the study population. It does not represent fixed
# dietary-guideline adherence.
# ============================================================


# ---- 0. Dependencies ----

required_packages <- c("dplyr", "janitor")

to_install <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(to_install) > 0) {
  stop(
    "Please install: ",
    paste(to_install, collapse = ", "),
    call. = FALSE
  )
}

library(dplyr)


# ============================================================
# PART A. WEEKLY FOOD CONSUMPTION
# ============================================================


# ---- 1. Frequency conversion ----
#
# Questionnaire response       Weekly frequency
# ------------------------------------------------
# Niet/Nvt                     0
# <2x/maand                    0.5
# 1x/week                      1
# 2-3x/week                    2.5
# Bijna dagelijks              5
# Dagelijks                    7

frequency_lookup <- c(
  # ============================================================
  # 01_edi_calculation.R
  # Calculate the Eating Diversity Index (EDI)
  # ============================================================
  #
  # Optimal input files are tab-delimited .txt files.
  # Files must contain a header row and tab-separated columns.
  #
  # Run this script from the project's root directory.
  #
  # ============================================================
  # PART A. WEEKLY FOOD CONSUMPTION
  # ============================================================
  #
  # Weekly consumption is calculated in four steps:
  #
  # 1. Convert questionnaire frequency responses into
  #    weekly frequencies.
  #
  # 2. Convert portion responses into numeric quantities.
  #
  # 3. Calculate weekly consumption for each individual food:
  #
  #    Weekly consumption =
  #      Frequency per week x Portion quantity per occasion
  #
  # 4. Aggregate individual food consumption into the
  #    following 12 EDI categories:
  #
  #    - whole_grains
  #    - refined_grains
  #    - vegetables
  #    - fruits
  #    - dairy
  #    - lean_protein
  #    - processed_meat
  #    - added_sugars
  #    - fats_healthy
  #    - fats_unhealthy
  #    - sodium_rich
  #    - alcohol
  #
  # IMPORTANT:
  # The weekly consumption values must be calculated and
  # aggregated before proceeding to the EDI calculation.
  #
  # This script imports the output of these steps:
  # data/processed/weekly_consumption.txt
  #
  # The input must contain one row per participant and
  # weekly consumption values for all 12 categories.
  #
  # Units depend on the portion measurements used in the
  # questionnaire (e.g. portions/week, g/week or ml/week).
  #
  # ============================================================
  # PART B. EDI CALCULATION
  # ============================================================
  #
  # 1. Import weekly consumption data.
  # 2. Validate the 12 EDI categories.
  # 3. Convert category values to numeric.
  # 4. Normalise category values within the study cohort.
  # 5. Reverse-score negative categories.
  # 6. Calculate category scores (0-10).
  # 7. Calculate the overall EDI (0-10).
  # 8. Export EDI results.
  #
  # CAUTION:
  # Cohort min-max normalisation makes scores dependent on
  # the study population. It does not represent fixed
  # dietary-guideline adherence.
  #
  # ============================================================
  
  
  ############################################################
  # PART A. WEEKLY FOOD CONSUMPTION
  ############################################################
  
  
  # ---- 1. Frequency conversion -------------------------------
  #
  # Questionnaire response       Weekly frequency
  # ------------------------------------------------
  # No/Nvt                       0
  # <2x/month                    0.5
  # 1x/week                      1
  # 2-3x/week                    2.5
  # Almost daily                 5
  # Daily                        7
  #
  # English equivalents are included for flexibility.
  
  frequency_lookup <- c(
    "no/nvt" = 0,
    "<2x/month" = 0.5,
    "1x/week" = 1,
    "2-3x/week" = 2.5,
    "almost daily" = 5,
    "daily" = 7
  )
  
  
  # Convert frequency responses into weekly frequencies.
  
  convert_frequency <- function(x) {
    
    x <- tolower(trimws(as.character(x)))
    
    result <- unname(frequency_lookup[x])
    
    # Retain numeric values if already provided.
    numeric_values <- suppressWarnings(as.numeric(x))
    
    use_numeric <- is.na(result) & !is.na(numeric_values)
    
    result[use_numeric] <- numeric_values[use_numeric]
    
    result
  }
  
  
  # ---- 2. Portion conversion ---------------------------------
  #
  # Questionnaire response       Numeric quantity
  # ------------------------------------------------
  # One quarter                  0.25
  # Half                         0.5
  # One                          1
  # Two                          2
  # Three or more                Actual numeric quantity
  #
  # For quantities of 3 or more, enter the actual numeric
  # value whenever available.
  #
  # Do not automatically convert an ambiguous response such
  # as "3 or more" to 3 unless this is specified in the
  # questionnaire coding protocol.
  
  portion_lookup <- c(
    "one quarter" = 0.25,
    "een kwart" = 0.25,
    "half" = 0.5,
    "een halve" = 0.5,
    "one" = 1,
    "een" = 1,
    "two" = 2,
    "twee" = 2
  )
  
  
  # Convert portion responses into numeric quantities.
  
  convert_portion <- function(x) {
    
    x <- tolower(trimws(as.character(x)))
    
    result <- unname(portion_lookup[x])
    
    # Retain actual numeric quantities, including 3, 4, 5, etc.
    numeric_values <- suppressWarnings(as.numeric(x))
    
    use_numeric <- is.na(result) & !is.na(numeric_values)
    
    result[use_numeric] <- numeric_values[use_numeric]
    
    result
  }
  
  
  # ---- 3. Calculate weekly food consumption -----------------
  #
  # Formula:
  #
  # Weekly consumption =
  #   Frequency per week x Portion quantity per occasion
  #
  # If frequency is explicitly zero, weekly consumption
  # is zero even when portion quantity is missing.
  #
  # If frequency is greater than zero but portion quantity
  # is missing, weekly consumption remains missing.
  
  calculate_weekly <- function(frequency, portion) {
    
    result <- frequency * portion
    
    zero_frequency <- !is.na(frequency) & frequency == 0
    
    result[zero_frequency] <- 0
    
    result
  }
  
  
  # Example of how to use the conversion functions:
  #
  # frequency <- convert_frequency(
  #   c("1x/week", "Dagelijks", "Niet/Nvt")
  # )
  #
  # portion <- convert_portion(
  #   c("Two", "0.5", NA)
  # )
  #
  # weekly_consumption <- calculate_weekly(
  #   frequency,
  #   portion
  # )
  
  
  # ---- 4. Aggregate individual foods into 12 EDI categories -
  #
  # The category mapping is defined in PART B below.
  # The actual aggregation is performed after importing the
  # individual food-item weekly consumption data, if available.
  #
  # The input to the EDI calculation must contain the weekly
  # consumption totals for the 12 categories.
  #
  # ============================================================
  # PART B. EDI CALCULATION
  # ============================================================
  
  
  # ---- 1. Load required packages -----------------------------
  
  library(dplyr)
  library(janitor)
  
  
  # ---- 2. Import weekly consumption data --------------------
  #
  # The input file must contain:
  # - One row per participant.
  # - A participant identifier (patient_id).
  # - Optional metadata (e.g. status).
  # - Weekly consumption values for the 12 EDI categories.
  #
  # These values must represent weekly consumption, not
  # previously calculated EDI scores.
  
  input_file <- file.path(
    "data",
    "processed",
    "weekly_consumption.txt"
  )
  
  if (!file.exists(input_file)) {
    stop(
      "Input file not found: ",
      input_file,
      "\nPlease check the project directory and input filename."
    )
  }
  
  dat <- read.delim(
    input_file,
    header = TRUE,
    sep = "\t",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  dat <- janitor::clean_names(dat)
  
  
  # ---- 3. Validate participant identifier -------------------
  
  id_column <- "patient_id"
  
  if (!id_column %in% names(dat)) {
    stop(
      "The participant identifier column 'patient_id' ",
      "was not found in the input file."
    )
  }
  
  
  # Remove an exported row-number column if it is simply 1, 2, 3...
  
  if (
    "x" %in% names(dat) &&
    identical(
      as.character(dat$x),
      as.character(seq_len(nrow(dat)))
    )
  ) {
    dat <- dat %>%
      select(-x)
  }
  
  
  # Preserve participant identifier and optional metadata.
  
  metadata_columns <- id_column
  
  if ("status" %in% names(dat)) {
    metadata_columns <- c(metadata_columns, "status")
  }
  
  metadata <- dat %>%
    select(all_of(metadata_columns))
  
  
  # ---- 4. Define the 12 EDI categories -----------------------
  #
  # This is a required step in the EDI calculation.
  #
  # Individual food items must be assigned to one of the
  # 12 predefined EDI categories according to the study
  # protocol and the dietary rationale underlying the EDI.
  #
  # When starting from individual food-item consumption,
  # weekly values must be aggregated within these categories
  # before category scoring.
  #
  # The current input file already contains the aggregated
  # category-level weekly consumption values.
  #
  # The mapping below documents the original food-item
  # composition of each category and should be retained
  # to ensure methodological transparency.
  
  edi_components <- list(
    
    whole_grains = janitor::make_clean_names(c(
      "Bread_WholeGrain",
      "Muesli"
    )),
    
    refined_grains = janitor::make_clean_names(c(
      "Bread_White",
      "Dish_White_Pasta_Rice",
      "Dessert_Cake"
    )),
    
    vegetables = janitor::make_clean_names(c(
      "Veg_Cruciferous",
      "Veg_Other_Fresh_Frozen",
      "Veg_Raw"
    )),
    
    fruits = janitor::make_clean_names(c(
      "Fruit_Fresh",
      "Fruit_Stewed"
    )),
    
    dairy = janitor::make_clean_names(c(
      "Milk",
      "Yogurt_Plain",
      "Topping_Sliced_Cheese"
    )),
    
    lean_protein = janitor::make_clean_names(c(
      "Meat_Chicken_Fillet",
      "Fish_Boiled_Steamed",
      "Topping_Boiled_Egg"
    )),
    
    processed_meat = janitor::make_clean_names(c(
      "Meat_Curry_Sausage",
      "Meat_Sausage",
      "Meat_Other_Sausage"
    )),
    
    added_sugars = janitor::make_clean_names(c(
      "Snacks_Chocolate",
      "Sugared_Drink",
      "Dessert_IceCream"
    )),
    
    fats_healthy = janitor::make_clean_names(c(
      "Fat_Liquid",
      "Oil"
    )),
    
    fats_unhealthy = janitor::make_clean_names(c(
      "Fat_Hard",
      "Fat_Soft"
    )),
    
    sodium_rich = janitor::make_clean_names(c(
      "Dish_Soy_Sauce",
      "Soup_Can_Pack"
    )),
    
    alcohol = janitor::make_clean_names(c(
      "Beer",
      "Wine",
      "Liquor"
    ))
  )
  
  
  # Category names used in the EDI calculation.
  
  edi_categories <- names(edi_components)
  
  
  # ---- 5. Validate the 12 EDI category columns ---------------
  
  missing_categories <- setdiff(
    edi_categories,
    names(dat)
  )
  
  if (length(missing_categories) > 0) {
    stop(
      "The following EDI category columns are missing: ",
      paste(missing_categories, collapse = ", "),
      "\nThe input file must contain weekly consumption ",
      "values for all 12 EDI categories."
    )
  }
  
  
  # ---- 6. Convert category values to numeric -----------------
  
  dat[edi_categories] <- lapply(
    dat[edi_categories],
    function(x) {
      suppressWarnings(as.numeric(as.character(x)))
    }
  )
  
  
  # ---- 7. Define scoring direction ---------------------------
  #
  # Positive categories:
  # Higher consumption receives a higher score.
  #
  # Negative categories:
  # Higher consumption receives a lower score.
  #
  # The direction of each category should be justified
  # by the study's dietary rationale.
  
  positive_categories <- c(
    "whole_grains",
    "vegetables",
    "fruits",
    "dairy",
    "lean_protein",
    "fats_healthy"
  )
  
  negative_categories <- c(
    "refined_grains",
    "processed_meat",
    "added_sugars",
    "fats_unhealthy",
    "sodium_rich",
    "alcohol"
  )
  
  
  if (
    !setequal(
      c(positive_categories, negative_categories),
      edi_categories
    )
  ) {
    stop(
      "The positive and negative category definitions ",
      "do not match the 12 EDI categories."
    )
  }
  
  
  # ---- 8. Normalise category values within the study cohort --
  
  # Min-max normalisation:
  #
  # (Observed value - Minimum value) /
  # (Maximum value - Minimum value)
  #
  # Positive categories retain the normalised score.
  # Negative categories are reverse-scored:
  #
  # 1 - normalised score
  #
  # Categories with no variation are assigned NA because
  # min-max normalisation cannot be calculated.
  
  normalise_category <- function(x, direction) {
    
    if (all(is.na(x))) {
      return(rep(NA_real_, length(x)))
    }
    
    min_value <- min(x, na.rm = TRUE)
    max_value <- max(x, na.rm = TRUE)
    
    if (max_value == min_value) {
      return(rep(NA_real_, length(x)))
    }
    
    normalised <- (
      x - min_value
    ) / (
      max_value - min_value
    )
    
    if (direction == "negative") {
      normalised <- 1 - normalised
    }
    
    normalised
  }
  
  
  normalised_categories <- as.data.frame(
    lapply(
      edi_categories,
      function(category) {
        
        direction <- if (
          category %in% positive_categories
        ) {
          "positive"
        } else {
          "negative"
        }
        
        normalise_category(
          dat[[category]],
          direction
        )
      }
    )
  )
  
  names(normalised_categories) <- edi_categories
  
  
  # ---- 9. Calculate category scores (0-10) -------------------
  
  # Each normalised category score is multiplied by 10.
  # Category scores therefore range from 0 to 10.
  
  category_scores <- normalised_categories * 10
  
  
  # ---- 10. Calculate overall EDI score (0-10) ----------------
  #
  # The overall EDI is calculated as the mean of the
  # available category scores.
  #
  # At least 80% of the 12 categories must be available.
  # This corresponds to at least 10 categories.
  
  required_categories <- ceiling(
    0.80 * length(edi_categories)
  )
  
  category_matrix <- as.matrix(category_scores)
  
  available_categories <- rowSums(
    !is.na(category_matrix)
  )
  
  edi_score <- rowMeans(
    category_matrix,
    na.rm = TRUE
  )
  
  # Avoid NaN when all category scores are missing.
  
  edi_score[available_categories == 0] <- NA_real_
  
  # Apply the minimum category availability requirement.
  
  edi_score[
    available_categories < required_categories
  ] <- NA_real_
  
  # Ensure scores remain within the 0-10 range.
  
  edi_score <- pmax(
    0,
    pmin(10, edi_score)
  )
  
  
  # ---- 11. Prepare final EDI results -------------------------
  
  edi_results <- bind_cols(
    metadata,
    category_scores
  ) %>%
    mutate(
      edi_score = edi_score,
      n_categories_available = available_categories
    )
  
  
  # ---- 12. Export EDI results --------------------------------
  
  dir.create(
    "output",
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write.table(
    edi_results,
    file = file.path(
      "output",
      "edi_scores.txt"
    ),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE,
    na = ""
  )
  
  
  # ============================================================
  # PART C. QUALITY CONTROL
  # ============================================================
  
  
  # ---- 1. Number of participants -----------------------------
  
  cat(
    "\nNumber of participants:",
    nrow(edi_results),
    "\n"
  )
  
  
  # ---- 2. Summary of category scores -------------------------
  
  cat("\nSummary of EDI category scores:\n")
  
  print(
    summary(
      edi_results[edi_categories]
    )
  )
  
  
  # ---- 3. Summary of overall EDI score -----------------------
  
  cat("\nSummary of overall EDI score:\n")
  
  print(
    summary(edi_results$edi_score)
  )
  
  
  # ---- 4. Missing overall EDI scores -------------------------
  
  cat(
    "\nParticipants without an overall EDI score:",
    sum(is.na(edi_results$edi_score)),
    "\n"
  )
  
  
  # ---- 5. Available category counts --------------------------
  
  cat(
    "\nMinimum number of available categories required:",
    required_categories,
    "\n"
  )
  
  cat("\nDistribution of available categories:\n")
  
  print(
    table(edi_results$n_categories_available)
  )
  
  
  # ============================================================
  # END OF SCRIPT
  # ============================================================
  
  "niet/nvt" = 0,
  "no/nvt" = 0,
  "<2x/maand" = 0.5,
  "<2x/month" = 0.5,
  "1x/week" = 1,
  "2-3x/week" = 2.5,
  "bijna dagelijks" = 5,
  "almost daily" = 5,
  "dagelijks" = 7,
  "daily" = 7
)


# ---- 2. Portion conversion ----
#
# Questionnaire response       Numeric quantity
# ------------------------------------------------
# One quarter                  0.25
# Half                         0.5
# One                          1
# Two                          2
# Three or more                Actual numeric quantity
#
# For quantities of 3 or more, enter the actual numeric
# value whenever available.

portion_lookup <- c(
  "one quarter" = 0.25,
  "1/4" = 0.25,
  "half" = 0.5,
  "1/2" = 0.5,
  "one" = 1,
  "1" = 1,
  "two" = 2,
  "2" = 2
)


# ---- 3. Calculate weekly consumption for individual foods ----
#
# Formula:
#
# Weekly consumption =
#   Frequency per week x Portion quantity per occasion
#
# Missing values remain missing unless frequency is zero.
#
# If frequency is zero, weekly consumption is zero even
# when the portion response is missing.
#
# NOTE:
# The actual questionnaire column mapping must be supplied
# before this calculation can be run from raw questionnaire data.


convert_frequency <- function(x, column_name) {
  
  raw <- trimws(as.character(x))
  key <- tolower(raw)
  
  numeric_value <- suppressWarnings(as.numeric(raw))
  
  result <- rep(NA_real_, length(raw))
  
  is_numeric <- !is.na(numeric_value)
  result[is_numeric] <- numeric_value[is_numeric]
  
  is_label <- !is_numeric & !is.na(raw) & nzchar(raw)
  
  result[is_label] <- unname(
    frequency_lookup[key[is_label]]
  )
  
  unknown <- is_label & is.na(result)
  
  if (any(unknown)) {
    stop(
      "Unknown frequency response in column ",
      column_name, ": ",
      paste(unique(raw[unknown]), collapse = ", "),
      call. = FALSE
    )
  }
  
  if (any(result < 0 | result > 7, na.rm = TRUE)) {
    stop(
      "Frequency must be between 0 and 7 per week in: ",
      column_name,
      call. = FALSE
    )
  }
  
  result
}


convert_portion <- function(x, column_name) {
  
  raw <- trimws(as.character(x))
  key <- tolower(raw)
  
  numeric_value <- suppressWarnings(as.numeric(raw))
  
  result <- rep(NA_real_, length(raw))
  
  is_numeric <- !is.na(numeric_value)
  result[is_numeric] <- numeric_value[is_numeric]
  
  is_label <- !is_numeric & !is.na(raw) & nzchar(raw)
  
  result[is_label] <- unname(
    portion_lookup[key[is_label]]
  )
  
  unknown <- is_label & is.na(result)
  
  if (any(unknown)) {
    stop(
      "Unknown portion response in column ",
      column_name, ": ",
      paste(unique(raw[unknown]), collapse = ", "),
      ". Enter the actual numeric quantity for values of 3 or more.",
      call. = FALSE
    )
  }
  
  if (any(result < 0, na.rm = TRUE)) {
    stop(
      "Portion quantity cannot be negative in: ",
      column_name,
      call. = FALSE
    )
  }
  
  result
}


calculate_weekly <- function(frequency, portion) {
  
  result <- frequency * portion
  
  # Explicitly reported zero frequency means zero consumption.
  result[!is.na(frequency) & frequency == 0] <- 0
  
  result
}


# ---- 4. Aggregate weekly food consumption into 12 categories ----
#
# The individual weekly food variables must be assigned to
# their appropriate EDI categories.
#
# Aggregation should follow the study's predefined food-item
# classification and should not include overlapping aggregate
# variables more than once.
#
# The resulting dataset must contain:
#
# patient_id, status (if available), and the 12 weekly
# category-level consumption variables.
#
# This aggregation is performed in the weekly-consumption
# preparation step. The resulting file is imported below.


# ============================================================
# PART B. EDI CALCULATION
# ============================================================


# ---- 1. Import weekly category-level consumption data ----

weekly_input_file <- file.path(
  "data", "processed", "weekly_consumption.txt"
)

output_file <- file.path(
  "output", "edi_scores.txt"
)

if (!file.exists(weekly_input_file)) {
  stop(
    "Weekly consumption file not found: ",
    weekly_input_file,
    call. = FALSE
  )
}

dat <- read.delim(
  weekly_input_file,
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
) |>
  janitor::clean_names()


# ---- 2. Validate participant ID and metadata ----

id_column <- "patient_id"

if (!id_column %in% names(dat)) {
  stop(
    "ID column not found: ",
    id_column,
    "\nAvailable columns: ",
    paste(names(dat), collapse = ", "),
    call. = FALSE
  )
}

metadata_columns <- id_column

if ("status" %in% names(dat)) {
  metadata_columns <- c(metadata_columns, "status")
}

# Remove an automatically exported row-number column,
# if it contains exactly 1, 2, 3, ...
if (
  "x" %in% names(dat) &&
  identical(
    as.character(dat$x),
    as.character(seq_len(nrow(dat)))
  )
) {
  dat <- dat |> select(-x)
}


# ---- 3. Define the 12 EDI categories ----

edi_categories <- c(
  "whole_grains",
  "refined_grains",
  "vegetables",
  "fruits",
  "dairy",
  "lean_protein",
  "processed_meat",
  "added_sugars",
  "fats_healthy",
  "fats_unhealthy",
  "sodium_rich",
  "alcohol"
)

positive_categories <- c(
  "whole_grains",
  "vegetables",
  "fruits",
  "dairy",
  "lean_protein",
  "fats_healthy"
)

negative_categories <- c(
  "refined_grains",
  "processed_meat",
  "added_sugars",
  "fats_unhealthy",
  "sodium_rich",
  "alcohol"
)


# ---- 4. Validate category variables and scoring directions ----

missing_categories <- setdiff(
  edi_categories,
  names(dat)
)

if (length(missing_categories) > 0) {
  stop(
    "Missing EDI categories: ",
    paste(missing_categories, collapse = ", "),
    call. = FALSE
  )
}

if (
  length(intersect(
    positive_categories,
    negative_categories
  )) > 0 ||
  !setequal(
    c(positive_categories, negative_categories),
    edi_categories
  )
) {
  stop(
    "Every EDI category must have exactly one scoring direction.",
    call. = FALSE
  )
}


# ---- 5. Convert category variables to numeric ----

for (category in edi_categories) {
  
  original <- dat[[category]]
  
  converted <- suppressWarnings(
    as.numeric(as.character(original))
  )
  
  if (any(!is.na(original) & is.na(converted))) {
    stop(
      "Non-numeric values found in: ",
      category,
      call. = FALSE
    )
  }
  
  if (any(converted < 0, na.rm = TRUE)) {
    stop(
      "Negative weekly consumption found in: ",
      category,
      call. = FALSE
    )
  }
  
  dat[[category]] <- converted
}


# ---- 6. Normalise category values to 0-1 ----

score_item <- function(
    x,
    direction = c("positive", "negative")
) {
  
  direction <- match.arg(direction)
  
  observed <- x[!is.na(x)]
  
  if (length(observed) == 0) {
    return(rep(NA_real_, length(x)))
  }
  
  xmin <- min(observed)
  xmax <- max(observed)
  
  # Constant variables cannot distinguish participants.
  if (xmax == xmin) {
    return(rep(NA_real_, length(x)))
  }
  
  score <- (x - xmin) / (xmax - xmin)
  
  if (direction == "negative") {
    score <- 1 - score
  }
  
  score
}


# ---- 7. Calculate category scores (0-10) ----

category_scores <- dat |>
  select(all_of(metadata_columns))

for (category in positive_categories) {
  
  category_scores[[category]] <- score_item(
    dat[[category]],
    direction = "positive"
  ) * 10
}

for (category in negative_categories) {
  
  category_scores[[category]] <- score_item(
    dat[[category]],
    direction = "negative"
  ) * 10
}


# ---- 8. Calculate overall EDI (0-10) ----
#
# The EDI is the mean of available category scores.
#
# At least 80% of the 12 categories must be available,
# corresponding to at least 10 categories.

category_matrix <- as.matrix(
  category_scores[, edi_categories, drop = FALSE]
)

available_categories <- rowSums(
  !is.na(category_matrix)
)

required_categories <- ceiling(
  0.80 * length(edi_categories)
)

edi <- rowMeans(
  category_matrix,
  na.rm = TRUE
)

edi[available_categories == 0] <- NA_real_

edi[available_categories < required_categories] <- NA_real_

edi <- pmax(0, pmin(10, edi))


# ---- 9. Prepare final EDI results ----

edi_results <- category_scores |>
  mutate(
    n_categories_available = available_categories,
    edi_score = edi
  )


# ---- 10. Export EDI results ----

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

write.table(
  edi_results,
  file = output_file,
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  na = "NA"
)


# ---- 11. Quality-control summary ----

cat("\n--- EDI quality control ---\n")

cat("Participants:", nrow(edi_results), "\n")

cat(
  "Valid EDI scores:",
  sum(!is.na(edi_results$edi_score)),
  "\n"
)

cat(
  "Missing EDI scores:",
  sum(is.na(edi_results$edi_score)),
  "\n"
)

if (any(!is.na(edi_results$edi_score))) {
  
  cat(
    "Minimum EDI:",
    min(edi_results$edi_score, na.rm = TRUE),
    "\n"
  )
  
  cat(
    "Maximum EDI:",
    max(edi_results$edi_score, na.rm = TRUE),
    "\n"
  )
  
  cat(
    "Mean EDI:",
    mean(edi_results$edi_score, na.rm = TRUE),
    "\n"
  )
}

cat("\nSaved to:\n", output_file, "\n")
