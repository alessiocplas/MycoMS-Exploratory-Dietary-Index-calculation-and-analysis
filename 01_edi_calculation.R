
# ============================================================
# 01_edi_calculation.R
# Calculate the Eating Diversity Index (EDI)
# ============================================================
#
# INPUT FILES:
# Tab-delimited .txt files with a header row and
# tab-separated columns.
#
# Run this script from the project's root directory.
#
# ============================================================
# GENERAL DESCRIPTION
# ============================================================
#
# The Eating Diversity Index (EDI) is calculated using
# food consumption data grouped into predefined dietary
# categories.
#
# The number and composition of EDI categories may differ
# between studies, depending on the questionnaire(s) used,
# the food items assessed, the study protocol and the
# underlying dietary rationale.
#
# For example, food items may originate from different
# dietary questionnaires or questionnaire sections.
# Researchers should therefore define and document how
# individual food items are assigned to the EDI categories.
#
# In the present study, we used 12 predefined EDI categories:
#
# 1.  whole_grains
# 2.  refined_grains
# 3.  vegetables
# 4.  fruits
# 5.  dairy
# 6.  lean_protein
# 7.  processed_meat
# 8.  added_sugars
# 9.  fats_healthy
# 10. fats_unhealthy
# 11. sodium_rich
# 12. alcohol
#
# The food items included in each category are specified
# in the category mapping below.
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
#    12 EDI categories defined for this study.
#
# IMPORTANT:
# Weekly consumption must be calculated and aggregated
# before proceeding to the EDI calculation.
#
# The current script imports the output of these steps:
# data/processed/weekly_consumption.txt
#
# The input must contain one row per participant and
# weekly consumption values for all 12 EDI categories.
#
# Units depend on the portion measurements used in the
# questionnaire (e.g. portions/week, g/week or ml/week).
#
# ============================================================
# PART B. EDI CALCULATION
# ============================================================
#
# 1. Import weekly consumption data.
# 2. Define and validate the 12 EDI categories.
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
# English equivalents are also included.

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
  "half" = 0.5,
  "one" = 1,
  "two" = 2,
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
# The food-item composition of each category is defined
# in PART B below.
#
# The current input file already contains weekly consumption
# aggregated into the 12 categories.
#
# If starting from individual food-item data, the weekly
# values must first be aggregated according to the mapping
# in PART B before running the EDI scoring procedure.
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
# The composition of EDI categories is study-specific.
# It may depend on the food items available in the
# questionnaire(s) used in a particular study.
#
# In the present study, the following 12 categories and
# food-item groupings were used.
#
# This mapping documents which individual food items
# contribute to each category.
#
# The same mapping should be applied consistently to all
# participants.
#
# Food items should not be counted in multiple categories
# unless this is explicitly justified by the methodology.

edi_components <- list(

  whole_grains = janitor::make_clean_names(c(
    "Bread_WholeGrain",
    "Cereals"
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


