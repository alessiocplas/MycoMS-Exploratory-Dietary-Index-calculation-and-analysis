
# ============================================================
# 01_edi_calculation.R
# Calculate the Eating Diversity Index (EDI)
# ============================================================
#
# This script implements the original EDI structure:
#   1. (OPTIONAL) Convert raw questionnaire frequency and portion
#      responses into weekly food consumption.
#   2. Food-item scores are normalized within the study cohort.
#   3. Positive components are scored in the increasing direction.
#   4. Negative components are reverse-scored.
#   5. Food-item scores are averaged within 12 categories.
#   6. The overall EDI is the mean of available category scores,
#      rescaled to 0-100.
#
# CAUTION:
# Cohort min-max normalization makes scores dependent on the sample.
# It does not automatically represent compliance with dietary
# guidelines. For a guideline-based index, like the Healthy Eating Index (HEI), replace this scoring
# section with pre-specified, justified component thresholds.
# ============================================================


# ---- 0. Packages ----
required_packages <- c(
  "readr", "readxl", "dplyr", "janitor", "here", "tibble"
)

to_install <- required_packages[
  !vapply(required_packages, requireNamespace,
          logical(1), quietly = TRUE)
]

if (length(to_install) > 0) {
  stop(
    "Please install: ", paste(to_install, collapse = ", "),
    call. = FALSE
  )
}

library(dplyr)


# ============================================================
# OPTIONAL STEP: CALCULATE WEEKLY FOOD CONSUMPTION
# ============================================================
#
# Set calculate_weekly_consumption <- TRUE if your input data
# contain raw questionnaire responses.
#
# Set it to FALSE if you already have a dataset containing
# weekly food consumption values.
#
# The calculation is:
#
# Weekly consumption =
#   Frequency per week x Portion quantity per eating occasion
#
# IMPORTANT:
# The resulting unit depends on the portion variable:
#   - Number of portions -> portions/week
#   - Grams             -> g/week
#   - Millilitres       -> ml/week
#
# Do not multiply by an additional standard portion size unless
# the questionnaire portion response is only a multiplier and
# your protocol explicitly requires that additional conversion.
# ============================================================

calculate_weekly_consumption <- FALSE


# ---- 1. Frequency and portion conversion tables ----
#
# Frequency conversion:
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
# Portion conversion:
#
# Questionnaire response       Numeric quantity (g/ml)
# ------------------------------------------------
# One quarter                  0.25
# Half                         0.5
# One                          1
# Two                          2
# Three or more                Actual numeric quantity:
#                              3, 4, 5, etc.
#
# For responses of three or more, enter the actual numeric
# quantity whenever available. The script cannot determine
# whether "3 or more" means 3, 4, or 5 without that information.


frequency_lookup <- c(
  "No/Nvt" = 0,
  "<2x/month" = 0.5,
  "1x/week" = 1,
  "2-3x/week" = 2.5,
  "Almost daily" = 5,
  "Daily" = 7
)

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


# ---- 2. Optional raw data configuration ----
#
# If calculate_weekly_consumption = TRUE:
#
#   - Set raw_input_file to your raw questionnaire file.
#   - Set participant_id_column to your participant ID variable.
#   - Complete food_mapping with the exact column names of each
#     food's frequency and portion responses.
#
# Column names are cleaned using janitor::clean_names().
# Therefore, enter the cleaned column names in food_mapping.
#
# Example:
#
# food_mapping <- tibble::tribble(
#   ~food_item,          ~frequency_column,          ~portion_column,
#   "whole_grain_bread", "whole_grain_bread_freq",   "whole_grain_bread_portion",
#   "vegetables",        "vegetables_freq",          "vegetables_portion"
# )


raw_input_file <- here::here(
  "data", "raw", "raw_questionnaire.xlsx"
)

participant_id_column <- "participant_id"

food_mapping <- tibble::tibble(
  food_item = character(),
  frequency_column = character(),
  portion_column = character()
)


# ---- 3. Functions for response conversion ----

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
      "Unknown frequency response in column '", column_name, "': ",
      paste(unique(raw[unknown]), collapse = ", "),
      call. = FALSE
    )
  }

  if (any(result < 0 | result > 7, na.rm = TRUE)) {
    stop(
      "Frequency values in '", column_name,
      "' must be between 0 and 7 per week.",
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
      "Unknown portion response in column '", column_name, "': ",
      paste(unique(raw[unknown]), collapse = ", "),
      ". For quantities of 3 or more, provide the actual numeric value.",
      call. = FALSE
    )
  }

  if (any(result < 0, na.rm = TRUE)) {
    stop(
      "Portion quantities in '", column_name,
      "' cannot be negative.",
      call. = FALSE
    )
  }

  result
}


# ---- 4. Perform optional weekly calculation ----

if (calculate_weekly_consumption) {

  if (nrow(food_mapping) == 0) {
    stop(
      "Weekly calculation is activated, but food_mapping is empty. ",
      "Complete the food_mapping table in Section 2.",
      call. = FALSE
    )
  }

  if (!file.exists(raw_input_file)) {
    stop(
      "Raw questionnaire file not found: ", raw_input_file,
      call. = FALSE
    )
  }

  raw_data <- readxl::read_excel(raw_input_file) |>
    janitor::clean_names()

  if (!participant_id_column %in% names(raw_data)) {
    stop(
      "Participant ID column not found: ", participant_id_column,
      call. = FALSE
    )
  }

  required_mapping_columns <- c(
    "food_item", "frequency_column", "portion_column"
  )

  if (!all(required_mapping_columns %in% names(food_mapping))) {
    stop(
      "food_mapping must contain food_item, frequency_column, ",
      "and portion_column.",
      call. = FALSE
    )
  }

  required_raw_columns <- unique(c(
    participant_id_column,
    food_mapping$frequency_column,
    food_mapping$portion_column
  ))

  missing_columns <- setdiff(
    required_raw_columns,
    names(raw_data)
  )

  if (length(missing_columns) > 0) {
    stop(
      "These columns were not found in the raw questionnaire: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (any(as.character(raw_data[[participant_id_column]]) == "Opties",
          na.rm = TRUE)) {
    stop(
      "Rows labelled 'Opties' were detected. These may be questionnaire ",
      "option/codebook rows rather than participants. Remove them from ",
      "the raw participant dataset before proceeding.",
      call. = FALSE
    )
  }

  weekly_data <- raw_data |>
    select(all_of(participant_id_column))

  for (i in seq_len(nrow(food_mapping))) {

    food <- food_mapping$food_item[i]
    frequency_column <- food_mapping$frequency_column[i]
    portion_column <- food_mapping$portion_column[i]

    if (food %in% names(weekly_data)) {
      stop("Duplicate food item in food_mapping: ", food,
           call. = FALSE)
    }

    frequency_per_week <- convert_frequency(
      raw_data[[frequency_column]],
      frequency_column
    )

    portion_quantity <- convert_portion(
      raw_data[[portion_column]],
      portion_column
    )

    # Weekly consumption = frequency x portion quantity.
    #
    # If frequency is explicitly zero, weekly consumption is zero,
    # even if the portion response is missing.
    #
    # If frequency is positive and either response is missing,
    # weekly consumption remains NA rather than being set to zero.

    weekly_value <- ifelse(
      !is.na(frequency_per_week) & frequency_per_week == 0,
      0,
      ifelse(
        is.na(frequency_per_week) | is.na(portion_quantity),
        NA_real_,
        frequency_per_week * portion_quantity
      )
    )

    weekly_data[[food]] <- weekly_value
  }

  input_file <- here::here(
    "output", "weekly_consumption.csv"
  )

  dir.create(
    dirname(input_file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  readr::write_csv(
    weekly_data,
    input_file,
    na = ""
  )

  message(
    "Weekly consumption calculated and exported to: ",
    input_file
  )

} else {

  # If the optional calculation is not activated, the script
  # imports an already calculated weekly consumption dataset.

  input_file <- here::here(
    "output", "weekly_consumption.csv"
  )

}


# ============================================================
# END OF OPTIONAL WEEKLY CALCULATION
# ============================================================


# ---- 5. Import weekly consumption data ----

output_file <- here::here(
  "output", "edi_scores.csv"
)

if (!file.exists(input_file)) {
  stop(
    "Weekly dataset not found. Either activate the optional ",
    "weekly calculation step or provide output/weekly_consumption.csv.",
    call. = FALSE
  )
}

dat <- readr::read_csv(
  input_file,
  show_col_types = FALSE
) |>
  janitor::clean_names()

id_column <- "participant_id"

if (!id_column %in% names(dat)) {
  stop(
    "Participant ID column not found: ", id_column,
    call. = FALSE
  )
}


# ---- 6. Define food-item scoring direction ----
# Replace these example names with the exact cleaned variable names
# in your weekly dataset. Keep every food item in exactly one
# direction unless a scientifically justified exception is documented.
#
# Positive = higher intake receives a higher item score.
# Negative = higher intake receives a lower item score.

positive_items <- c(
  "whole_grain_bread",
  "cereals",
  "vegetables",
  "fruits",
  "milk",
  "plain_yogurt",
  "sliced_cheese",
  "chicken_fillet",
  "boiled_steamed_fish",
  "boiled_egg",
  "liquid_fat",
  "oil"
)

negative_items <- c(
  "white_bread",
  "white_pasta_rice",
  "dessert_cake",
  "sausages",
  "chocolate",
  "sugared_drinks",
  "ice_cream",
  "hard_fat",
  "soft_fat",
  "soy_sauce",
  "canned_packaged_soup",
  "beer",
  "wine",
  "liquor"
)


# ---- 7. Define the 12 EDI categories ----
# Check and edit names against the final data dictionary.

edi_categories <- list(
  whole_grains = c("whole_grain_bread", "cereals"),
  refined_grains = c("white_bread", "white_pasta_rice", "dessert_cake"),
  vegetables = c("vegetables"),
  fruits = c("fruits"),
  dairy = c("milk", "plain_yogurt", "sliced_cheese"),
  lean_protein = c("chicken_fillet", "boiled_steamed_fish", "boiled_egg"),
  processed_meat = c("sausages"),
  processed_sugars = c("chocolate", "sugared_drinks", "ice_cream"),
  fats_healthy = c("liquid_fat", "oil"),
  fats_unhealthy = c("hard_fat", "soft_fat"),
  sodium_rich = c("soy_sauce", "canned_packaged_soup"),
  alcohol = c("beer", "wine", "liquor")
)


# ---- 8. Validate configuration ----

all_scored_items <- unique(c(
  positive_items,
  negative_items
))

if (length(intersect(positive_items, negative_items)) > 0) {
  stop(
    "Some items are listed as both positive and negative.",
    call. = FALSE
  )
}

category_items <- unique(
  unlist(edi_categories, use.names = FALSE)
)

not_directioned <- setdiff(
  category_items,
  all_scored_items
)

not_categorized <- setdiff(
  all_scored_items,
  category_items
)

if (length(not_directioned) > 0) {
  stop(
    "Category items without scoring direction: ",
    paste(not_directioned, collapse = ", "),
    call. = FALSE
  )
}

if (length(not_categorized) > 0) {
  warning(
    "Scored items not assigned to a category: ",
    paste(not_categorized, collapse = ", ")
  )
}

missing_items <- setdiff(
  category_items,
  names(dat)
)

if (length(missing_items) > 0) {
  stop(
    "The following configured food variables are absent from the data: ",
    paste(missing_items, collapse = ", "),
    "\nEdit the item names and category mapping to match the data dictionary.",
    call. = FALSE
  )
}


# Convert declared food variables to numeric.

for (column in category_items) {

  converted <- suppressWarnings(
    as.numeric(dat[[column]])
  )

  if (any(!is.na(dat[[column]]) & is.na(converted))) {
    stop(
      "Non-numeric values found in: ", column,
      call. = FALSE
    )
  }

  if (any(converted < 0, na.rm = TRUE)) {
    stop(
      "Negative weekly consumption found in: ", column,
      call. = FALSE
    )
  }

  dat[[column]] <- converted
}


# ---- 9. Normalize food items to 0-1 within this cohort ----
# Positive item: minimum = 0, maximum = 1.
# Negative item: reverse the min-max score.
# Constant variables and all-missing variables become NA.

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

  if (xmax == xmin) {
    return(rep(NA_real_, length(x)))
  }

  score <- (x - xmin) / (xmax - xmin)

  if (direction == "negative") {
    score <- 1 - score
  }

  score
}

item_scores <- dat |>
  select(all_of(id_column))

for (item in positive_items) {
  if (item %in% category_items) {
    item_scores[[item]] <- score_item(
      dat[[item]], "positive"
    )
  }
}

for (item in negative_items) {
  if (item %in% category_items) {
    item_scores[[item]] <- score_item(
      dat[[item]], "negative"
    )
  }
}


# ---- 10. Calculate category scores (0-10) ----
# Require at least half of the category's items to be observed,
# rounded up. This reproduces the missingness rule from the
# original script; justify and sensitivity-test it in the paper.

category_scores <- tibble::tibble(
  participant_id = dat[[id_column]]
)

for (category in names(edi_categories)) {

  items <- edi_categories[[category]]

  score_matrix <- as.matrix(
    item_scores[, items, drop = FALSE]
  )

  observed_n <- rowSums(!is.na(score_matrix))
  required_n <- ceiling(length(items) / 2)

  score <- rowMeans(
    score_matrix,
    na.rm = TRUE
  ) * 10

  score[observed_n < required_n] <- NA_real_
  score[observed_n == 0] <- NA_real_

  category_scores[[category]] <- score
}


# ---- 11. Calculate total EDI (0-100) ----

category_matrix <- as.matrix(
  category_scores[, names(edi_categories), drop = FALSE]
)

available_categories <- rowSums(
  !is.na(category_matrix)
)

# Original rule: at least 80% of 12 categories must be available.
required_categories <- ceiling(
  0.80 * length(edi_categories)
)

edi <- rowMeans(
  category_matrix,
  na.rm = TRUE
) * 10

edi[available_categories < required_categories] <- NA_real_
edi[available_categories == 0] <- NA_real_

edi_results <- category_scores |>
  mutate(
    n_categories_available = available_categories,
    edi_score = edi
  )


# ---- 12. Export ----

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  edi_results,
  output_file,
  na = ""
)

message(
  "EDI results exported to: ", output_file
)
