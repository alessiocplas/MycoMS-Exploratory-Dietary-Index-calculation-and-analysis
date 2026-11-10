# ============================================================
# 01_edi_calculation.R
# Calculate the Eating Diversity Index (EDI)
# ============================================================
#
# This script implements the original EDI structure:
#   1. Food-item scores are normalized within the study cohort.
#   2. Positive components are scored in the increasing direction.
#   3. Negative components are reverse-scored.
#   4. Food-item scores are averaged within 12 categories.
#   5. The overall EDI is the mean of available category scores,
#      rescaled to 0-100.
#
# CAUTION:
# Cohort min-max normalization makes scores dependent on the sample.
# It does not automatically represent compliance with dietary
# guidelines. For a guideline-based EDI, replace this scoring
# section with pre-specified, justified component thresholds.
# Do not invent thresholds.
# ============================================================

# ---- 0. Packages ----
required_packages <- c("readr", "dplyr", "janitor", "here", "tibble")
to_install <- required_packages[!vapply(required_packages, requireNamespace,
                                        logical(1), quietly = TRUE)]
if (length(to_install) > 0) {
  stop("Please install: ", paste(to_install, collapse = ", "),
       call. = FALSE)
}

library(dplyr)

# ---- 1. Paths ----
input_file <- here::here("output", "weekly_consumption.csv")
output_file <- here::here("output", "edi_scores.csv")

if (!file.exists(input_file)) {
  stop("Weekly dataset not found. Run scripts/00_week_calculation.R first.",
       call. = FALSE)
}

dat <- readr::read_csv(input_file, show_col_types = FALSE) |>
  janitor::clean_names()

id_column <- "participant_id"
if (!id_column %in% names(dat)) {
  stop("Participant ID column not found: ", id_column, call. = FALSE)
}

# ---- 2. Define food-item scoring direction ----
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

# ---- 3. Define the 12 EDI categories ----
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

# ---- 4. Validate configuration ----
all_scored_items <- unique(c(positive_items, negative_items))
if (length(intersect(positive_items, negative_items)) > 0) {
  stop("Some items are listed as both positive and negative.",
       call. = FALSE)
}

category_items <- unique(unlist(edi_categories, use.names = FALSE))
not_directioned <- setdiff(category_items, all_scored_items)
not_categorized <- setdiff(all_scored_items, category_items)

if (length(not_directioned) > 0) {
  stop("Category items without scoring direction: ",
       paste(not_directioned, collapse = ", "), call. = FALSE)
}
if (length(not_categorized) > 0) {
  warning("Scored items not assigned to a category: ",
          paste(not_categorized, collapse = ", "))
}

missing_items <- setdiff(category_items, names(dat))
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
  converted <- suppressWarnings(as.numeric(dat[[column]]))
  if (any(!is.na(dat[[column]]) & is.na(converted))) {
    stop("Non-numeric values found in: ", column, call. = FALSE)
  }
  if (any(converted < 0, na.rm = TRUE)) {
    stop("Negative weekly consumption found in: ", column,
         call. = FALSE)
  }
  dat[[column]] <- converted
}

# ---- 5. Normalize food items to 0-1 within this cohort ----
# Positive item: minimum = 0, maximum = 1.
# Negative item: reverse the min-max score.
# Constant variables and all-missing variables become NA.
score_item <- function(x, direction = c("positive", "negative")) {
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
    item_scores[[item]] <- score_item(dat[[item]], "positive")
  }
}
for (item in negative_items) {
  if (item %in% category_items) {
    item_scores[[item]] <- score_item(dat[[item]], "negative")
  }
}

# ---- 6. Calculate category scores (0-10) ----
# Require at least half of the category's items to be observed,
# rounded up. This reproduces the missingness rule from the
# original script; justify and sensitivity-test it in the paper.
category_scores <- tibble::tibble(
  participant_id = dat[[id_column]]
)

for (category in names(edi_categories)) {
  items <- edi_categories[[category]]
  score_matrix <- as.matrix(item_scores[, items, drop = FALSE])
  observed_n <- rowSums(!is.na(score_matrix))
  required_n <- ceiling(length(items) / 2)

  score <- rowMeans(score_matrix, na.rm = TRUE) * 10
  score[observed_n < required_n] <- NA_real_
  score[observed_n == 0] <- NA_real_

  category_scores[[category]] <- score
}

# ---- 7. Calculate total EDI (0-100) ----
category_matrix <- as.matrix(
  category_scores[, names(edi_categories), drop = FALSE]
)
available_categories <- rowSums(!is.na(category_matrix))

# Original rule: at least 80% of 12 categories must be available.
required_categories <- ceiling(0.80 * length(edi_categories))

edi <- rowMeans(category_matrix, na.rm = TRUE) * 10
edi[available_categories < required_categories] <- NA_real_
edi[available_categories == 0] <- NA_real_

edi_results <- category_scores |>
  mutate(
    n_categories_available = available_categories,
    edi_score = edi
  )

# ---- 8. Export ----
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(edi_results, output_file, na = "")
message("EDI results exported to: ", output_file)
