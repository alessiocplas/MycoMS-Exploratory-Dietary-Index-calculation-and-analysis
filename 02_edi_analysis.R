# ============================================================
# 02_edi_analysis.R
# Statistical analysis and visualization of EDI scores
# ============================================================

required_packages <- c("readr", "readxl", "dplyr", "ggplot2",
                       "ggpubr", "janitor", "here")
to_install <- required_packages[!vapply(required_packages, requireNamespace,
                                        logical(1), quietly = TRUE)]
if (length(to_install) > 0) {
  stop("Please install: ", paste(to_install, collapse = ", "),
       call. = FALSE)
}

library(dplyr)
library(ggplot2)

edi_file <- here::here("output", "edi_scores.csv")
metadata_file <- here::here("data", "metadata.xlsx")
figure_file <- here::here("output", "edi_by_group.pdf")

if (!file.exists(edi_file)) {
  stop("EDI results not found. Run scripts/01_edi_calculation.R first.",
       call. = FALSE)
}
if (!file.exists(metadata_file)) {
  stop("Metadata file not found: ", metadata_file, call. = FALSE)
}

edi <- readr::read_csv(edi_file, show_col_types = FALSE) |>
  janitor::clean_names()
metadata <- readxl::read_excel(metadata_file) |>
  janitor::clean_names()

# Adapt these names to the metadata file.
id_column <- "participant_id"
mycotype_column <- "mycotype"
status_column <- "status"

required_edi <- c(id_column, "edi_score")
required_meta <- c(id_column, mycotype_column, status_column)

if (!all(required_edi %in% names(edi))) {
  stop("Required EDI columns missing: ",
       paste(setdiff(required_edi, names(edi)), collapse = ", "))
}
if (!all(required_meta %in% names(metadata))) {
  stop("Required metadata columns missing: ",
       paste(setdiff(required_meta, names(metadata)), collapse = ", "))
}

if (anyDuplicated(edi[[id_column]]) > 0 ||
    anyDuplicated(metadata[[id_column]]) > 0) {
  stop("Duplicate participant IDs found; resolve before joining.")
}

analysis_data <- edi |>
  left_join(metadata, by = id_column)

# Descriptive summary
summary_table <- analysis_data |>
  group_by(.data[[mycotype_column]]) |>
  summarise(
    n = sum(!is.na(edi_score)),
    median_edi = median(edi_score, na.rm = TRUE),
    iqr_edi = IQR(edi_score, na.rm = TRUE),
    .groups = "drop"
  )
print(summary_table)

# Plot by mycotype
p_mycotype <- ggplot(
  analysis_data,
  aes(x = .data[[mycotype_column]], y = edi_score,
      fill = .data[[mycotype_column]])
) +
  geom_boxplot(width = 0.65, outlier.shape = NA, alpha = 0.75) +
  geom_jitter(width = 0.15, alpha = 0.65, size = 2) +
  labs(x = "Mycotype", y = "Eating Diversity Index (EDI; 0-100)") +
  theme_classic() +
  theme(legend.position = "none")

# Plot by clinical status
p_status <- ggplot(
  analysis_data,
  aes(x = .data[[status_column]], y = edi_score,
      fill = .data[[status_column]])
) +
  geom_boxplot(width = 0.65, outlier.shape = NA, alpha = 0.75) +
  geom_jitter(width = 0.15, alpha = 0.65, size = 2) +
  labs(x = "Status", y = "Eating Diversity Index (EDI; 0-100)") +
  theme_classic() +
  theme(legend.position = "none")

dir.create(dirname(figure_file), recursive = TRUE, showWarnings = FALSE)
pdf(figure_file, width = 8, height = 6)
print(p_mycotype)
print(p_status)
dev.off()

# Optional inferential tests:
# Check group sizes and distribution before selecting a test.
# Example for two independent groups:
# wilcox.test(edi_score ~ status, data = analysis_data)

message("Analysis figures saved to: ", figure_file)
