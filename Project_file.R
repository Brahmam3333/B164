################################################################################
# analysis.R
# Full analysis pipeline for 7COM1079 group project
#  (columns: Buyer Gender, Color, Make,
# New Car, Purchase Date, Buyer Age, Discount, Sale Price)
#
# Saves:
# - cleaned_dataset.csv
# - summary_stats_by_gender.csv
# - saleprice_boxplot.png
# - saleprice_histogram.png
# - contingency_newcar_by_gender.csv
# - t_test_results.txt
# - analysis_session_info.txt
#
################################################################################

# --- 0. Setup ----------------------------------------------------------------

required_pkgs <- c("tidyverse", "janitor", "broom", "ggpubr")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if(length(new_pkgs) > 0) {
  install.packages(new_pkgs, repos = "https://cloud.r-project.org")
}

library(tidyverse)
library(janitor)
library(broom)
library(ggpubr)   

# Set input file name here 
data_file <- "Car Sales.csv"

# Set output filenames
cleaned_file <- "cleaned_dataset.csv"
summary_file <- "summary_stats_by_gender.csv"
boxplot_file <- "saleprice_boxplot.png"
hist_file <- "saleprice_histogram.png"
contingency_file <- "contingency_newcar_by_gender.csv"
t_test_output_file <- "t_test_results.txt"
session_info_file <- "analysis_session_info.txt"

# --- 1. Load data ------------------------------------------------------------
# Read CSV - make column names friendly
# Alternatively, using the readr package (more robust)
library(readr)
df_raw <- read_tsv(data_file, locale = locale(encoding = "UTF-16LE"))

names(df_raw)
str(df_raw)

# Tidy names (makes them lowercase, underscores etc.)
df <- df_raw %>%
  janitor::clean_names()  # columns become buyer_gender, color, make, new_car, purchase_date, buyer_age, discount, sale_price

# Quick check: show column names expected
expected_cols <- c("buyer_gender", "color", "make", "new_car", "purchase_date", "buyer_age", "discount", "sale_price")
missing_cols <- setdiff(expected_cols, names(df))
if(length(missing_cols) > 0) {
  warning("Expected columns missing from dataset: ", paste(missing_cols, collapse = ", "),
          "\nPlease check your CSV column headings. The script will continue and try best-effort.")
}

# --- 2. Data cleaning --------------------------------------------------------
# Keep only the columns we know; allow for some columns to be missing
df <- df %>% select(any_of(expected_cols))

# Trim whitespace in character columns
df <- df %>% mutate(across(where(is.character), ~trimws(.)))

# Standardise buyer_gender values: uppercase first letter and convert common variants
if("buyer_gender" %in% names(df)) {
  df$buyer_gender <- tolower(df$buyer_gender)
  df$buyer_gender <- case_when(
    df$buyer_gender %in% c("m", "male") ~ "Male",
    df$buyer_gender %in% c("f", "female") ~ "Female",
    df$buyer_gender %in% c("other", "non-binary", "nonbinary") ~ "Other",
    TRUE ~ stringr::str_to_title(df$buyer_gender)
  )
  df$buyer_gender <- factor(df$buyer_gender)
}

# Sale price: make numeric (remove currency symbols, commas)
if("sale_price" %in% names(df)) {
  df$sale_price <- gsub("£|\\$|,", "", as.character(df$sale_price))
  df$sale_price <- as.numeric(df$sale_price)
}

# buyer_age numeric
if("buyer_age" %in% names(df)) {
  df$buyer_age <- as.numeric(df$buyer_age)
}

# discount numeric (if percent sign or currency)
if("discount" %in% names(df)) {
  df$discount <- gsub("%|£|\\$|,", "", as.character(df$discount))
  df$discount <- as.numeric(df$discount)
}

# new_car: standardise to Yes/No
if("new_car" %in% names(df)) {
  df$new_car <- tolower(as.character(df$new_car))
  df$new_car <- case_when(
    df$new_car %in% c("yes","y","true","1") ~ "Yes",
    df$new_car %in% c("no","n","false","0") ~ "No",
    TRUE ~ stringr::str_to_title(df$new_car)
  )
  df$new_car <- factor(df$new_car)
}

# Remove rows with missing essential values: buyer_gender or sale_price
essential_cols <- c()
if("buyer_gender" %in% names(df)) essential_cols <- c(essential_cols, "buyer_gender")
if("sale_price" %in% names(df)) essential_cols <- c(essential_cols, "sale_price")

# Count rows before cleaning
rows_before <- nrow(df)

if(length(essential_cols) > 0) {
  df <- df %>% drop_na(any_of(essential_cols))
}

rows_after <- nrow(df)

message("Rows before cleaning: ", rows_before, "; rows after removing NA in essential columns: ", rows_after)

# Save cleaned dataset
write.csv(df, file = cleaned_file, row.names = FALSE)
message("Saved cleaned dataset to: ", cleaned_file)

# --- 3. Exploratory summary --------------------------------------------------
# Summary stats by gender (mean, sd, n, median)
if(all(c("buyer_gender", "sale_price") %in% names(df))) {
  summary_by_gender <- df %>%
    group_by(buyer_gender) %>%
    summarise(
      n = n(),
      mean_sale_price = mean(sale_price, na.rm = TRUE),
      median_sale_price = median(sale_price, na.rm = TRUE),
      sd_sale_price = sd(sale_price, na.rm = TRUE),
      min_sale_price = min(sale_price, na.rm = TRUE),
      max_sale_price = max(sale_price, na.rm = TRUE)
    ) %>%
    arrange(buyer_gender)
  
  write.csv(summary_by_gender, summary_file, row.names = FALSE)
  message("Saved summary stats by gender to: ", summary_file)
} else {
  warning("Cannot compute summary by gender: buyer_gender or sale_price missing.")
}

# --- 4. Visualisations -------------------------------------------------------
# Boxplot (Sale Price by Buyer Gender) - main plot required by the assignment.
if(all(c("buyer_gender", "sale_price") %in% names(df))) {
  p_box <- ggplot(df, aes(x = buyer_gender, y = sale_price)) +
    geom_boxplot(outlier.shape = 21) +
    labs(
      title = "Sale Price by Buyer Gender",
      x = "Buyer Gender",
      y = "Sale Price (units)"
    ) +
    theme_minimal(base_size = 14)
  
  ggsave(boxplot_file, plot = p_box, width = 8, height = 6, dpi = 300)
  message("Saved boxplot to: ", boxplot_file)
} else {
  warning("Cannot draw boxplot: buyer_gender or sale_price missing.")
}

# Histogram of Sale Price - supplementary plot
if("sale_price" %in% names(df)) {
  p_hist <- ggplot(df, aes(x = sale_price)) +
    geom_histogram(bins = 40, boundary = 0) +
    labs(
      title = "Distribution of Sale Price",
      x = "Sale Price (units)",
      y = "Count"
    ) +
    theme_minimal(base_size = 14)
  
  ggsave(hist_file, plot = p_hist, width = 8, height = 5, dpi = 300)
  message("Saved histogram to: ", hist_file)
}

# --- 5. Contingency table (example for proportions RQ) -----------------------
# For the assignment they want a contingency if comparing proportions.
# We create a contingency table for new_car by gender and save it.
if(all(c("buyer_gender", "new_car") %in% names(df))) {
  cont_table <- table(df$buyer_gender, df$new_car)
  cont_df <- as.data.frame.matrix(cont_table)
  cont_df <- tibble::rownames_to_column(cont_df, var = "buyer_gender")
  write.csv(cont_df, contingency_file, row.names = FALSE)
  message("Saved contingency table to: ", contingency_file)
}

# --- 6. Statistical testing --------------------------------------------------
# Research question chosen: difference in means between two groups (Male vs Female)
# Null H0: mean_sale_price_male == mean_sale_price_female
# Alternative H1: they differ (two-tailed)

sink(t_test_output_file)
cat("T-TEST ANALYSIS\n")
cat("================\n\n")
cat("Rows before cleaning: ", rows_before, "\n")
cat("Rows after cleaning: ", rows_after, "\n\n")

if(all(c("buyer_gender", "sale_price") %in% names(df))) {
  
  # Keep only Male and Female for clear 2-group comparison
  df_two <- df %>% filter(buyer_gender %in% c("Male", "Female"))
  cat("Counts by gender (Male/Female):\n")
  print(table(df_two$buyer_gender))
  cat("\nSummary statistics by gender:\n")
  print(summary_by_gender)
  cat("\n\n")
  
  # Check normality with Shapiro-Wilk per group (note: Shapiro is sensitive with big n; with n>5000 it will usually be significant)
  cat("Normality check (Shapiro-Wilk) per group - NOTE: large samples often give significant results.\n")
  shapiro_results <- df_two %>%
    group_by(buyer_gender) %>%
    summarise(
      n = n(),
      shapiro_p = ifelse(n <= 5000, list(shapiro.test(sale_price)$p.value), NA_real_)
    )
  print(shapiro_results)
  cat("\n(If group n > 5000 Shapiro test not performed due to size.)\n\n")
  
  # Check equality of variances: Levene's test (from car package) OR F-test as quick check
  # Use var.test (F-test) but it assumes normality roughly - we'll do both and then choose Welch if variances differ.
  male_prices <- df_two %>% filter(buyer_gender == "Male") %>% pull(sale_price)
  female_prices <- df_two %>% filter(buyer_gender == "Female") %>% pull(sale_price)
  
  cat("Variance (sd^2) and var test (F-test) result:\n")
  cat("Male variance: ", var(male_prices, na.rm = TRUE), "\n")
  cat("Female variance: ", var(female_prices, na.rm = TRUE), "\n")
  
  # Only run var.test if both groups have at least 2 obs
  if(length(male_prices) >= 2 && length(female_prices) >= 2) {
    ftest <- tryCatch(var.test(male_prices, female_prices), error = function(e) e)
    print(ftest)
  } else {
    cat("Not enough data for variance test.\n")
    ftest <- NULL
  }
  cat("\n")
  
  # Run t-test: use Welch (var.equal = FALSE) by default; we will also run classic t-test if variances appear equal.
  cat("Running Welch two-sample t-test (does not assume equal variances):\n")
  t_welch <- t.test(sale_price ~ buyer_gender, data = df_two, var.equal = FALSE)
  print(t_welch)
  cat("\nTidy results:\n")
  print(broom::tidy(t_welch))
  cat("\n")
  
  # If F-test indicates equal variances (p > 0.05), optionally run pooled t-test
  if(!inherits(ftest, "error") && !is.null(ftest)) {
    if(ftest$p.value > 0.05) {
      cat("F-test suggests equal variances (p > 0.05). Running pooled (Student's) t-test:\n")
      t_pooled <- t.test(sale_price ~ buyer_gender, data = df_two, var.equal = TRUE)
      print(t_pooled)
      cat("\nTidy pooled results:\n")
      print(broom::tidy(t_pooled))
      cat("\n")
    } else {
      cat("F-test suggests variances differ (p <= 0.05). Using Welch results above.\n\n")
    }
  } else {
    cat("Variance test was not performed or failed; using Welch t-test output above.\n\n")
  }
  
  # Effect size (Cohen's d) simple calculation
  cat("Effect size (Cohen's d) approximation:\n")
  m1 <- mean(male_prices, na.rm = TRUE)
  m2 <- mean(female_prices, na.rm = TRUE)
  sd1 <- sd(male_prices, na.rm = TRUE)
  sd2 <- sd(female_prices, na.rm = TRUE)
  n1 <- length(male_prices)
  n2 <- length(female_prices)
  pooled_sd <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1 + n2 - 2))
  cohens_d <- (m1 - m2) / pooled_sd
  cat("Cohen's d (pooled sd): ", round(cohens_d, 4), "\n\n")
  
  # Decision rule at alpha = 0.05
  alpha <- 0.05
  pval <- t_welch$p.value
  if(pval < alpha) {
    cat("Decision: p-value <", alpha, "=> reject H0. Evidence suggests a difference in mean sale price between genders.\n")
  } else {
    cat("Decision: p-value >=", alpha, "=> do not reject H0. No strong evidence of difference in mean sale price between genders.\n")
  }
  
} else {
  cat("Cannot perform t-test: buyer_gender or sale_price missing.\n")
}
sink()  # close sink to file
message("Saved test output to: ", t_test_output_file)

# --- 7. Save session info for reproducibility --------------------------------
writeLines(capture.output(sessionInfo()), con = session_info_file)
message("Saved session info to: ", session_info_file)

# --- End --------------------------------------------------------------------
message("Analysis finished. Files created in working directory:")
message("- ", cleaned_file)
message("- ", summary_file)
message("- ", boxplot_file)
message("- ", hist_file)
message("- ", contingency_file, " (if new_car and buyer_gender present)")
message("- ", t_test_output_file)
message("- ", session_info_file)

################################################################################
# End of analysis.R
################################################################################

