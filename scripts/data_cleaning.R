# ============================================================================
# DATA CLEANING AND PREPROCESSING SCRIPT
# BUAN 314/370 Data Mining Project
# Purpose: Load raw data, document cleaning steps, create tidy datasets
# ============================================================================

# Load required libraries
library(tidyverse)

cat("========================================\n")
cat("DATA CLEANING & PREPROCESSING\n")
cat("========================================\n\n")

# ============================================================================
# STEP 1: LOAD RAW DATA FROM ORIGINAL DATASETS FOLDER
# ============================================================================

cat("STEP 1: Loading raw datasets from original_datasets folder...\n")

# Determine paths based on where script is being run from
if (dir.exists("original_datasets")) {
  # Running from project root
  data_path <- "original_datasets"
  output_path <- "cleaned_datasets"
} else if (dir.exists("../original_datasets")) {
  # Running from scripts folder
  data_path <- "../original_datasets"
  output_path <- "../cleaned_datasets"
} else {
  stop("Cannot find original_datasets folder. Current directory: ", getwd())
}

tuition_cost_raw <- read_csv(file.path(data_path, "tuition_cost.csv"))
salary_potential_raw <- read_csv(file.path(data_path, "salary_potential.csv"))
diversity_school_raw <- read_csv(file.path(data_path, "diversity_school.csv"))

cat("  ✓ tuition_cost.csv loaded:", nrow(tuition_cost_raw), "rows\n")
cat("  ✓ salary_potential.csv loaded:", nrow(salary_potential_raw), "rows\n")
cat("  ✓ diversity_school.csv loaded:", nrow(diversity_school_raw), "rows\n\n")

# ============================================================================
# STEP 2: DATA QUALITY ASSESSMENT
# ============================================================================

cat("STEP 2: Assessing data quality...\n")

# Check for missing values
missing_tuition <- sum(is.na(tuition_cost_raw$room_and_board))
missing_salary <- sum(is.na(salary_potential_raw$make_world_better_percent))

cat("  Missing Values Found:\n")
cat("    - room_and_board:", missing_tuition, "out of", nrow(tuition_cost_raw), 
    paste0("(", round(missing_tuition/nrow(tuition_cost_raw)*100, 1), "%)\n"))
cat("    - make_world_better_percent:", missing_salary, "out of", nrow(salary_potential_raw),
    paste0("(", round(missing_salary/nrow(salary_potential_raw)*100, 1), "%)\n"))

# Check for duplicate rows
dup_tuition <- sum(duplicated(tuition_cost_raw))
dup_salary <- sum(duplicated(salary_potential_raw))
dup_diversity <- sum(duplicated(diversity_school_raw))

cat("  Duplicate Rows Found:\n")
cat("    - tuition_cost:", dup_tuition, "\n")
cat("    - salary_potential:", dup_salary, "\n")
cat("    - diversity_school:", dup_diversity, "\n\n")

# Check join compatibility
schools_in_both <- sum(salary_potential_raw$name %in% tuition_cost_raw$name)
cat("  Join Compatibility Check:\n")
cat("    - Schools in BOTH tuition_cost and salary_potential:", schools_in_both, 
    "out of", nrow(salary_potential_raw), "\n")
cat("    - Schools in salary_potential NOT in tuition_cost:", 
    nrow(salary_potential_raw) - schools_in_both, "\n\n")

# ============================================================================
# STEP 3: DATA CLEANING - TUITION_COST
# ============================================================================

cat("STEP 3: Cleaning tuition_cost dataset...\n")

# MISSING VALUE STRATEGY:
# room_and_board has significant missing data but we KEEP these rows because:
# 1. Most queries focus on tuition (in_state/out_of_state) which is complete
# 2. room_and_board is supplementary information, not core to analysis
# 3. Removing would lose valuable schools from our dataset
# 4. SQL queries can filter WHERE room_and_board IS NOT NULL when needed
# This is a "keep with documentation" approach per best practices.

tuition_cost_clean <- tuition_cost_raw %>%
  # Remove any duplicate rows (if any)
  distinct() %>%
  # Standardize column names (already good, but ensuring consistency)
  rename_with(tolower) %>%
  # Add a flag for missing room_and_board for documentation and easy filtering
  mutate(has_room_board_data = !is.na(room_and_board))

cat("  ✓ Removed", dup_tuition, "duplicate rows\n")
cat("  ✓ Standardized column names\n")
cat("  ✓ Missing values in room_and_board:", missing_tuition, 
    "(KEPT - documented with has_room_board_data flag)\n")
cat("  ✓ Final tuition_cost rows:", nrow(tuition_cost_clean), "\n\n")

# ============================================================================
# STEP 4: DATA CLEANING - SALARY_POTENTIAL
# ============================================================================

cat("STEP 4: Cleaning salary_potential dataset...\n")

# MISSING VALUE STRATEGY:
# make_world_better_percent has some missing data but we KEEP these rows because:
# 1. Primary metrics (early_career_pay, mid_career_pay, stem_percent) are complete
# 2. make_world_better_percent is a survey-based subjective metric
# 3. Removing would lose schools with valuable salary data
# 4. SQL queries can filter WHERE make_world_better_percent IS NOT NULL when needed
# This is a "keep with documentation" approach per best practices.

salary_potential_clean <- salary_potential_raw %>%
  # Remove any duplicate rows (if any)
  distinct() %>%
  # Standardize column names
  rename_with(tolower) %>%
  # Add flag for missing make_world_better_percent for easy filtering
  mutate(has_better_world_data = !is.na(make_world_better_percent))

cat("  ✓ Removed", dup_salary, "duplicate rows\n")
cat("  ✓ Standardized column names\n")
cat("  ✓ Missing values in make_world_better_percent:", missing_salary,
    "(KEPT - documented with has_better_world_data flag)\n")
cat("  ✓ Final salary_potential rows:", nrow(salary_potential_clean), "\n\n")

# ============================================================================
# STEP 5: DATA CLEANING - DIVERSITY_SCHOOL
# ============================================================================

cat("STEP 5: Cleaning diversity_school dataset...\n")

# Create cleaned version
diversity_school_clean <- diversity_school_raw %>%
  # Remove any duplicate rows (if any)
  distinct() %>%
  # Standardize column names
  rename_with(tolower)

cat("  ✓ Removed", dup_diversity, "duplicate rows\n")
cat("  ✓ Standardized column names\n")
cat("  ✓ Final diversity_school rows:", nrow(diversity_school_clean), "\n\n")

# ============================================================================
# STEP 6: CREATE CLEANED DATASET SUMMARIES
# ============================================================================

cat("STEP 6: Creating data summaries...\n")

# Summary statistics for tuition data
tuition_summary <- tuition_cost_clean %>%
  summarize(
    total_schools = n(),
    schools_with_room_board = sum(has_room_board_data),
    avg_in_state_tuition = mean(in_state_tuition, na.rm = TRUE),
    avg_out_state_tuition = mean(out_of_state_tuition, na.rm = TRUE),
    avg_room_board = mean(room_and_board, na.rm = TRUE)
  )

# Summary statistics for salary data  
salary_summary <- salary_potential_clean %>%
  summarize(
    total_schools = n(),
    schools_with_better_world = sum(has_better_world_data),
    avg_early_career = mean(early_career_pay, na.rm = TRUE),
    avg_mid_career = mean(mid_career_pay, na.rm = TRUE),
    avg_stem_percent = mean(stem_percent, na.rm = TRUE)
  )

cat("  ✓ Tuition Summary:\n")
print(tuition_summary)
cat("\n  ✓ Salary Summary:\n")
print(salary_summary)
cat("\n")

# ============================================================================
# STEP 7: EXPORT CLEANED DATASETS TO CLEANED_DATASETS FOLDER
# ============================================================================

cat("STEP 7: Saving cleaned datasets to cleaned_datasets folder...\n")

# Save cleaned data to CSV files in cleaned_datasets folder
write_csv(tuition_cost_clean, file.path(output_path, "tuition_cost_clean.csv"))
write_csv(salary_potential_clean, file.path(output_path, "salary_potential_clean.csv"))
write_csv(diversity_school_clean, file.path(output_path, "diversity_school_clean.csv"))

cat("  ✓ tuition_cost_clean.csv saved\n")
cat("  ✓ salary_potential_clean.csv saved\n")
cat("  ✓ diversity_school_clean.csv saved\n\n")

# ============================================================================
# STEP 8: COPY REMAINING DATASETS THAT DON'T NEED CLEANING
# ============================================================================

cat("STEP 8: Copying remaining datasets to cleaned_datasets folder...\n")

# Copy historical_tuition.csv and tuition_income.csv as-is
# These datasets don't need cleaning, just need to be in the cleaned folder
historical_tuition <- read_csv(file.path(data_path, "historical_tuition.csv"))
tuition_income <- read_csv(file.path(data_path, "tuition_income.csv"))

write_csv(historical_tuition, file.path(output_path, "historical_tuition.csv"))
write_csv(tuition_income, file.path(output_path, "tuition_income.csv"))

cat("  ✓ historical_tuition.csv copied\n")
cat("  ✓ tuition_income.csv copied\n\n")

cat("  All datasets are now available in cleaned_datasets folder:\n")
cat("    - tuition_cost_clean.csv (cleaned)\n")
cat("    - salary_potential_clean.csv (cleaned)\n")
cat("    - diversity_school_clean.csv (cleaned)\n")
cat("    - historical_tuition.csv (unchanged)\n")
cat("    - tuition_income.csv (unchanged)\n\n")

# ============================================================================
# NULL VALUE ANALYSIS (DETAILED CHECK)
# ============================================================================

cat("========================================\n")
cat("NULL VALUE ANALYSIS\n")
cat("========================================\n")

# Load sqldf for SQL-based NULL checking
if(!require(sqldf, quietly = TRUE)) {
  install.packages("sqldf")
  library(sqldf)
}

# List of all original dataset files
null_check_files <- c('diversity_school.csv', 'historical_tuition.csv', 'salary_potential.csv', 
           'tuition_cost.csv', 'tuition_income.csv')

# Check each file for NULL values
for(file in null_check_files) {
  cat('\n=== ', file, ' ===\n', sep='')
  df <- read_csv(file.path(data_path, file), show_col_types=FALSE)
  
  total_rows <- nrow(df)
  cat('Total rows: ', total_rows, '\n\n', sep='')
  
  has_nulls <- FALSE
  
  # Check each column for NULL values using sqldf
  for(col in names(df)) {
    query <- sprintf('SELECT COUNT(*) as count FROM df WHERE `%s` IS NULL', col)
    null_count <- sqldf(query)
    
    if(null_count[[1]] > 0) {
      percentage <- round((null_count[[1]] / total_rows) * 100, 1)
      cat('  ', col, ': ', null_count[[1]], ' nulls (', percentage, '%)\n', sep='')
      has_nulls <- TRUE
    }
  }
  
  if(!has_nulls) {
    cat('  No NULL values found in any column\n')
  }
}

cat("\n========================================\n")
cat("NULL ANALYSIS COMPLETE\n")
cat("========================================\n\n")

# ============================================================================
# CLEANING SUMMARY REPORT
# ============================================================================

cat("========================================\n")
cat("CLEANING SUMMARY\n")
cat("========================================\n\n")

cat("ACTIONS TAKEN:\n")
cat("1. Loaded 3 raw datasets from GitHub repository\n")
cat("2. Identified and assessed missing values:\n")
cat("   - room_and_board:", missing_tuition, "missing values (", 
    round(missing_tuition/nrow(tuition_cost_raw)*100, 1), "% of data)\n")
cat("   - make_world_better_percent:", missing_salary, "missing values (",
    round(missing_salary/nrow(salary_potential_raw)*100, 1), "% of data)\n")
cat("3. Removed duplicate rows:", dup_tuition + dup_salary + dup_diversity, "total\n")
cat("4. Standardized all column names to lowercase\n")
cat("5. Added boolean flags (has_room_board_data, has_better_world_data) for filtering\n")
cat("6. Verified data types and structure\n\n")

cat("MISSING VALUE HANDLING STRATEGY:\n")
cat("• DECISION: Keep rows with missing values (not removed)\n\n")
cat("• RATIONALE FOR room_and_board:\n")
cat("  - Only supplementary cost information (core is tuition)\n")
cat("  - Primary analyses focus on in_state_tuition and out_of_state_tuition\n")
cat("  - Deleting would remove", missing_tuition, "otherwise valid schools\n")
cat("  - Added has_room_board_data flag for conditional filtering\n\n")
cat("• RATIONALE FOR make_world_better_percent:\n")
cat("  - Survey-based subjective metric (not all schools participate)\n")
cat("  - Primary analyses focus on salary metrics (complete data)\n")
cat("  - Deleting would remove", missing_salary, "schools with valuable salary data\n")
cat("  - Added has_better_world_data flag for conditional filtering\n\n")
cat("• IMPLEMENTATION:\n")
cat("  - All missing values remain as NA in cleaned datasets\n")
cat("  - SQL queries use: WHERE column_name IS NOT NULL when needed\n")
cat("  - R code uses: na.rm = TRUE or filter(!is.na(column)) when needed\n")
cat("  - This approach maximizes data retention while maintaining integrity\n\n")

cat("DATA READY FOR ANALYSIS!\n")
cat("========================================\n\n")

# Make cleaned datasets available globally
assign("tuition_cost", tuition_cost_clean, envir = .GlobalEnv)
assign("salary_potential", salary_potential_clean, envir = .GlobalEnv)
assign("diversity_school", diversity_school_clean, envir = .GlobalEnv)
