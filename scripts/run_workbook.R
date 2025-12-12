# ============================================================================
# RUN WORKBOOK - MASTER SCRIPT
# BUAN 314/370 Data Mining Project
# Purpose: Execute all analysis scripts in the correct order
# ============================================================================

cat("\n")
cat("========================================\n")
cat("BUAN 314/370 DATA MINING PROJECT\n")
cat("MASTER WORKBOOK EXECUTION\n")
cat("========================================\n\n")

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(sqldf)
  library(scales)
})

# ============================================================================
# STEP 1: DATA CLEANING
# ============================================================================

cat("========================================\n")
cat("STEP 1: DATA CLEANING & PREPROCESSING\n")
cat("========================================\n")

# Determine paths based on where script is being run from
if (dir.exists("original_datasets")) {
  # Running from project root
  original_data_path <- "original_datasets"
  cleaned_data_path <- "cleaned_datasets"
  scripts_path <- "scripts"
} else if (dir.exists("../original_datasets")) {
  # Running from scripts folder
  original_data_path <- "../original_datasets"
  cleaned_data_path <- "../cleaned_datasets"
  scripts_path <- "."
} else {
  cat("\nERROR: original_datasets folder not found!\n")
  cat("Looking in current directory and parent directory.\n")
  cat("Current working directory:", getwd(), "\n\n")
  stop("Missing original_datasets folder")
}

# Create cleaned_datasets folder if it doesn't exist
if (!dir.exists(cleaned_data_path)) {
  dir.create(cleaned_data_path)
  cat("✓ Created cleaned_datasets folder\n")
}

# Run data cleaning script
cat("\nRunning data_cleaning.R...\n")
cat("----------------------------------------\n")
source(file.path(scripts_path, "data_cleaning.R"))
cat("----------------------------------------\n")
cat("✓ Data cleaning completed!\n\n")

# ============================================================================
# STEP 2: RUN ALL ANALYSIS SCRIPTS
# ============================================================================

cat("========================================\n")
cat("STEP 2: RUNNING ANALYSIS SCRIPTS\n")
cat("========================================\n\n")

# Find all script files (files ending with _script.R)
script_files <- list.files(path = scripts_path, pattern = "_script\\.R$", full.names = FALSE)

if (length(script_files) == 0) {
  cat("⚠️  No script files found (looking for files ending in '_script.R')\n\n")
} else {
  cat("Found", length(script_files), "analysis script(s):\n")
  for (script in script_files) {
    cat("  -", script, "\n")
  }
  cat("\n")
  
  # Execute each script
  for (script in script_files) {
    cat("========================================\n")
    cat("RUNNING:", script, "\n")
    cat("========================================\n")
    
    tryCatch({
      source(file.path(scripts_path, script))
      cat("\n✓", script, "completed successfully!\n\n")
    }, error = function(e) {
      cat("\n✗ ERROR in", script, ":\n")
      cat("  ", conditionMessage(e), "\n\n")
    })
  }
}

# ============================================================================
# STEP 3: SUMMARY
# ============================================================================

cat("========================================\n")
cat("WORKBOOK EXECUTION SUMMARY\n")
cat("========================================\n\n")

# Check what files were created
queries_path <- ifelse(dir.exists("query_results"), "query_results", "../query_results")
viz_path <- ifelse(dir.exists("visualizations"), "visualizations", "../visualizations")

queries_created <- list.files(queries_path, pattern = "\\.csv$")
viz_created <- list.files(viz_path, pattern = "\\.png$")

cat("OUTPUTS GENERATED:\n\n")

cat("Cleaned Datasets:", length(list.files(cleaned_data_path, pattern = "\\.csv$")), "\n")
cat("  Location: cleaned_datasets/\n\n")

cat("SQL Query Results:", length(queries_created), "\n")
if (length(queries_created) > 0) {
  cat("  Location: query_results/\n")
  for (q in queries_created) {
    cat("    -", q, "\n")
  }
}
cat("\n")

cat("Visualizations:", length(viz_created), "\n")
if (length(viz_created) > 0) {
  cat("  Location: visualizations/\n")
  for (v in viz_created) {
    cat("    -", v, "\n")
  }
}
cat("\n")

cat("========================================\n")
cat("ALL ANALYSIS COMPLETE\n")
cat("========================================\n\n")