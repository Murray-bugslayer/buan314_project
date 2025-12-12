# ============================================================================
# LUKE'S DATA MINING PROJECT SCRIPT
# ============================================================================
# Author: Luke
# Analysis Focus: School Types, Degree Programs, and Career Outcomes
# ============================================================================

# Load required libraries
library(tidyverse)  
library(sqldf)      
library(scales)

# ============================================================================
# DATA LOADING - CLEANED DATA
# ============================================================================
# Load cleaned datasets from cleaned_datasets folder
# These datasets have been preprocessed by data_cleaning.R with:
# - Standardized lowercase column names
# - Missing value flags added
# - Duplicates removed
# - Data quality validated

# Determine paths based on where script is being run from
if (dir.exists("cleaned_datasets")) {
  data_path <- "cleaned_datasets"
  queries_path <- "query_results"
  viz_path <- "visualizations"
} else {
  data_path <- "../cleaned_datasets"
  queries_path <- "../query_results"
  viz_path <- "../visualizations"
}

tuition_cost <- read_csv(file.path(data_path, "tuition_cost_clean.csv"))
salary_potential <- read_csv(file.path(data_path, "salary_potential_clean.csv"))
diversity_school <- read_csv(file.path(data_path, "diversity_school_clean.csv"))

cat("\n=== DATA LOADED SUCCESSFULLY ===\n")
cat("Tuition Cost records:", nrow(tuition_cost), "\n")
cat("Salary Potential records:", nrow(salary_potential), "\n")
cat("Diversity records:", nrow(diversity_school), "\n\n")


# ============================================================================
# LUKE'S QUERY 1: Public vs Private Tuition Comparison
# ============================================================================
# Objective: Compare average tuition costs between public and private universities
# Business Value: Helps students understand cost differences between institution types

query1 <- "SELECT type,
                  COUNT(*) AS num_schools,
                  ROUND(AVG(in_state_tuition), 2) AS avg_in_state,
                  ROUND(AVG(out_of_state_tuition), 2) AS avg_out_state
           FROM tuition_cost
           WHERE type IN ('Public', 'Private')
             AND in_state_tuition IS NOT NULL
           GROUP BY type"

result1 <- sqldf(query1)
cat("\n=== QUERY 1: Public vs Private Tuition ===\n")
print(result1)

# Save results with new naming convention
write_csv(result1, file.path(queries_path, "luke_public_vs_private_tuition.csv"))


# ============================================================================
# LUKE'S QUERY 2: 2-Year vs 4-Year Program Costs
# ============================================================================
# Objective: Analyze cost differences between 2-year and 4-year degree programs
# Business Value: Highlights affordability of community colleges vs universities

query2 <- "SELECT degree_length,
                  COUNT(*) AS num_schools,
                  ROUND(AVG(in_state_tuition), 2) AS avg_in_state_tuition,
                  ROUND(AVG(out_of_state_tuition), 2) AS avg_out_state_tuition
           FROM tuition_cost
           WHERE degree_length IN ('2 Year', '4 Year')
             AND in_state_tuition IS NOT NULL
           GROUP BY degree_length
           ORDER BY avg_in_state_tuition DESC"

result2 <- sqldf(query2)
cat("\n=== QUERY 2: 2-Year vs 4-Year Programs ===\n")
print(result2)

# Save results
write_csv(result2, file.path(queries_path, "luke_degree_length_comparison.csv"))


# ============================================================================
# LUKE'S QUERY 3: Career Salary Growth Analysis
# ============================================================================
# Objective: Identify schools with highest salary growth from early to mid-career
# Business Value: Shows which schools provide best long-term career advancement

query3 <- "SELECT name,
                  state_name,
                  early_career_pay,
                  mid_career_pay,
                  ROUND((mid_career_pay - early_career_pay), 2) AS salary_increase,
                  ROUND(((mid_career_pay - early_career_pay) * 100.0 / early_career_pay), 2) AS growth_percentage
           FROM salary_potential
           WHERE early_career_pay IS NOT NULL
             AND mid_career_pay IS NOT NULL
           ORDER BY growth_percentage DESC
           LIMIT 20"

result3 <- sqldf(query3)
cat("\n=== QUERY 3: Top Career Salary Growth ===\n")
print(result3)

# Save results
write_csv(result3, file.path(queries_path, "luke_career_growth_analysis.csv"))


# ============================================================================
# LUKE'S QUERY 4: STEM Emphasis and Salary Correlation
# ============================================================================
# Objective: Examine relationship between STEM focus and career earnings
# Business Value: Helps students understand value of STEM education

query4 <- "SELECT name,
                  state_name,
                  stem_percent,
                  early_career_pay,
                  mid_career_pay,
                  ROUND((early_career_pay + mid_career_pay) / 2, 2) AS avg_career_pay
           FROM salary_potential
           WHERE stem_percent IS NOT NULL
             AND early_career_pay IS NOT NULL
             AND mid_career_pay IS NOT NULL
           ORDER BY stem_percent DESC
           LIMIT 25"

result4 <- sqldf(query4)
cat("\n=== QUERY 4: STEM Emphasis vs Salary ===\n")
print(result4)

# Save results
write_csv(result4, file.path(queries_path, "luke_stem_salary_correlation.csv"))


# ============================================================================
# DATA PREPARATION FOR VISUALIZATIONS
# ============================================================================

cat("\n=== PREPARING VISUALIZATION DATA ===\n")

# Prepare data for visualization 1
type_comparison <- tuition_cost %>%
  filter(type %in% c("Public", "Private"), !is.na(in_state_tuition)) %>%
  group_by(type) %>%
  summarize(
    avg_tuition = mean(in_state_tuition, na.rm = TRUE),
    num_schools = n()
  )

# Prepare data for visualization 2
stem_salary_data <- salary_potential %>%
  filter(!is.na(stem_percent), !is.na(mid_career_pay)) %>%
  mutate(stem_category = case_when(
    stem_percent >= 75 ~ "Very High STEM (75-100%)",
    stem_percent >= 50 ~ "High STEM (50-74%)",
    stem_percent >= 25 ~ "Moderate STEM (25-49%)",
    TRUE ~ "Low STEM (<25%)"
  ))


# ============================================================================
# LUKE'S VISUALIZATION 1: Public vs Private Tuition
# ============================================================================
# Objective: Visual comparison of average tuition by institution type
# Chart Type: Bar chart with dual labels

cat("\n=== Creating Visualization 1: School Type Comparison ===\n")

viz1 <- ggplot(type_comparison, aes(x = type, y = avg_tuition, fill = type)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0("$", scales::comma(round(avg_tuition)))), 
            vjust = -1, size = 5, fontface = "bold") +
  geom_text(aes(label = paste0("(", num_schools, " schools)")), 
            vjust = 1.5, size = 3.5, color = "white") +
  scale_fill_manual(values = c("Public" = "#06FFA5", "Private" = "#FFBE0B")) +
  scale_y_continuous(
    labels = dollar_format(prefix = "$", big.mark = ","),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Average In-State Tuition: Public vs Private Universities",
    subtitle = "Private schools cost nearly three times more than public institutions",
    x = "School Type",
    y = "Average In-State Tuition",
    caption = "Source: College Tuition, Diversity, and Pay Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(size = 11)
  )

print(viz1)
ggsave(file.path(viz_path, "luke_public_vs_private_tuition.png"), viz1, width = 10, height = 7, dpi = 300, bg = "white")


# ============================================================================
# LUKE'S VISUALIZATION 2: STEM Emphasis vs Mid-Career Salary
# ============================================================================
# Objective: Explore correlation between STEM focus and career earnings
# Chart Type: Scatter plot with trend line and categories

cat("\n=== Creating Visualization 2: STEM vs Salary ===\n")

viz2 <- ggplot(stem_salary_data, aes(x = stem_percent, y = mid_career_pay)) +
  geom_point(aes(color = stem_category), alpha = 0.5, size = 3) +
  geom_smooth(method = "lm", color = "black", size = 1.2, se = TRUE, alpha = 0.2) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  scale_color_manual(values = c(
    "Very High STEM (75-100%)" = "#06FFA5",
    "High STEM (50-74%)" = "#4ECDC4",
    "Moderate STEM (25-49%)" = "#FFBE0B",
    "Low STEM (<25%)" = "#FF006E"
  )) +
  labs(
    title = "STEM Emphasis and Mid-Career Salary Outcomes",
    subtitle = "Higher STEM percentages show moderate positive correlation with earnings",
    x = "STEM Enrollment Percentage",
    y = "Mid-Career Pay (Annual)",
    color = "STEM Category",
    caption = "Source: College Tuition, Diversity, and Pay Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(size = 11)
  )

print(viz2)
ggsave(file.path(viz_path, "luke_stem_salary_correlation.png"), viz2, width = 10, height = 7, dpi = 300, bg = "white")


# ============================================================================
# SUMMARY AND OUTPUT
# ============================================================================

# ============================================================================
# LUKE'S VISUALIZATION 3: Degree Length Cost Comparison
# ============================================================================
# Objective: Compare costs between 2-year and 4-year programs visually

degree_comparison <- tuition_cost %>%
  filter(!is.na(degree_length), 
         !is.na(out_of_state_tuition),
         degree_length %in% c("2 Year", "4 Year")) %>%
  group_by(degree_length) %>%
  summarize(
    avg_tuition = mean(out_of_state_tuition, na.rm = TRUE),
    count = n()
  )

viz3 <- ggplot(degree_comparison, aes(x = degree_length, y = avg_tuition, fill = degree_length)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("$", format(round(avg_tuition), big.mark = ","))), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("2 Year" = "#2E86AB", "4 Year" = "#A23B72")) +
  labs(
    title = "Average Out-of-State Tuition: 2-Year vs 4-Year Programs",
    subtitle = "4-year programs cost significantly more per year",
    x = "Degree Length",
    y = "Average Out-of-State Tuition",
    caption = "Note: This is annual tuition cost, not total program cost | Source: College Tuition, Diversity, and Pay Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(size = 11)
  )

print(viz3)
ggsave(file.path(viz_path, "luke_degree_length_comparison.png"), viz3, width = 10, height = 7, dpi = 300, bg = "white")


# ============================================================================
# LUKE'S VISUALIZATION 4: Career Growth by School Type
# ============================================================================
# Objective: Show salary progression from early to mid-career by school type

career_progress <- tuition_cost %>%
  inner_join(salary_potential, by = "name") %>%
  filter(!is.na(early_career_pay), !is.na(mid_career_pay), !is.na(type),
         type %in% c("Public", "Private")) %>%
  select(type, early_career_pay, mid_career_pay) %>%
  pivot_longer(cols = c(early_career_pay, mid_career_pay), 
               names_to = "career_stage", 
               values_to = "salary") %>%
  mutate(career_stage = ifelse(career_stage == "early_career_pay", "Early Career", "Mid Career"))

viz4 <- ggplot(career_progress, aes(x = career_stage, y = salary, fill = type)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~type, nrow = 1) +
  labs(
    title = "Career Salary Progression by School Type",
    subtitle = "Distribution of salaries at different career stages",
    x = "Career Stage",
    y = "Annual Salary",
    fill = "School Type",
    caption = "Source: College Tuition, Diversity, and Pay Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(size = 11)
  )

print(viz4)
ggsave(file.path(viz_path, "luke_career_growth_analysis.png"), viz4, width = 12, height = 7, dpi = 300, bg = "white")


cat("\n" , rep("=", 70), "\n", sep = "")
cat("ANALYSIS COMPLETE - LUKE'S PROJECT\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("QUERIES COMPLETED:\n")
cat("  1. luke_public_vs_private_tuition.csv\n")
cat("  2. luke_degree_length_comparison.csv\n")
cat("  3. luke_career_growth_analysis.csv\n")
cat("  4. luke_stem_salary_correlation.csv\n\n")

cat("VISUALIZATIONS COMPLETED:\n")
cat("  1. luke_public_vs_private_tuition.png\n")
cat("  2. luke_stem_salary_correlation.png\n")
cat("  3. luke_degree_length_comparison.png\n")
cat("  4. luke_career_growth_analysis.png\n\n")

cat("All files saved to their respective folders.\n")
cat(rep("=", 70), "\n", sep = "")
