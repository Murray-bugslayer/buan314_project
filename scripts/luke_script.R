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
# DATA LOADING
# ============================================================================
# Load all relevant datasets from the repository

tuition_cost <- read_csv("https://raw.githubusercontent.com/Murray-bugslayer/buan314_project/refs/heads/main/datasets/tuition_cost.csv")
salary_potential <- read_csv("https://raw.githubusercontent.com/Murray-bugslayer/buan314_project/refs/heads/main/datasets/salary_potential.csv")
diversity_school <- read_csv("https://raw.githubusercontent.com/Murray-bugslayer/buan314_project/refs/heads/main/datasets/diversity_school.csv")

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
write_csv(result1, "queries/luke_public_vs_private_tuition.csv")


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
write_csv(result2, "queries/luke_degree_length_comparison.csv")


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
write_csv(result3, "queries/luke_career_growth_analysis.csv")


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
write_csv(result4, "queries/luke_stem_salary_correlation.csv")


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
    stem_percent >= 50 ~ "High STEM (≥50%)",
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
ggsave("visualizations/luke_school_type_comparison.png", viz1, width = 10, height = 7, dpi = 300, bg = "white")


# ============================================================================
# LUKE'S VISUALIZATION 2: STEM Emphasis vs Mid-Career Salary
# ============================================================================
# Objective: Explore correlation between STEM focus and career earnings
# Chart Type: Scatter plot with trend line and categories

cat("\n=== Creating Visualization 2: STEM vs Salary ===\n")

viz2 <- ggplot(stem_salary_data, aes(x = stem_percent, y = mid_career_pay)) +
  geom_point(aes(color = stem_category), alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", color = "#FF006E", size = 1.2, se = TRUE, alpha = 0.2) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  scale_color_manual(values = c(
    "High STEM (≥50%)" = "#06FFA5",
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
ggsave("visualizations/luke_stem_salary_correlation.png", viz2, width = 10, height = 7, dpi = 300, bg = "white")


# ============================================================================
# SUMMARY AND OUTPUT
# ============================================================================

cat("\n" , rep("=", 70), "\n", sep = "")
cat("ANALYSIS COMPLETE - LUKE'S PROJECT\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("QUERIES COMPLETED:\n")
cat("  1. luke_public_vs_private_tuition.csv\n")
cat("  2. luke_degree_length_comparison.csv\n")
cat("  3. luke_career_growth_analysis.csv\n")
cat("  4. luke_stem_salary_correlation.csv\n\n")

cat("VISUALIZATIONS COMPLETED:\n")
cat("  1. luke_school_type_comparison.png\n")
cat("  2. luke_stem_salary_correlation.png\n\n")

cat("All files saved to their respective folders.\n")
cat(rep("=", 70), "\n", sep = "")
