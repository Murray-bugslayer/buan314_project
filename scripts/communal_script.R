# ============================================================================
# COMMUNAL DATA MINING PROJECT SCRIPT
# ============================================================================
# Authors: Project Team
# Analysis Focus: Comprehensive university data analysis across multiple dimensions
# This script contains 6 SQL queries analyzing education costs, outcomes, and value
# ============================================================================

# Load required libraries
library(tidyverse)  
library(sqldf)      
library(scales)

# ============================================================================
# DATA LOADING - CLEANED DATA
# ============================================================================
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
# QUERY 1: LARGE ENROLLMENT SCHOOLS WITH BEST SALARY OUTCOMES
# ============================================================================
# Objective: Find schools with high enrollment that also deliver strong career outcomes
# Business Value: Identify scalable institutions that maintain quality

cat("\n=== QUERY 1: LARGE ENROLLMENT SCHOOLS WITH BEST SALARIES ===\n")

query1 <- "SELECT d.name,
                  d.state,
                  d.total_enrollment,
                  s.early_career_pay,
                  s.mid_career_pay,
                  ROUND((s.mid_career_pay - s.early_career_pay), 2) AS salary_growth,
                  t.type,
                  t.in_state_tuition
           FROM (SELECT name, state, MAX(total_enrollment) AS total_enrollment
                 FROM diversity_school
                 GROUP BY name, state) d
           INNER JOIN salary_potential s ON d.name = s.name
           INNER JOIN tuition_cost t ON d.name = t.name
           WHERE d.total_enrollment > 10000
             AND s.mid_career_pay IS NOT NULL
           ORDER BY s.mid_career_pay DESC
           LIMIT 10"

result1 <- sqldf(query1)
cat("Large schools analyzed:", nrow(result1), "\n")
cat("\nTop Large Enrollment Schools by Career Outcomes:\n")
print(result1)

# Save results
write_csv(result1, file.path(queries_path, "communal_large_enrollment_high_salary.csv"))


# ============================================================================
# QUERY 2: STATE EDUCATION AFFORDABILITY ANALYSIS
# ============================================================================
# Objective: Compare average tuition costs vs career earnings by state
# Business Value: Identify states offering best balance of affordability and earning potential

cat("\n=== QUERY 2: STATE EDUCATION AFFORDABILITY ===\n")

query2 <- "SELECT t.state,
                  COUNT(DISTINCT t.name) AS num_schools,
                  ROUND(AVG(t.in_state_tuition), 2) AS avg_tuition,
                  ROUND(AVG(s.early_career_pay), 2) AS avg_starting_salary,
                  ROUND(AVG(s.mid_career_pay), 2) AS avg_mid_career_salary,
                  ROUND(AVG(s.mid_career_pay) / AVG(t.in_state_tuition), 2) AS salary_to_tuition_ratio
           FROM tuition_cost t
           INNER JOIN salary_potential s 
              ON t.name = s.name
           WHERE t.in_state_tuition IS NOT NULL
             AND s.early_career_pay IS NOT NULL
             AND t.type = 'Public'
           GROUP BY t.state
           HAVING COUNT(DISTINCT t.name) >= 3
           ORDER BY salary_to_tuition_ratio DESC
           LIMIT 10"

result2 <- sqldf(query2)
cat("States analyzed:", nrow(result2), "\n")
cat("Total schools:", sum(result2$num_schools), "\n")
cat("\nTop 10 Best Value States:\n")
print(head(result2, 10))

# Save results
write_csv(result2, file.path(queries_path, "communal_state_education_affordability.csv"))


# ============================================================================
# QUERY 3: STEM EARNINGS PREMIUM BY SCHOOL TYPE
# ============================================================================
# Objective: Compare STEM vs non-STEM earnings at public vs private schools
# Business Value: Quantify return on investment differences for STEM education

cat("\n=== QUERY 3: STEM EARNINGS PREMIUM BY SCHOOL TYPE ===\n")

query3 <- "SELECT t.type,
                  s.stem_percent,
                  COUNT(*) AS num_schools,
                  ROUND(AVG(s.early_career_pay), 2) AS avg_early_pay,
                  ROUND(AVG(s.mid_career_pay), 2) AS avg_mid_pay,
                  ROUND(AVG(s.mid_career_pay - s.early_career_pay), 2) AS avg_pay_growth,
                  ROUND(AVG(t.out_of_state_tuition), 2) AS avg_tuition
           FROM tuition_cost t
           INNER JOIN salary_potential s
              ON t.name = s.name
           WHERE t.type IN ('Public', 'Private')
             AND s.stem_percent IS NOT NULL
             AND t.out_of_state_tuition IS NOT NULL
           GROUP BY t.type, s.stem_percent
           ORDER BY avg_mid_pay DESC
           LIMIT 10"

result3 <- sqldf(query3)
cat("School types analyzed:", length(unique(result3$type)), "\n")
cat("\nResults by School Type and STEM Focus:\n")
print(result3)

# Save results
write_csv(result3, file.path(queries_path, "communal_stem_earnings_premium_by_school_type.csv"))


# ============================================================================
# QUERY 4: TUITION PAYOFF TIMELINE ("MAKE IT BACK")
# ============================================================================
# Objective: Calculate years to recover 4-year tuition investment
# Business Value: Shows students how long it takes to "make back" tuition costs

cat("\n=== QUERY 4: TUITION PAYOFF TIMELINE ===\n")

query4 <- "SELECT t.name,
                  t.state,
                  t.type,
                  t.degree_length,
                  ROUND(t.out_of_state_tuition * 4, 2) AS total_4year_tuition,
                  s.early_career_pay,
                  s.mid_career_pay,
                  ROUND((t.out_of_state_tuition * 4) / s.early_career_pay, 2) AS years_to_payoff,
                  CASE 
                     WHEN (t.out_of_state_tuition * 4) / s.early_career_pay <= 2 THEN 'Excellent (<=2 years)'
                     WHEN (t.out_of_state_tuition * 4) / s.early_career_pay <= 4 THEN 'Good (2-4 years)'
                     WHEN (t.out_of_state_tuition * 4) / s.early_career_pay <= 6 THEN 'Fair (4-6 years)'
                     ELSE 'Poor (>6 years)'
                  END AS payoff_rating
           FROM tuition_cost t
           INNER JOIN salary_potential s
              ON t.name = s.name
           WHERE t.degree_length = '4 Year'
             AND t.out_of_state_tuition IS NOT NULL
             AND s.early_career_pay IS NOT NULL
             AND s.early_career_pay > 0
           ORDER BY years_to_payoff ASC
           LIMIT 10"

result4 <- sqldf(query4)
cat("Schools analyzed:", nrow(result4), "\n")
cat("\nBest Payoff Schools (Top 10):\n")
print(result4)

cat("\n\nPayoff Rating Distribution:\n")
payoff_summary <- table(result4$payoff_rating)
print(payoff_summary)

# Save results
write_csv(result4, file.path(queries_path, "communal_make_it_back_tuition_payoff_years.csv"))


# ============================================================================
# QUERY 5: HIGH EARNING LOW COST HIDDEN GEMS
# ============================================================================
# Objective: Find schools with below-average tuition but above-average earnings
# Business Value: Identify underrated schools offering exceptional ROI

cat("\n=== QUERY 5: HIGH EARNING LOW COST HIDDEN GEMS ===\n")

# First, show market averages for context
avg_stats <- sqldf("SELECT AVG(t.in_state_tuition) AS avg_tuition,
                           AVG(s.mid_career_pay) AS avg_salary
                    FROM tuition_cost t
                    INNER JOIN salary_potential s ON t.name = s.name
                    WHERE t.in_state_tuition IS NOT NULL
                      AND s.mid_career_pay IS NOT NULL")

cat("Market Averages:\n")
cat("Average In-State Tuition: $", round(avg_stats$avg_tuition, 2), "\n")
cat("Average Mid-Career Pay: $", round(avg_stats$avg_salary, 2), "\n\n")

# Find hidden gems
query5 <- "SELECT t.name,
                  t.state,
                  t.type,
                  t.degree_length,
                  t.in_state_tuition,
                  s.early_career_pay,
                  s.mid_career_pay,
                  ROUND(s.mid_career_pay - s.early_career_pay, 2) AS career_growth,
                  ROUND((s.mid_career_pay / t.in_state_tuition), 2) AS value_ratio
           FROM tuition_cost t
           INNER JOIN salary_potential s
              ON t.name = s.name
           WHERE t.in_state_tuition < (SELECT AVG(in_state_tuition) 
                                        FROM tuition_cost 
                                        WHERE in_state_tuition IS NOT NULL)
             AND s.mid_career_pay > (SELECT AVG(mid_career_pay)
                                     FROM salary_potential
                                     WHERE mid_career_pay IS NOT NULL)
             AND t.degree_length = '4 Year'
           ORDER BY value_ratio DESC
           LIMIT 10"

result5 <- sqldf(query5)
cat("Hidden Gem Schools Found:", nrow(result5), "\n")
cat("\nSchools with below-average tuition BUT above-average salaries:\n")
print(result5)

# Save results
write_csv(result5, file.path(queries_path, "communal_high_earning_low_cost_hidden_gems.csv"))


# ============================================================================
# QUERY 6: REGIONAL COST AND SALARY COMPARISON
# ============================================================================
# Objective: Group states into regions and compare education value metrics
# Business Value: Understand geographic patterns in education costs and career outcomes

cat("\n=== QUERY 6: REGIONAL COST AND SALARY COMPARISON ===\n")

query6 <- "SELECT 
                  CASE 
                     WHEN t.state IN ('ME', 'NH', 'VT', 'MA', 'RI', 'CT', 'NY', 'NJ', 'PA') 
                        THEN 'Northeast'
                     WHEN t.state IN ('OH', 'MI', 'IN', 'IL', 'WI', 'MN', 'IA', 'MO', 
                                      'ND', 'SD', 'NE', 'KS') 
                        THEN 'Midwest'
                     WHEN t.state IN ('DE', 'MD', 'DC', 'VA', 'WV', 'NC', 'SC', 'GA', 
                                      'FL', 'KY', 'TN', 'AL', 'MS', 'AR', 'LA', 'OK', 'TX') 
                        THEN 'South'
                     WHEN t.state IN ('MT', 'ID', 'WY', 'CO', 'NM', 'AZ', 'UT', 'NV', 
                                      'WA', 'OR', 'CA', 'AK', 'HI') 
                        THEN 'West'
                     ELSE 'Other'
                  END AS region,
                  COUNT(DISTINCT t.name) AS num_schools,
                  ROUND(AVG(t.in_state_tuition), 2) AS avg_instate_tuition,
                  ROUND(AVG(t.out_of_state_tuition), 2) AS avg_outstate_tuition,
                  ROUND(AVG(s.early_career_pay), 2) AS avg_starting_salary,
                  ROUND(AVG(s.mid_career_pay), 2) AS avg_mid_career_salary,
                  ROUND(AVG(s.mid_career_pay - s.early_career_pay), 2) AS avg_salary_growth,
                  ROUND(AVG(s.mid_career_pay) / AVG(t.in_state_tuition), 2) AS roi_multiplier
           FROM tuition_cost t
           INNER JOIN salary_potential s
              ON t.name = s.name
           WHERE t.in_state_tuition IS NOT NULL
             AND s.mid_career_pay IS NOT NULL
           GROUP BY region
           ORDER BY avg_mid_career_salary DESC"

result6 <- sqldf(query6)
cat("Regions analyzed:", nrow(result6), "\n")
cat("Total schools:", sum(result6$num_schools), "\n\n")
print(result6)

# Additional regional analysis: Public vs Private by Region
cat("\n\nPublic vs Private Comparison by Region:\n")

query6b <- "SELECT 
                   CASE 
                      WHEN t.state IN ('ME', 'NH', 'VT', 'MA', 'RI', 'CT', 'NY', 'NJ', 'PA') 
                         THEN 'Northeast'
                      WHEN t.state IN ('OH', 'MI', 'IN', 'IL', 'WI', 'MN', 'IA', 'MO', 
                                       'ND', 'SD', 'NE', 'KS') 
                         THEN 'Midwest'
                      WHEN t.state IN ('DE', 'MD', 'DC', 'VA', 'WV', 'NC', 'SC', 'GA', 
                                       'FL', 'KY', 'TN', 'AL', 'MS', 'AR', 'LA', 'OK', 'TX') 
                         THEN 'South'
                      WHEN t.state IN ('MT', 'ID', 'WY', 'CO', 'NM', 'AZ', 'UT', 'NV', 
                                       'WA', 'OR', 'CA', 'AK', 'HI') 
                         THEN 'West'
                      ELSE 'Other'
                   END AS region,
                   t.type,
                   COUNT(*) AS num_schools,
                   ROUND(AVG(t.in_state_tuition), 2) AS avg_tuition,
                   ROUND(AVG(s.mid_career_pay), 2) AS avg_salary
            FROM tuition_cost t
            INNER JOIN salary_potential s
               ON t.name = s.name
            WHERE t.type IN ('Public', 'Private')
              AND t.in_state_tuition IS NOT NULL
            GROUP BY region, t.type
            ORDER BY region, t.type"

result6b <- sqldf(query6b)
print(result6b)

# Save results
write_csv(result6, file.path(queries_path, "communal_regional_cost_salary_comparison.csv"))


# ============================================================================
# SUMMARY
# ============================================================================

cat("\n\n")
cat("========================================\n")
cat("COMMUNAL SCRIPT EXECUTION COMPLETE\n")
cat("========================================\n\n")

cat("QUERIES EXECUTED: 6\n")
cat("CSV OUTPUTS CREATED:\n")
cat("  1. communal_large_enrollment_high_salary.csv\n")
cat("  2. communal_state_education_affordability.csv\n")
cat("  3. communal_stem_earnings_premium_by_school_type.csv\n")
cat("  4. communal_make_it_back_tuition_payoff_years.csv\n")
cat("  5. communal_high_earning_low_cost_hidden_gems.csv\n")
cat("  6. communal_regional_cost_salary_comparison.csv\n\n")

cat("All results saved to:", queries_path, "/\n")
