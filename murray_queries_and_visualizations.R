install.packages("tidyverse", dependencies = TRUE)
install.packages("sqldf", dependencies = TRUE)
install.packages("scales", dependencies = TRUE)

library(tidyverse)  
library(sqldf)      
library(scales) 


tuition_cost <- read_csv("https://raw.githubusercontent.com/Murray-bugslayer/buan314_project/refs/heads/main/datasets/tuition_cost.csv")
salary_potential <- read_csv("https://raw.githubusercontent.com/Murray-bugslayer/buan314_project/refs/heads/main/datasets/salary_potential.csv")


# ============================================================================
# MURRAY'S SQL QUERY 1: Top 10 Most Expensive Universities
# ============================================================================

query1 <- "SELECT name, 
                  state, 
                  type, 
                  degree_length,
                  in_state_tuition, 
                  out_of_state_tuition
           FROM tuition_cost
           WHERE out_of_state_tuition IS NOT NULL
           ORDER BY out_of_state_tuition DESC
           LIMIT 10"

result1 <- sqldf(query1)
print(result1)

# Save results
write_csv(result1, "murray_query1_results.csv")



# ============================================================================
# MURRAY'S SQL QUERY 2: Average Starting Salary by State
# ============================================================================

query2 <- "SELECT state_name, 
                  ROUND(AVG(early_career_pay), 2) AS avg_early_pay,
                  ROUND(AVG(mid_career_pay), 2) AS avg_mid_pay,
                  COUNT(*) AS num_schools
           FROM salary_potential
           GROUP BY state_name
           HAVING COUNT(*) >= 3
           ORDER BY avg_early_pay DESC
           LIMIT 10"

result2 <- sqldf(query2)
print(result2)

# Save results
write_csv(result2, "murray_query2_results.csv")


# ============================================================================
# MURRAY'S SQL QUERY 3: ROI Analysis - Best Value Universities
# ============================================================================

query3 <- "SELECT t.name, 
                  t.state, 
                  t.type,
                  t.out_of_state_tuition,
                  s.early_career_pay,
                  (s.early_career_pay - (t.out_of_state_tuition * 4)) AS simple_roi,
                  ROUND((s.early_career_pay * 1.0) / (t.out_of_state_tuition * 4), 2) AS roi_ratio
           FROM tuition_cost t
           INNER JOIN salary_potential s 
              ON t.name = s.name
           WHERE t.out_of_state_tuition IS NOT NULL 
             AND s.early_career_pay IS NOT NULL
           ORDER BY simple_roi DESC
           LIMIT 15"

result3 <- sqldf(query3)
print(result3)

# Save results
write_csv(result3, "murray_query3_results.csv")


# ============================================================================
# PREPARE DATA FOR VISUALIZATIONS
# ============================================================================

tuition_salary <- inner_join(tuition_cost, salary_potential, by = "name")

# ============================================================================
# MURRAY'S VISUALIZATION 1: Scatter Plot
# ============================================================================

viz1 <- ggplot(tuition_salary, aes(x = out_of_state_tuition, y = early_career_pay)) +
  geom_point(aes(color = type), alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "darkred", size = 1.2, se = TRUE) +
  scale_x_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  scale_color_manual(values = c("Public" = "#2E86AB", 
                                "Private" = "#A23B72", 
                                "For Profit" = "#F18F01")) +
  labs(
    title = "Relationship Between Tuition and Starting Salary",
    subtitle = "Higher tuition shows moderate correlation with higher earnings",
    x = "Out-of-State Tuition (Annual)",
    y = "Early Career Pay (Annual)",
    color = "University Type",
    caption = "Source: College Tuition, Diversity, and Pay Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "bottom"
  )

print(viz1)
ggsave("murray_viz1_tuition_vs_salary.png", viz1, width = 10, height = 7, dpi = 300)


# ============================================================================
# MURRAY'S VISUALIZATION 2: Bar Chart
# ============================================================================

top_tuition_states <- tuition_cost %>%
  group_by(state) %>%
  summarize(avg_tuition = mean(out_of_state_tuition, na.rm = TRUE)) %>%
  arrange(desc(avg_tuition)) %>%
  head(10)

viz2 <- ggplot(top_tuition_states, aes(x = reorder(state, avg_tuition), y = avg_tuition)) +
  geom_col(fill = "#E63946", alpha = 0.8) +
  geom_text(aes(label = paste0("$", scales::comma(round(avg_tuition)))), 
            hjust = -0.1, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(
    labels = dollar_format(prefix = "$", big.mark = ","),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "States with Highest Average Out-of-State Tuition",
    subtitle = "Top 10 states ranked by average annual tuition cost",
    x = "State",
    y = "Average Out-of-State Tuition"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank()
  )

print(viz2)
ggsave("murray_viz2_tuition_by_state.png", viz2, width = 10, height = 8, dpi = 300)


# ============================================================================
# MURRAY'S VISUALIZATION 3 (SIMPLIFIED): Simple Bar Chart
# ============================================================================

# Calculate average salary by university type
avg_salary_by_type <- tuition_salary %>%
  group_by(type) %>%
  summarize(
    avg_salary = mean(early_career_pay, na.rm = TRUE),
    num_schools = n()
  ) %>%
  arrange(desc(avg_salary))

# Create a simple, clean bar chart
viz3_simple <- ggplot(avg_salary_by_type, aes(x = reorder(type, avg_salary), y = avg_salary, fill = type)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = paste0("$", scales::comma(round(avg_salary)))), 
            vjust = -0.5, size = 5, fontface = "bold") +
  geom_text(aes(label = paste0("(", num_schools, " schools)")), 
            vjust = 1.5, size = 3.5, color = "white") +
  scale_y_continuous(
    labels = dollar_format(prefix = "$", big.mark = ","),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_manual(values = c("Public" = "#06FFA5", 
                               "Private" = "#FFBE0B", 
                               "For Profit" = "#FF006E")) +
  labs(
    title = "Average Early Career Salary by University Type",
    subtitle = "Private universities lead in average graduate earnings",
    x = "University Type",
    y = "Average Early Career Pay",
    caption = "Source: College Tuition, Diversity, and Pay Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 11)
  )

print(viz3_simple)
ggsave("murray_viz3_salary_by_type_SIMPLE.png", viz3_simple, width = 10, height = 7, dpi = 300)
```
