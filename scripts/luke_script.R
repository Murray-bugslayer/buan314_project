# ============================================================================
# LUKE'S DATA MINING PROJECT SCRIPT
# ============================================================================
# Analysis Focus: School Types and Degree Lengths
# ============================================================================

library(tidyverse)  
library(sqldf)      
library(scales)

# Load datasets
tuition_cost <- read_csv("https://raw.githubusercontent.com/Murray-bugslayer/buan314_project/refs/heads/main/datasets/tuition_cost.csv")
salary_potential <- read_csv("https://raw.githubusercontent.com/Murray-bugslayer/buan314_project/refs/heads/main/datasets/salary_potential.csv")


# ============================================================================
# LUKE'S SQL QUERY 1: Public vs Private Tuition Comparison
# ============================================================================

query1 <- "SELECT type,
                  COUNT(*) AS num_schools,
                  ROUND(AVG(in_state_tuition), 2) AS avg_in_state,
                  ROUND(AVG(out_of_state_tuition), 2) AS avg_out_state
           FROM tuition_cost
           WHERE type IN ('Public', 'Private')
             AND in_state_tuition IS NOT NULL
           GROUP BY type"

result1 <- sqldf(query1)
print(result1)

# Save results
write_csv(result1, "queries/luke_query1_results.csv")


# ============================================================================
# LUKE'S SQL QUERY 2: 2-Year vs 4-Year Schools
# ============================================================================

query2 <- "SELECT degree_length,
                  COUNT(*) AS num_schools,
                  ROUND(AVG(in_state_tuition), 2) AS avg_tuition
           FROM tuition_cost
           WHERE degree_length IN ('2 Year', '4 Year')
             AND in_state_tuition IS NOT NULL
           GROUP BY degree_length
           ORDER BY avg_tuition DESC"

result2 <- sqldf(query2)
print(result2)

# Save results
write_csv(result2, "queries/luke_query2_results.csv")


# ============================================================================
# LUKE'S VISUALIZATION 1: School Type Comparison
# ============================================================================

# Simple bar chart comparing public vs private tuition
type_comparison <- tuition_cost %>%
  filter(type %in% c("Public", "Private"), !is.na(in_state_tuition)) %>%
  group_by(type) %>%
  summarize(
    avg_tuition = mean(in_state_tuition, na.rm = TRUE),
    num_schools = n()
  )

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
    subtitle = "Private schools cost about five times more than in-state public institutions",
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
ggsave("visualizations/luke_viz1_school_type_comparison.png", viz1, width = 10, height = 7, dpi = 300, bg = "white")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Query results saved to queries/ folder\n")
cat("Visualization saved to visualizations/ folder\n")
