# Query and Visualization Ideas

This repository includes tuition, salary potential, diversity, and historical cost datasets. The ideas below pair those sources to support further exploration.

## Query Ideas (13)
1. **Top mid-career salaries vs. in-state tuition**: Rank schools by highest `mid_career_pay` (from `salary_potential.csv`) joined to `tuition_cost.csv` to show the ratio of mid-career pay to `in_state_total`.
2. **Early vs. mid-career pay growth**: Calculate percentage growth between `early_career_pay` and `mid_career_pay` and find the top and bottom 15 institutions to highlight salary acceleration.
3. **Salary vs. school type**: Compare average `early_career_pay` and `mid_career_pay` across `type` (Public, Private, For Profit) using `salary_potential.csv` joined with `tuition_cost.csv` on school name.
4. **Tuition premium for out-of-state students**: Compute average and median difference between `out_of_state_total` and `in_state_total` by `state` in `tuition_cost.csv` to find the largest premiums.
5. **Room and board impact**: Within `tuition_cost.csv`, calculate what percentage of `in_state_total` comes from `room_and_board` to identify campuses where housing dominates total cost.
6. **Income-level net cost variation**: From `tuition_income.csv`, compare mean `net_cost` by `income_lvl` buckets to see how affordability changes with income.
7. **Net cost vs. sticker price**: For each school in `tuition_income.csv`, compute the gap between `total_price` and average `net_cost` across income levels to surface campuses with the largest discounts.
8. **Historical tuition trends**: Using `historical_tuition.csv`, calculate CAGR for `tuition_cost` by `tuition_type` (All Constant, All Current, etc.) between 1985–86 and the latest year.
9. **State-level salary vs. tuition alignment**: Join `salary_potential.csv` and `tuition_cost.csv` by state to correlate median mid-career pay with median `in_state_total`, ranking states by best salary-to-cost ratio.
10. **STEM emphasis and salary**: In `salary_potential.csv`, correlate `stem_percent` with both `early_career_pay` and `mid_career_pay`; identify outliers where high STEM share does not translate to higher pay.
11. **Make-world-better sentiment vs. salary**: Analyze whether `make_world_better_percent` correlates with salary outcomes in `salary_potential.csv` and list the top 10 schools combining high sentiment and high pay.
12. **Diversity concentration by category**: Using `diversity_school.csv`, compute each category’s share of `total_enrollment` per school to find campuses with the highest representation for each demographic.
13. **Diversity and affordability intersection**: Join `diversity_school.csv` with `tuition_cost.csv` on school name to compare diversity percentages against `in_state_total`, highlighting the most diverse schools with below-median tuition.

## Visualization Ideas (6)
1. **Scatter: Salary vs. In-State Tuition** — X-axis `in_state_total`, Y-axis `mid_career_pay`, point color by `type` from the joined salary and tuition datasets.
2. **Slope chart: Early vs. Mid-Career Pay** — Connect `early_career_pay` to `mid_career_pay` for each school to show salary growth trajectory.
3. **Grouped bar: Tuition premium by state** — Bars for average `in_state_total` vs. `out_of_state_total` per `state` from `tuition_cost.csv` to visualize premiums.
4. **Stacked bar: Enrollment composition** — For `diversity_school.csv`, stack demographic categories per school (or per state) to show proportional diversity.
5. **Line chart: Historical tuition trends** — Lines for each `tuition_type` over `year` from `historical_tuition.csv`, optionally separating constant vs. current dollars.
6. **Boxplot: Net cost by income level** — Boxplots of `net_cost` grouped by `income_lvl` from `tuition_income.csv` to display affordability dispersion across income brackets.
