library("dplyr")

grad_stu_df <- read.csv("data/all-ages.csv",
  stringsAsFactors = FALSE
)

summary_table <- grad_stu_df %>%
  group_by(Major_category) %>%
  summarise(
    avg_median = round(mean(Median, na.rm = TRUE), 0),
    avg_p25th = round(mean(P25th, na.rm = TRUE), 0),
    avg_p75th = round(mean(P75th, na.rm = TRUE), 0)
  ) %>%
  arrange(-avg_median)

displayed_table <- summary_table %>%
  rename(
    "Major Category" = Major_category,
    "Average Median Salary" = avg_median,
    "Average 25th Percentile Salary" = avg_p25th,
    "Average 75th Percentile Salary" = avg_p75th
  )
