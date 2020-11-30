library("dplyr")

# Read the data file
grad_stu_df <- read.csv("data/all-ages.csv",
  stringsAsFactors = FALSE
)

# create a function that reads a dataframe as an argument, and return the
# summary values of that dataframe. The return includes "Major Category"
# "Average Median Salary" "Average 25th Percentile Salary" "Average 75th
# Percentile Salary".
get_summary_table <- function(df) {
  return(
    df %>%
      group_by(Major_category) %>%
      summarise(
        avg_median = round(mean(Median, na.rm = TRUE), 0),
        avg_p25th = round(mean(P25th, na.rm = TRUE), 0),
        avg_p75th = round(mean(P75th, na.rm = TRUE), 0)
      ) %>%
      arrange(-avg_median) %>%
    rename(
      "Major Category" = Major_category,
      "Average Median Salary" = avg_median,
      "Average 25th Percentile Salary" = avg_p25th,
      "Average 75th Percentile Salary" = avg_p75th
    )
  )
}
