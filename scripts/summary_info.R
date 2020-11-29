library(dplyr)

all_grad_data <- read.csv("data/all-ages.csv", stringsAsFactors = FALSE)
grad_student_data <- read.csv("data/grad-students.csv", 
                              stringsAsFactors = FALSE)

get_summary_info <- function(dataset_one, dataset_two) {
  ret <-list()
  # The major category with the highest unemployment rate including recent grads
  # grad students
  ret$highest_avg_unemployment_rate <- dataset_one %>%
    group_by(Major_category) %>%
    summarize(average_unemployment_rate = mean(Unemployment_rate,
      na.rm = TRUE
    )) %>%
    arrange(desc(average_unemployment_rate)) %>%
    filter(average_unemployment_rate == max(average_unemployment_rate,
      na.rm = TRUE
    )) %>%
    pull(Major_category)

  # The major category with the lowest unemployment rate including recent grads
  # and grad students
  ret$lowest_avg_unemployment_rate <- dataset_one %>%
    group_by(Major_category) %>%
    summarize(average_unemployment_rate = mean(Unemployment_rate,
      na.rm = TRUE
    )) %>%
    arrange(desc(average_unemployment_rate)) %>%
    filter(average_unemployment_rate == min(average_unemployment_rate,
      na.rm = TRUE
    )) %>%
    pull(Major_category)

  # The major category with the highest average median salary
  ret$highest_median_salary <- dataset_one %>%
    group_by(Major_category) %>%
    summarize(highest_median_salary = mean(Median, na.rm = TRUE)) %>%
    arrange(desc(highest_median_salary)) %>%
    filter(highest_median_salary == max(highest_median_salary,
      na.rm = TRUE
    )) %>%
    pull(Major_category)


  # The major category with the lowest median salary
  ret$lowest_median_salary <- dataset_one %>%
    group_by(Major_category) %>%
    summarize(lowest_median_salary = mean(Median, na.rm = TRUE)) %>%
    arrange(desc(lowest_median_salary)) %>%
    filter(lowest_median_salary == min(lowest_median_salary,
      na.rm = TRUE
    )) %>%
    pull(Major_category)


  # grad median salary vs non-grad median salary difference
  ret$grad_vs_nongrad <- dataset_two %>%
    group_by(Major_category) %>%
    summarize(
      avg_median_grad_salary = mean(Grad_median, na.rm = TRUE),
      avg_median_nongrad_salary = mean(Nongrad_median,
        na.rm = TRUE
      ),
      avg_diff_salary = avg_median_grad_salary -
        avg_median_nongrad_salary
    ) %>%
    summarize(avg_median_difference_salary = mean(avg_diff_salary,
                                                  na.rm = TRUE)) %>%
    pull(avg_median_difference_salary)
  
  return(ret)
}
