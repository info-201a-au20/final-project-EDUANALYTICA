library(dplyr)

all_grad_data <- read.csv("../data/all-ages.csv", stringsAsFactors = FALSE)
grad_student_data <- read.csv("../data/grad-students.csv", 
                              stringsAsFactors = FALSE)


get_summary_info <- function(dataset){
  # The major category with the highest unemployment rate including recent grads
  # grad students
  highest_average_unemployment_rate <- all_grad_data %>% 
    group_by(Major_category) %>% 
    summarize(average_unemployment_rate = mean(Unemployment_rate, 
                                              na.rm = TRUE)) %>%
    arrange(desc(average_unemployment_rate)) %>% 
    filter(average_unemployment_rate == max(average_unemployment_rate,
                                                    na.rm = TRUE)) %>% 
    pull(Major_category)

  #The major category with the lowest unemployment rate including recent grads
  # and grad students
  lowest_average_unemployment_rate <- all_grad_data %>% 
    group_by(Major_category) %>% 
    summarize(average_unemployment_rate = mean(Unemployment_rate, 
                                               na.rm = TRUE)) %>%
    arrange(desc(average_unemployment_rate)) %>% 
    filter(average_unemployment_rate == min(average_unemployment_rate,
                                            na.rm = TRUE)) %>% 
    pull(Major_category)

  # The major category with the highest average median salary
  highest_median_salary <- all_grad_data %>% 
    group_by(Major_category) %>% 
    summarize(highest_median_salary = mean(Median, na.rm = TRUE)) %>% 
    arrange(desc(highest_median_salary)) %>% 
    filter(highest_median_salary == max(highest_median_salary, 
                                        na.rm = TRUE)) %>% 
    pull(Major_category)


  #The major category with the lowest median salary
  lowest_median_salary <- all_grad_data %>% 
    group_by(Major_category) %>% 
    summarize(lowest_median_salary = mean(Median, na.rm = TRUE)) %>% 
    arrange(desc(lowest_median_salary)) %>% 
    filter(lowest_median_salary == min(lowest_median_salary, 
                                        na.rm = TRUE)) %>% 
    pull(Major_category)
  
  
  return(ret <- list("Lowest Average Unemployment Rate" = 
                lowest_average_unemployment_rate, 
              "Highest Average Unemployment Rate" = 
                highest_average_unemployment_rate,
              "Lowest Median Salary" = 
                lowest_median_salary,
              "Highest Median Salary" = 
                highest_median_salary))
  
}
summary_info <- get_summary_info(all_grad_data)
