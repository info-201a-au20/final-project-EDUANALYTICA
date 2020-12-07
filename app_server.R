library(dplyr)
library(ggplot2)
library(plotly)
# WIDGET NAMING CONVENTIONS CHARTTYPE_WIDGET_#

# INTRO - BRENDA'S SECTION








# WOMEN vs MEN pie chart - JERRY'S SECTION









# RECENTGRAD vs GRAD line chart - LEON'S SECTION
recent_grad <- read.csv("data/recent-grads.csv", stringsAsFactors = FALSE)
grad_students <- read.csv("data/grad-students.csv", stringsAsFactors = FALSE)

median_salary_recent_grad <- recent_grad %>% 
  select(Major, Major_category, Median) %>% 
  group_by(Major_category) %>% 
  summarise(
    medianRecent = mean(Median, na.rm = TRUE)
  )
median_salary_grad_students <- grad_students %>% 
  select(Major, Major_category, Grad_median) %>% 
  group_by(Major_category) %>% 
  summarise(
    medianGrad = mean(Grad_median, na.rm = TRUE)
  )

line_plot_data <- left_join(median_salary_recent_grad, 
                            median_salary_grad_students)

line_plot <- plot_ly(line_plot_data, 
                     x = ~Major_category, y = ~medianRecent, 
                     name = "Median Salary of Recent graduates",
                     type = "scatter", mode = 'lines',
                     line = list(color = 'rgb(205, 12, 24', width = 3))
line_plot <- line_plot %>%  
  add_trace(y = ~medianGrad, name = "Median Salary of Graduates",
            line = list(color = 'rgb(22, 96, 167)', width = 3, dash = 'dash'))
line_plot <- line_plot %>% 
  layout(title = " The Comparison of Median Salary between Recent grad and grad",
         xaxis = list(title = "Major Categories"),
         yaxis = list(title = "Median Salaries in dollar"))
line_plot

# implement UI: widget and side/main panel
# widget - sliding bar: y = salary median






# MAJOR UNEMPLOYED vs EMPLOYED bar plot - NICOLE'S SECTION









# CONCLUSION TAKEAWAYS - IAN'S SECTION


my_server <- function(input, output){
  
}