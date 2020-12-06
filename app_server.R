library(dplyr)
library(ggplot2)
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

line_plot_data <- left_join(median_salary_recent_grad, median_salary_grad_students)

line_plot = ggplot(line_plot_data, aes(x = Major_category)) +
  geom_line(aes(y = medianRecent), color = "blue", linetype = "twodash") +
  geom_line(aes(y = medianGrad), color = "red", linetype = "twodash") +
  xlab("Major categories") +
  ylab("Salary median")

ggplot(economics, aes(x=date)) + 
  geom_line(aes(y = psavert), color = "darkred") + 
  geom_line(aes(y = uempmed), color="steelblue", linetype="twodash")






# MAJOR UNEMPLOYED vs EMPLOYED bar plot - NICOLE'S SECTION









# CONCLUSION TAKEAWAYS - IAN'S SECTION


my_server <- function(input, output){
  
}