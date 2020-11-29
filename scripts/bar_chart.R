chart_description <- "The graph below arranges the different majors in categories 
and then calculates their average salary.
It shows their wages from the hoghest to the lowest average pay"

salaries <- read.csv("data/recent-grads.csv")
library(dplyr)
library(ggplot2)
wages_by_major <- salaries %>%
  group_by(Major_category) %>%
  summarise(median_pay = mean(Median))
chart_plot <- ggplot(wages_by_major,
                     aes(x = reorder(Major_category, -median_pay),
                         y = median_pay)) +
  geom_bar(stat = "identity") + 
  labs(title = "Salary by major",
     x = "Major category",
     y = "Average pay"
  ) + coord_flip() + 
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank())