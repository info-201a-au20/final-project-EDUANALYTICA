salaries <- read.csv("../data/recent-grads.csv")
library(dplyr)
library(ggplot2)
wages_by_major <- salaries %>%
  group_by(Major_category) %>%
  summarise(their_pay = mean(Median))
chart_plot <- ggplot(wages_by_major,
                     aes(x = reorder(Major_category, -their_pay),
                         y = their_pay)) +
  geom_bar(stat = "identity")
print(chart_plot)
print(wages_by_major)
