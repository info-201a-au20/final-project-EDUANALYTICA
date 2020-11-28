salaries <- read.csv("../data/recent-grads.csv")
library(dplyr)
library(ggplot2)
wages_by_major <- salaries %>%
  group_by(Major_category) %>%
  summarise(their_pay = mean(Median)) %>%
  arrange(desc(their_pay))
chart_plot <- ggplot(wages_by_major, aes(x = Major_category, y = their_pay)) + geom_bar(stat = "identity")

