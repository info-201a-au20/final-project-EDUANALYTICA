# PIE CHART OF WOMEN IN STEM

# libraries
library(dplyr)
library(ggplot2)
library(lintr)
library(stringr)

# women in STEM data set
women_stem <- read.csv("data/women-stem.csv", stringsAsFactors = FALSE)

# total women in STEM
total_women <- women_stem %>%
  summarise(Women = sum(Women, na.rm = TRUE)) %>%
  pull(Women)

# total men in STEM
total_men <- women_stem %>%
  summarise(Men = sum(Men, na.rm = TRUE)) %>%
  pull(Men)

# total of both genders in STEM
total_both_genders <- women_stem %>%
  summarise(Total = sum(Total, na.rm = TRUE)) %>%
  pull(Total)

# proportions of both genders in STEM
proportions_genders <- data.frame(
  Gender = c("Women", "Men"),
  n = c(total_women, total_men),
  prop = c(total_women / total_both_genders, total_men / total_both_genders)
)

# Pie Chart of the proportion of men vs women in STEM
pie_menvswomen <- ggplot(proportions_genders, aes(x = factor(1), y = prop, fill = Gender)) +
  geom_bar(width = 3, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  labs(fill = "Genders",
       x = NULL,
       y = NULL,
       title = "Pie Chart of the proportion of men vs women in STEM")
pie_menvswomen

#Description of Pie Chart of the proportion of men vs women in STEM
desc_chart_menvswomen <- paste("this chart was intended to compare the total proportion of women vs the total proportion of men in STEM.")

# Pie chart of women in different major categories in STEM
pie_women_majors <- ggplot(women_stem, aes(x = factor(1), y = ShareWomen, fill = Major_category)) +
  geom_bar(width = 3, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  labs(fill = "Major Categories",
       x = NULL,
       y = NULL,
       title = "Pie Chart of women in different major categories in STEM")
pie_women_majors

#Description of Pie chart of women in different major categories in STEM
desc_chart_diff_majors <- paste("this chart was intended to display the proportion of women in different the major categories of STEM")
