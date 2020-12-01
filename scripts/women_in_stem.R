# PIE CHART OF WOMEN IN STEM

# libraries
library(dplyr)
library(ggplot2)

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
pie_menvswomen <- ggplot(proportions_genders, aes(
  x = factor(1),
  y = prop, fill = Gender
)) +
  geom_bar(width = 3, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  labs(
    fill = "Genders",
    x = NULL,
    y = NULL,
    title = "Pie Chart of the proportion of men vs women in STEM"
  )


# Description of Pie Chart of the proportion of men vs women in STEM
desc_chart_menvswomen <- paste0(
  "This chart was intended to compare the total",
  " proportion of women vs the total proportion of men in STEM."
)

# Pie chart of women in different major categories in STEM
pie_women_majors <- ggplot(women_stem, aes(
  x = factor(1),
  y = ShareWomen,
  fill = Major_category
)) +
  geom_bar(width = 3, stat = "identity") +
  coord_polar(theta = "y") +
  labs(
    fill = "Major Categories",
    x = NULL,
    y = NULL,
    title = "Pie Chart of women in different major categories in STEM"
  )

# Description of Pie chart of women in different major categories in STEM
desc_chart_diff_majors <- paste0(
  "this chart was intended to display the",
  " proportion of women in different the major categories of STEM"
)


# the proportion of major categories by genders
# women_by_majors <- women_stem %>%
genders_by_majors <- women_stem %>%
  select(Major_category, Total, Men, Women, ShareWomen) %>%
  group_by(Major_category) %>%
  summarise(
    Men = (sum(Men, na.rm = TRUE) / sum(Total, na.rm = TRUE)) * 100,
    Women = (sum(Women, na.rm = TRUE) / sum(Total, na.rm = TRUE)) * 100,
  )

# rotate the dataframe `genders_by_majors`
majors_by_genders <- data.frame(t(genders_by_majors)) %>%
  rename(
    "Biology_LifeSciences" = "X1",
    "Computers_Mathematics" = "X2",
    "Engineering" = "X3",
    "Health" = "X4",
    "Physical_Sciences" = "X5"
  ) %>%
  slice(2:3) %>%
  mutate(Gender = c("Male" = "Male", "Female" = "Female"))

# pie chart: comparsion between males and females in Biology Life Sciences field
pie_biology <- ggplot(data = majors_by_genders,
                      aes(x = "", y = as.numeric(Biology_LifeSciences),
                          fill = Gender)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar(theta = "y") +
  labs(
    title = "Proportion of males and females in Biology Life Sciences field",
    x = "",
    y = ""
  )

# pie chart: comparsion between males and females in computers and math field
pie_computers <- ggplot(data = majors_by_genders, 
                        aes(x = "", y = as.numeric(Computers_Mathematics), 
                            fill = Gender)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar(theta = "y") +
  labs(
    title = "Proportion of males and females in Computers Mathematics field",
    x = "",
    y = ""
  )

# pie chart: comparison between males and females in engineering field
pie_engineering <- ggplot(data = majors_by_genders, 
                          aes(x = "", y = as.numeric(Engineering), 
                              fill = Gender)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar(theta = "y") +
  labs(
    title = "Proportion of males and females in Engineering field",
    x = "",
    y = ""
  )

# pie chart: comparison between males and females in Health field
pie_health <- ggplot(data = majors_by_genders, 
                     aes(x = "", y = as.numeric(Health), fill = Gender)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar(theta = "y") +
  labs(
    title = "Proportion of males and females in Health field",
    x = "",
    y = ""
  )

# pie chart: comparsion between males and females in Physical Sciences field
pie_physical <- ggplot(data = majors_by_genders, 
                       aes(x = "", y = as.numeric(Physical_Sciences), 
                           fill = Gender)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar(theta = "y") +
  labs(
    title = "Proportion of males and females in Physical Sciences field",
    x = "",
    y = ""
  )
