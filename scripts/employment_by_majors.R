# import libraries
library(dplyr)
library(ggplot2)
library(plotly)

# access and store data from all-age.csv
all_ages <- read.csv(
  file = "data/all-ages.csv",
  stringsAsFactors = FALSE
)

# group the data frame `all_ages` by `Major_category`
scatter_plot_data <- all_ages %>%
  group_by(Major_category) %>%
  summarize(
    employed = sum(Employed, na.rm = TRUE) / 1000,
    unemployed = sum(Unemployed, na.rm = TRUE) / 1000,
    median = Median / 1000
  )

# generate an interactive scatter plot according to `scatter_plot_data`
scatter_plot <- ggplot(data = scatter_plot_data) +
  geom_point(
    mapping = aes(
      x = employed, y = unemployed, color = Major_category,
      size = median
    )
  ) +
  labs(
    title = "The Post-Graduation Employment Staus by College Major Categories",
    subtitle = "",
    color = "Major Category",
    size = "Dot size is median salary(1=$1000)",
    x = "Number of unemployed students (1 = 1000 students)",
    y = "Number of employed students (1 = 1000 students)"
  )
scatter_plot <- ggplotly(scatter_plot)

# description of the scatter plot
desc_employment_by_majors <-
  paste0(
    "This chart was intended to visualize the difference between ",
    "employeed students and unemployeed by majors. ",
    "The radius of each dot is a representation of the median salary by majors."
  )
desc_employment_by_majors
