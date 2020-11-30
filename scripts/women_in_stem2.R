library(dplyr)
library(ggplot2)
library(plotly)
library(lintr)
library(stringr)

women_stem <- read.csv("data/women-stem.csv", stringsAsFactors = FALSE)

women_by_majors <- women_stem %>% 
  select(Major_category, Total, Men, Women, ShareWomen) %>% 
  group_by(Major_category) %>% 
  summarise(
    TotalShareMen = sum(Men, na.rm = TRUE), # / sum(Total, na.rm = TRUE),
    TotalShareWomen = sum(Women, na.rm = TRUE), # / sum(Total, na.rm = TRUE),
    TotalGenders = TotalShareMen + TotalShareWomen
  )

gender <- c("Women", "Men")

pie_menvswomen <- ggplot(women_by_majors, 
                         mapping = aes(
                           x = factor(1), y = "", fill = factor(TotalShareWomen))) + 
  geom_bar(width = 1, stat = "identity", color = "white")