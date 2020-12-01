library(dplyr)
library(ggplot2)
library(plotly)
library(lintr)
library(stringr)

women_stem <- read.csv("../data/women-stem.csv", stringsAsFactors = FALSE)

women_by_majors <- women_stem %>% 
  select(Major_category, Total, Men, Women, ShareWomen) %>% 
  group_by(Major_category) %>% 
  summarise(
    TotalShareMen = sum(Men, na.rm = TRUE)/sum(Total, na.rm = TRUE), # / sum(Total, na.rm = TRUE),
    TotalShareWomen = sum(Women, na.rm = TRUE)/sum(Total, na.rm = TRUE), # / sum(Total, na.rm = TRUE),
  )
women_by_majors_transpose <- data.frame(t(women_by_majors))


pie_menvswomen <- ggplot(women_by_majors_transpose)+
  geom_bar(mapping = aes(x = TotalShareMen, fill = Major_category))+
  coord_polar(theta = "x")+
  facet_wrap(~Major_category)

                      
                      
          