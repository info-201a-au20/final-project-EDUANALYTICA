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
    Men = sum(Men, na.rm = TRUE)/sum(Total, na.rm = TRUE), # / sum(Total, na.rm = TRUE),
    Women = sum(Women, na.rm = TRUE)/sum(Total, na.rm = TRUE), # / sum(Total, na.rm = TRUE),
  )

women_by_majors_test <- data.frame(t(women_by_majors)) %>% 
  rename("X1" = "Biology & Life Sciences", "X2" = "Computers & Mathematics", "X3" = "Engineering", "X4" = "Health", "X5" = "Physical Sciences")



  mutate(Gender = list("Male" = "Male", "Female" = "Female"))



pie_menvswomen <- ggplot(data = women_by_majors)+
  geom_bar(mapping = aes(x = TotalShareMen, y = TotalShareWomen))+
  facet_wrap(~Major_category)
  
                      
                      
          