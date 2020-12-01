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
    Men = as.numeric(sum(Men, na.rm = TRUE)/ sum(Total, na.rm = TRUE)), # / sum(Total, na.rm = TRUE),
    Women = as.numeric(sum(Women, na.rm = TRUE)/ sum(Total, na.rm = TRUE)), # / sum(Total, na.rm = TRUE),
  )

women_by_majors_test <- data.frame(t(women_by_majors)) %>% 
  rename("Biology_LifeSciences" = "X1", 
         "Computers_Mathematics" = "X2", 
         "Engineering" = "X3",  
         "Health"= "X4", 
         "Physical_Sciences"= "X5") %>% 
  slice(2:3) %>% 
  mutate(Gender = c("Male" = "Male", "Female" = "Female"))
  

READ <- women_by_majors_test$Biology_LifeSciences

rownames(women_by_majors_test) = list("Male" = "Male", "Female" = "Female")

cols <- colnames(women_by_majors_test)


pie_biology <- ggplot(data = women_by_majors_test, aes(x = "", y = Biology_LifeSciences, fill = Gender))+
  geom_bar(stat = "identity", color = "black", width = 1)+
  coord_polar(theta = "y")
ggplotly(pie_biology)
  
pie_computers <- ggplot(data = women_by_majors_test, aes(x = "", y = Computers_Mathematics, fill = factor(Gender)))+
  geom_bar(stat = "identity", color = "black")+
  coord_polar(theta = "y")

pie_engineering <- ggplot(data = women_by_majors_test, aes(x = "", y = Engineering , fill = Gender))+
  geom_bar(stat = "identity", color = "black")+
  coord_polar(theta = "y")

pie_health <- ggplot(data = women_by_majors_test, aes(x = "", y = Health , fill = Gender))+
  geom_bar(stat = "identity", color = "black")+
  coord_polar(theta = "y")

pie_physical <- ggplot(data = women_by_majors_test, aes(x = "", y = Physical_Sciences , fill = Gender))+
  geom_bar(stat = "identity", color = "black")+
  coord_polar(theta = "y")     
                      
          