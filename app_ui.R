library("shiny")
library("dplyr")

women_in_stem <- read.csv("data/women-stem.csv", stringsAsFactors = FALSE)
major_category <- women_in_stem %>% 
  group_by(Major_category) %>% 
  summarize() %>% 
  pull(Major_category)

# intro_sidebar_content 
# intro_main_content

pie_sidebar_content <- sidebarPanel(
  selectInput(
    inputId = "pie_widget_one",
    label = "Major Category:",
    choice = major_category
  )
)
pie_main_content <- mainPanel(
  plotOutput("pieplot")
)

# line_sidebar_content
# line_main_content
# 
# bar_sidebar_content
# bar_main_content
# 
# conc_sidebar_content
# conc_main_content
# 
# 

my_ui <- navbarPage(
  tabPanel(
    pie_sidebar_content
  )
)