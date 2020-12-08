library("shiny")
library("dplyr")
library("plotly")
library("ggplot2")

# intro_sidebar_content 
# intro_main_content
# 
# pie_sidebar_content
# pie_main_content

# import combined dataframe from server.r
# source("app_server.R")
# line_plot_data
# 
# salary_range_recent <- range(line_plot_data$salary_index)
# 
# line_sidebar_content <- sidebarPanel(
#   h2("Select a range of median salary"),
#   # radioButtons("user_select", label = h3("Radio buttons"),
#   #              choices = list("Median of Recent Graduates" = 1, 
#   #                             "Average of Medians" = 2, 
#   #                             "Median of Graduates" = 3), 
#   #              selected = avg),
#   sliderInput(inputId = "LinePlot_SliderBar", label = h3("Slider Range"), 
#               min = salary_range_recent[1], max = salary_range_recent[2], 
#               value = salary_range_recent),
# )
# 
# 
# line_main_content <- mainPanel(
#   h1("The Comparison of Median Salary between Recent grad and grad"),
#   plotlyOutput(
#     outputId = "LinePlot_widget"
#   )
# )

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
  plotlyOutput("pieplot")
)

# page_three <- tabPanel(
#   "Page 3",
#   sidebarLayout(
#     line_sidebar_content,
#     line_main_content
#   )
# )

# bar_sidebar_content
# bar_main_content
# 
# conc_sidebar_content
# conc_main_content
# 
# 


my_ui <- fluidPage(
  pie_sidebar_content,
  pie_main_content
)