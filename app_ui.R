library("shiny")
library("plotly")
library("ggplot2")

# intro_sidebar_content 
# intro_main_content
# 
# pie_sidebar_content
# pie_main_content

# import combined dataframe from server.r
source("app_server.R")
line_plot_data

salary_range_recent <- range(line_plot_data$medianRecent)
line_sidebar_content <- sidebarPanel(
  h2("Select a Median Salary Range - Recent Grads"),
  sliderInput(inputId = "LinePlot_recent_SliderBar", label = h3("Slider Range"),
              min = salary_range_recent[1], max = salary_range_recent[2],
              value = salary_range_recent)
)

salary_range_grad <- range(line_plot_data$medianGrad)
line_sidebar_content2 <- sidebarPanel(
  h2("Select a Median Salary Range - Grads"),
  sliderInput(inputId = "LinePlot_grad_SliderBar", label = h3("Slider Range"),
              min = salary_range_grad[1], max = salary_range_grad[2],
              value = salary_range_grad)
)

line_main_content <- mainPanel(
  plotlyOutput(
    outputId = "LinePlot_recent_widget"
  )
)

line_main_content2 <- mainPanel(
  plotlyOutput(
    outputId = "LinePlot_grad_widget"
  )
)

page_three <- tabPanel(
  "Page 3",
  sidebarLayout(
    line_sidebar_content,
    line_main_content
  ),
  sidebarLayout(
    line_sidebar_content2,
    line_main_content2
  )
)

# bar_sidebar_content
# bar_main_content
# 
# conc_sidebar_content
# conc_main_content



my_ui <- navbarPage(
  page_three
)