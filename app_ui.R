library("shiny")
library("plotly")
library("ggplot2")

# intro_sidebar_content 
# intro_main_content
# 
# pie_sidebar_content
# pie_main_content

line_sidebar_content <- sidebarPanel(
  h2("Select a range of median salary"),
  fluidPage(
    fluidRow(
      column(8,
             sliderInput(inputId = "LinePlot_SliderBar", label = h3("Slider Range"), min = 30, 
                         max = 100, value = c(50, 70))
      )
    ),
    
    hr(),
    
    fluidRow(
      column(8, verbatimTextOutput("LinePlot_Range"))
    )
  )
)

line_main_content <- mainPanel(
  h1("The Comparison of Median Salary between Recent grad and grad"),
  plotlyOutput(
    outputId = "LinePlot_widget"
  )
)

# bar_sidebar_content
# bar_main_content
# 
# conc_sidebar_content
# conc_main_content



my_ui <- navbarPage(
  line_sidebar_content,
  line_main_content
)