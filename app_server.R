# import libraries
library("shiny")
library("dplyr")
library("tidyr")
library("ggplot2")
library("plotly")
library("knitr")


# INTRO - BRENDA'S SECTION
# ALL CODE BELOW HAS BEEN REMMOVED TO app_ui.R
intro_widget <- fluidPage(
  navbarPage(tabPanel("Overview"),
             tabPanel("B"),
             tabPanel("c"),
             tabPanel("D"),
             tabPanel("E")
    
  ),
  titlePanel("An Investment in Knowledge Pays The Best Interest"),
  
  h3("Background Information"),
  
  p(" People contemplate getting a college degree and they wonder if it is 
  worth the resources.
    Female identifying students in STEM fields deliberate about being the only 
    females in their classes.
    People who already have their undergraduate degrees are curious to know if 
    getting a graduate degree in 
     their field is a good investment.
    Students are also curious to know about the probability of not getting a job
    once they graduate from school 
    with their majors"),
  
  h3("Questions answered"),
  
  p("This project seeks to provide insights about the education domain. 
  It answers the following questions:"),
  ("1. What are the gender disparities within majors?"),
 ("2How much does a recent graduate earn as comapared to a graduate student?"),
  ("3. What is the unemployment rate within different majors?"),
 
 h3("Source of Data"),
 
p("Our data is obtained from the American Community Survey Public Use Microdata
  series and from the United States Census. The data contains grad students 
    (ages 25+) as well as recent grads (ages < 28) with information about basic
    earnings and labor force information."),
h3("Abouts us"),
h4("Authors: Nicole Fendi, Ian Wang, Brenda Obonyo, Leon kan, Zhengrui Sun"),

p("The authors are students at the University of Washington studying Informatics.
  We are passionate about creating accessible information to help people make data
  -driven decisions.")
)




# RECENTGRAD vs GRAD line chart - LEON'S SECTION
recent_grad <- read.csv("data/recent-grads.csv", stringsAsFactors = FALSE)
grad_students <- read.csv("data/grad-students.csv", stringsAsFactors = FALSE)

median_salary_recent_grad <- recent_grad %>%
  select(Major, Major_category, Median) %>%
  group_by(Major_category) %>%
  summarise(
    medianRecent = mean(Median, na.rm = TRUE)
  )
median_salary_grad_students <- grad_students %>%
  select(Major, Major_category, Grad_median) %>%
  group_by(Major_category) %>%
  summarise(
    medianGrad = mean(Grad_median, na.rm = TRUE)
  )

line_plot_data <- left_join(median_salary_recent_grad,
                            median_salary_grad_students) %>% 
  mutate(avg = (medianRecent + medianGrad) / 2) %>% 
  mutate(salary_index = seq(30000,96250, by = 4375))

# MAJOR UNEMPLOYED vs EMPLOYED bar plot - NICOLE'S SECTION

# MAJOR UNEMPLOYED vs EMPLOYED bar plot - NICOLE'S SECTION
recent_grad <- read.csv("data/recent-grads.csv", stringsAsFactors = FALSE)

major_categories <- recent_grad %>%
  group_by(Major_category) %>%
  summarise(
    total_employed = sum(Employed, na.rm = TRUE),
    total_unemployed = sum(Unemployed, na.rm = TRUE)
  )









# CONCLUSION TAKEAWAYS - IAN'S SECTION


# Server function
my_server <- function(input, output){
  
  # output$LinePlot_Range <- renderPrint({ input$LinePlot_SliderBar })
  # 
  # output$LinePlot_widget <- renderPlotly({
  #   filtered_data <- line_plot_data %>% 
  #     filter(salary_index > input$LinePlot_SliderBar[1] & salary_index < input$LinePlot_SliderBar[2])
  #   
  #   # plot here
  #   LinePlot_widget <- plot_ly(filtered_data,
  #           # x = ~Major_category, y = ~input$user_select,
  #           x = ~Major_category, y = ~medianRecent,
  #           # x = ~Major_category, y = ~avg,
  #           name = "Median Salary of Recent graduates",
  #           type = "scatter", mode = 'lines',
  #           line = list(color = 'rgb(205, 12, 24', width = 3))
  #   LinePlot_widget <- LinePlot_widget %>%
  #     add_trace(y = ~medianGrad, name = "Median Salary of Graduates",
  #               line = list(color = 'rgb(22, 96, 167)', width = 3, mode = 'lines'))
  #   LinePlot_widget <- LinePlot_widget %>%
  #     add_trace(y = ~avg, name = "Mean of the Median Salary of Recent Grads and Grads",
  #               line = list(color = 'rgb(220,220,220)', width = 3, dash = 'dash'))
  #   LinePlot_widget <- LinePlot_widget %>%
  #     layout(title = "The Comparison of Median Salary between Recent grad and grad",
  #            xaxis = list(title = "Major Categories"),
  #            yaxis = list(title = "Median Salaries in dollar"))
  # })
  
  #Nicole's bar plot code
  output$BarPlot <- renderPlotly({ 
    BarPlot <- plot_ly(
      name = "Employed",
      data = major_categories,
      x = ~total_employed,
      y = input$x_var,
      type = "bar",
      orientation = "h"
    ) 
    BarPlot <- BarPlot %>% add_trace(x = ~total_unemployed, name = "Unemployed")
    BarPlot <- BarPlot %>% layout(xaxis = list(title = 'Count'), barmode = 'group')
    
  })
  
  # WOMEN vs MEN pie chart - JERRY'S SECTION
  output$pieplot <- renderPlotly({
    
    num_men <- read.csv("data/women-stem.csv",
                        stringsAsFactors = FALSE) %>%
      filter(Major_category == input$pie_widget_one) %>%
      summarize(
        sum_men = sum(Men, na.rm = TRUE),
        sum_women = sum(Women, na.rm = TRUE)
      ) %>%
      pull(
        sum_men
      )
    
    num_women <- read.csv("data/women-stem.csv",
                          stringsAsFactors = FALSE) %>%
      filter(Major_category == input$pie_widget_one) %>%
      summarize(
        sum_women = sum(Women, na.rm = TRUE)
      ) %>%
      pull(
        sum_women
      )
    
    gender <- c("Men", "Women")
    
    num <- c(num_men, num_women)
    
    df <- data.frame(gender, num)
    
    plot_ly(df, labels = ~gender, values = ~num, type = 'pie', textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
            text = ~paste0(gender, ": ", num),
            marker = list(colors = colors,line = list(color = '#FFFFFF',
                                                      width = 1)),
            showlegend = FALSE) 
  })
  
  
  # Leon's part: line plot
  output$LinePlot_Range <- renderPrint({ input$LinePlot_SliderBar })
  output$LinePlot_recent_widget <- renderPlotly({
    # salary_range_recent <- range(input$user_select)
    filtered_recent <- line_plot_data %>% 
      filter(medianRecent >= input$LinePlot_recent_SliderBar[1] & 
               medianRecent <= input$LinePlot_recent_SliderBar[2])
    
    LinePlot_widget_recent <- plot_ly(filtered_recent,
            x = ~Major_category, y = ~medianRecent,
            name = "Median Salary of Recent graduates",
            type = "scatter", mode = 'lines',
            line = list(color = 'rgb(205, 12, 24', width = 3)) %>% 
      layout(title = "Median of Recent Graduates by Majors - Under 28-years-old",
             xaxis = list(title = "Major Categories"),
             yaxis = list(title = "Median Salaries in dollar"))
  })
  
  output$LinePlot_grad_widget <- renderPlotly({
    filtered_grad <- line_plot_data %>% 
      filter(medianGrad >= input$LinePlot_grad_SliderBar[1] & 
               medianGrad <= input$LinePlot_grad_SliderBar[2])

    LinePlot_widget_grad <- plot_ly(filtered_grad,
                                    x = ~Major_category, y = ~medianGrad,
                                    name = "Median Salary of Graduates",
                                    type = "scatter", mode = 'lines',
                                    line = list(color = 'rgb(22, 96, 167)', 
                                                width = 3, mode = 'lines')) %>%
      layout(title = "Median of Graduates by Majors - Above 25-years-old",
             xaxis = list(title = "Major Categories"),
             yaxis = list(title = "Median Salaries in dollar"))
  })
}

