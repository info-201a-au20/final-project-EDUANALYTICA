library("shiny")
library("dplyr")
library("plotly")
library("ggplot2")

intro_main_content <- mainPanel(
  
    h1("An Investment in Knowledge Pays The Best Interest"),
    
   
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
    p(id = "list_name", "This project seeks to provide insights about the education domain. 
    It answers the following questions:"),
    tags$ol(
      tags$li("What are the gender disparities within majors?"),
      tags$li("How much does a recent graduate earn as comapared to a graduate student?"),
      tags$li("What is the unemployment rate within different majors?"),
    ),
   
    h3("Source of Data"),
    p("Our data is obtained from the American Community Survey Public Use Microdata
    series and from the United States Census. The data contains grad students 
      (ages 25+) as well as recent grads (ages < 28) with information about basic
      earnings and labor force information.")
)

page_intro <- tabPanel(
  "Introduction",
  fluidPage(
    intro_main_content
  )
)

women_in_stem <- read.csv("data/women-stem.csv", stringsAsFactors = FALSE)
major <- women_in_stem %>% 
  pull(Major)

pie_sidebar_content <- sidebarPanel(
  selectInput(
    inputId = "pie_widget_one",
    label = "Major:",
    choice = major
  )
)
pie_main_content <- mainPanel(
  plotlyOutput("pieplot")
)

page_one <- tabPanel(
  "Bar Chart",
  sidebarLayout(
  
  pie_sidebar_content,
  pie_main_content
  
))

# Leon's part: lineplot
# import combined dataframe from server.r
source("app_server.R")
line_plot_data

# store the range - recent_grad
salary_range_recent <- range(line_plot_data$medianRecent)
line_sidebar_content <- sidebarPanel(
  h2("Select a Median Salary Range - Recent Graduated Attendees"),
  sliderInput(inputId = "LinePlot_recent_SliderBar",label = "",
              min = round(salary_range_recent[1], digits = 0), 
              max = round(salary_range_recent[2], digits = 0),
              value = salary_range_recent)
)

# store the range - grad_students
salary_range_grad <- range(line_plot_data$medianGrad)
line_sidebar_content2 <- sidebarPanel(
  h2("Select a Median Salary Range - Graduate School Attendees"),
  sliderInput(inputId = "LinePlot_grad_SliderBar", label = "",
              min = round(salary_range_grad[1], digits = 0), 
              max = round(salary_range_grad[2], digits = 0),
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
page_two <- tabPanel(
  "Line plots",
  sidebarLayout(
    line_sidebar_content,
    line_main_content
  ),
  sidebarLayout(
    line_sidebar_content2,
    line_main_content2
  )
)

major_categories <- read.csv("data/recent-grads.csv", stringsAsFactors = FALSE) %>%
  group_by(Major) %>% 
  summarize() %>% 
  pull(Major)

  

bar_sidebar_content <- sidebarPanel(
selectInput(inputId = "x_var",
            label = "Select a major",
            choices = major_categories,
            selected = "Engineering")
)

bar_main_content <- mainPanel(
  h1("Comparison of Unemployed and Employed Rates of Recent Graduates based
     on major"),
  plotlyOutput(
    outputId = "BarPlot"
  )
)

page_three <- tabPanel(
  "Bar Chart",
  sidebarLayout(bar_sidebar_content, 
                bar_main_content)
)

page_four <- tabPanel(
  
  "Our Findings",
  h1("Our Findings"),
  h3("What are the gender disparities within majors?"),
  plotlyOutput("gender_disparities"), 
  p("From comparing the number of women vs. men within different majors, 
    we can observe some patterns among the gender disparities within majors.
    One of the patterns observed is, majors under engineering have a higher
    percentage of men compared to women. The bar chart above displays the 5 majors that have
    the most significant difference between the amount of men and women."),
  h3("How much does a recent undergraduate earn as comapared to a graduate student?"),
  p("A recent undergraduate "),
  h3("What is the unemployment rate within different majors?"),
  p()
)
# 
# conc_sidebar_content
# conc_main_content
# 
# 

page_four_main_content <- mainPanel(
  h3("Abouts Us"),
  h4("Authors: Nicole Fendi, Ian Wang, Brenda Obonyo, Leon Kan, Zhengrui Sun"),
  p("The authors are students at the University of Washington studying Informatics.
    We are passionate about creating accessible information to help people make data
    -driven decisions.")
)
  
page_five <- tabPanel(
  id = "aboutus",
  "About Us",
  fluidPage(page_four_main_content)
)


my_ui <- navbarPage("FINAL PROJECT",
  page_intro,
  page_one,
  page_two,
  page_three,
  page_four,
  page_five,
  includeCSS("styles.css")
)
