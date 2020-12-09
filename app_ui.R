# Import Library
library("shiny")
library("dplyr")
library("plotly")
library("ggplot2")
library("shinythemes")
library("gfonts")

# Initiate Introduction Page
intro_main_content <- mainPanel(
  h1("An Investment in Knowledge Pays The Best Interest"),
  img(src = "gradcaps.png"),
  h3("Background Information"),
  p(" People contemplate getting a college degree and they wonder if it is
    worth the resources.
    Female identifying students in STEM fields deliberate about being the only
    females in their classes.
    People who already have their undergraduate degrees are curious to know if
    getting a graduate degree in their field is a good investment.
    Students are also curious to know about the probability of not getting a
    job once they graduate from school with their majors."),
  h3("Questions Answered"),
  p(id = "list_name", "This project seeks to provide insights about the
    education domain. It answers the following questions:"),
  tags$ol(id = "list",
    tags$li("What are the gender disparities within majors?"),
    tags$li("How much does a recent graduate earn as comapared to a graduate
              student?"),
    tags$li("What is the unemployment rate within different majors?"),
  ),
  h3("Source of Data"),
  p("Our data is obtained from the American Community Survey Public Use
    Microdata series and from the United States Census. The data contains grad
    students (ages 25+) as well as recent grads (ages < 28) with information
    about basic earnings and labor force information.")
)

# Create a Tab Panel for Introduction
page_intro <- tabPanel(
  "Introduction",
  fluidPage(
    intro_main_content
  )
)

# Wrangle Data for Pie Select Input
women_in_stem <- read.csv("data/women-stem.csv", stringsAsFactors = FALSE)
major <- women_in_stem %>%
  pull(Major)

# Create Pie Chart Sidebar Panel
pie_sidebar_content <- sidebarPanel(
  selectInput(
    inputId = "pie_widget_one",
    label = "Major:",
    choice = major
  )
)

# Create Pie Chart Main Panel
pie_main_content <- mainPanel(
  plotlyOutput("pieplot")
)

# Create Page One Tab Panel
page_one <- tabPanel(
  "Pie Chart",
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Plot",
      pie_sidebar_content,
      pie_main_content
    ),
    tabPanel("Summary",
             h3("We explored the following questions with the pie chart"),
             tags$ol(id = "list",
                     tags$li("What are the gender disparities within majors?"),
                     tags$li("Are there any patterns or trends that can be
                             noticed with certain majors being more dominated
                             by a certain gender?")
             ),
             p("With the data provided to us, we split the bar graph into 2
             sections: men and women. The selector on the left allows the user
             to filter between the different majors. It is observed from the pie
             charts that fields in engineering were more dominated by men,
             while fields in healthcare were more dominated by women.")
    )
  ),
)

# Import combined dataframe from server.r
source("app_server.R")
line_plot_data

# Store the Range - recent_grad
salary_range_recent <- range(line_plot_data$medianRecent)

# Create Line Plot Sidebar Panel for Recent Grad
line_sidebar_content <- sidebarPanel(
  h2("Select a Median Salary Range - Recent Graduated Attendees"),
  sliderInput(
    inputId = "lineplot_recent_sliderBar", label = "",
    min = round(salary_range_recent[1], digits = 0),
    max = round(salary_range_recent[2], digits = 0),
    value = salary_range_recent
  )
)

# Store the Range - grad_students
salary_range_grad <- range(line_plot_data$medianGrad)

# Create Line Plot Sidebar Panvel for Grad
line_sidebar_content2 <- sidebarPanel(
  h2("Select a Median Salary Range - Graduate School Attendees"),
  sliderInput(
    inputId = "lineplot_grad_sliderBar", label = "",
    min = round(salary_range_grad[1], digits = 0),
    max = round(salary_range_grad[2], digits = 0),
    value = salary_range_grad
  )
)

# Create Line Plot Main Panel
line_main_content <- mainPanel(
  plotlyOutput(
    outputId = "lineplot_recent_widget"
  )
)
line_main_content2 <- mainPanel(
  plotlyOutput(
    outputId = "lineplot_grad_widget"
  )
)

# Create Page Two Tab Panel
page_two <- tabPanel(
  "Line plots",
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Plot",
      line_sidebar_content,
      line_main_content,
      line_sidebar_content2,
      line_main_content2
    ),
    tabPanel("Summary",
             h3("We explored the following questions with the line plots"),
             tags$ol(id = "list",
                     tags$li("What major categories are worth pursuing a
                             master’s or higher degree?"),
                     tags$li("What major categories are not worth pursuing a
                             master’s or higher degree?")
             ),
             p("With the data provided to us, we created two line charts.
               The first line chart is of different salaries of recent graduates
               < 28, while the second line chart is of different salaries of
               graduates 25+. The slider bar adjusts and filters the salary
               range of the line charts. It is observed that: Engineering majors
               have the highest median salary for both recent graduates and
               attendees went to graduate schools. A degree of master or higher
               level makes a huge improvement on the median salary in
               Psychology & Social Work (30k - 62k), Physical Science
               (42k - 88k), Humanities & Liberal Arts(32-67).")
    )
  ),
)

# Wrangle Data for Bar Select Input
major_categories <-
  read.csv("data/recent-grads.csv", stringsAsFactors = FALSE) %>%
  group_by(Major) %>%
  summarize() %>%
  pull(Major)

# Create Bar Plot Sidebar Panel
bar_sidebar_content <- sidebarPanel(
  selectInput(
    inputId = "x_var",
    label = "Select a major",
    choices = major_categories,
    selected = "Engineering"
  )
)

# Create Pie Plot Main Panel
bar_main_content <- mainPanel(
  plotlyOutput(
    outputId = "barplot"
  )
)

# Create Page Three Tab Panel
page_three <- tabPanel(
  "Bar Chart",
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Plot",
      bar_sidebar_content,
      bar_main_content
    ),
    tabPanel("Summary",
             h3("We explored the following questions with the bar chart"),
             tags$ol(id = "list",
                     tags$li("What are the employment and unemployment rates
                             across different majors?"),
                     tags$li("What is the difference between unemployment rates
                             across different majors?")
             ),
             p("With the data provided to us, we created a bar chart. The blue
               bar represents the unemployment rate of a specific major and the
               pink bar represents the employment rate. It is observed that most
               majors have an unemployment rate centered around 5 percent and
               there is no tremendous difference in the unemployment rate among
               different majors. The majors that have the lowest unemployment
               rate are Educational Administration and Supervision and
               Geological and Geophysical Engineering with no unemployment and
               the major that has the highest unemployment rate is Miscellaneous
               Fine Art with an unemployment rate of 15 percent.")
    )
  ),
)

# Create Page Four Tab Panel
page_four <- tabPanel(
  "Our Findings",
  h1("Our Findings"),
  h3("What are the gender disparities within majors?"),
  plotlyOutput("gender_disparities"),
  br(),
  p("From comparing the number of women vs. men within different majors,
    we can observe some patterns among the gender disparities within majors.
    One of the patterns observed is, majors under engineering have a higher
    percentage of men compared to women. The bar chart above displays the 5
    majors that have the most significant difference between the amount of men
    and women. The greater implication of this finding is that women are not
    equally represented in fields such as engineering, while similarly,
    men are not equally represented in fields such as health."),
  h3("How much does a recent undergraduate earn as comapared to a graduate
     student?"),
  plotlyOutput("salary_difference"),
  p("By comparing the salaries of recent undergraduates and those who have a
    graduate degree within different major categories, we found that people who
    earned graduate degrees make significantly more in all major categories.
    For example, in the chart above, an engineering recent undergraduate earns
    around $57k/per year while an engineering major with a graduate degree can
    earn up to $94k/per year. The greater implication of this finding is that
    graduate school can be an excellent way to boost your earnings;
    however, it notes the issue of affordable and attainable education for all
    who pursue it."),
  h3("What is the unemployment rate within different majors?"),
  plotlyOutput("employment_rate"),
  br(),
  p("We compared the number of unemployment and employed rates of different
    major categories and found that engineering, health, and humanities &
    liberal arts have the highest unemployment rate as well as employed rate.
    However, business runs as an outlier with the highest employment and
    unemployment rate, almost having 5000 more employed people than
    engineering. We also found that there's a linear pattern when comparing
    unemployment and employment rates for all majors. Therefore, the higher
    the unemployment rate, the higher the employment rate. The greater
    implication of this finding is that there is an abundance of business
    graduates while there is a huge demand for individuals
    in the engineering industry.")
)

# Create Page Five Main Panel
page_five_main_content <- mainPanel(
  h3("About Us"),
  h4("Authors: Nicole Fendi, Ian Wang, Brenda Obonyo, Leon Kan, Zhengrui Sun"),
  p("The authors are students at the University of Washington studying
  Informatics. We are passionate about creating accessible information to help
  people make data-driven decisions.")
)

# Create Page Five Tab Panel
page_five <- tabPanel(
  "About Us",
  fluidPage(page_five_main_content)
)

# Initiate UI
my_ui <- navbarPage(
  theme = shinytheme("yeti"),
  "FINAL PROJECT",
  page_intro,
  page_one,
  page_two,
  page_three,
  page_four,
  page_five,
  includeCSS("styles.css")
)