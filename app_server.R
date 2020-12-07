library(dplyr)
library(ggplot2)
library(shiny)
# WIDGET NAMING CONVENTIONS CHARTTYPE_WIDGET_#

# INTRO - BRENDA'S SECTION
ui <- fluidPage(
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
h4("Authors: Nicole Fendi, Ian Wang, Brenda Obonyo, Leon kan, Zheng Ruisun"),

p("The authors are students at the University of Washington studying Informatics.
  We are passionate about creating accessible information to help people make data
  -driven decisions.")


)

server <- function(input, output) {
  
}


shinyApp(ui = ui, server = server)





# WOMEN vs MEN pie chart - JERRY'S SECTION









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

line_plot_data <- left_join(median_salary_recent_grad, median_salary_grad_students)

line_plot = ggplot(line_plot_data, aes(x = Major_category)) +
  geom_line(aes(y = medianRecent), color = "blue", linetype = "twodash") +
  geom_line(aes(y = medianGrad), color = "red", linetype = "twodash") +
  xlab("Major categories") +
  ylab("Salary median")

ggplot(economics, aes(x=date)) + 
  geom_line(aes(y = psavert), color = "darkred") + 
  geom_line(aes(y = uempmed), color="steelblue", linetype="twodash")






# MAJOR UNEMPLOYED vs EMPLOYED bar plot - NICOLE'S SECTION









# CONCLUSION TAKEAWAYS - IAN'S SECTION


my_server <- function(input, output){
  
}