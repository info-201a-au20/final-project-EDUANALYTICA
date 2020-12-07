library("shiny")
library("dplyr")
library("tidyr")
library("ggplot2")
library("plotly")
library("knitr")

# my_server <- function(input, output){
#   
# # WIDGET NAMING CONVENTIONS CHARTTYPE_WIDGET_#
# 
# # INTRO - BRENDA'S SECTION
# 
# 
# 
# 
#   
# 
# 
# 
# # WOMEN vs MEN pie chart - JERRY'S SECTION
#   num_men <- read.csv("data/women-stem.csv", 
#                       stringsAsFactors = FALSE) %>% 
#     filter(Major_category == input$pie_widget_one) %>% 
#     summarize(
#       sum_men = sum(Men, na.rm = TRUE),
#       sum_women = sum(Women, na.rm = TRUE)
#     ) %>% 
#     pull(
#       sum_men
#     )
#   num_women <- read.csv("data/women-stem.csv", 
#                         stringsAsFactors = FALSE) %>% 
#     filter(Major_category == input$pie_widget_one) %>% 
#     summarize(
#       sum_women = sum(Women, na.rm = TRUE)
#     ) %>% 
#     pull(
#       sum_women
#     )
#   
#   gender <- c("Men", "Women")
#   num <- c(num_men, num_women)
#   df <- data.frame(gender, num)
#   output$pieplot <- renderPlot({
#       pie_plot <- pie(num, labels = gender)
#       return(pie_plot)
#       # plot_ly(df, labels = ~gender, values = ~num, type = 'pie', textposition = 'inside',
#       #       textinfo = 'label+percent',
#       #       insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
#       #       text = ~paste0(gender, ": ", num),  
#       #       marker = list(colors = colors,line = list(color = '#FFFFFF', 
#       #                                                 width = 1)),
#       #       showlegend = FALSE) # %>%
#       # layout(autosize = F, width = 500, height = 500,
#       #        title = ~paste0("Percentage of Men and Women in ", 
#       #                        input$pie_widget_one, " <br /> (Total in Major: ", 
#       #                        num_men + num_women, " )"),  
#       #        showlegend = F,
#       #        xaxis = list(showgrid = FALSE, zeroline = FALSE, 
#       #                     showticklabels = FALSE),
#       #        yaxis = list(showgrid = FALSE, zeroline = FALSE, 
#       #                     showticklabels = FALSE))
#   })
# 
# 
# 
# 
# 
# 
# 
# 
# # RECENTGRAD vs GRAD line chart - LEON'S SECTION
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # MAJOR UNEMPLOYED vs EMPLOYED bar plot - NICOLE'S SECTION
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # CONCLUSION TAKEAWAYS - IAN'S SECTION
# }