# import libraries
library("shiny")
library("dplyr")
library("tidyr")
library("ggplot2")
library("plotly")
library("knitr")

# Wrangle data for line charts - LEON'S SECTION
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

line_plot_data <- left_join(
  median_salary_recent_grad,
  median_salary_grad_students
) %>%
  mutate(avg = (medianRecent + medianGrad) / 2) %>%
  mutate(salary_index = seq(30000, 96250, by = 4375))

# Server function
my_server <- function(input, output) {
  # Nicole's bar plot code
  output$barplot <- renderPlotly({
    recent_grad <- read.csv("data/all-ages.csv", stringsAsFactors = FALSE) %>%
      filter(Major == input$x_var) %>%
      summarize(
        total_unemployed = Unemployment_rate,
        total_employed = 1 - total_unemployed
      )

    barplot <- plot_ly(
      name = "Employed",
      data = recent_grad,
      x = ~total_employed,
      y = input$x_var,
      type = "bar",
      orientation = "h",
      marker=list(color = c("#d35d6e"))
    )
    barplot <- barplot %>% add_trace(x = ~total_unemployed, name = "Unemployed",
                                     marker = list(color = c("#6886c5")))
    barplot <- barplot %>% layout(
      title = "Employment Rate of Recent Graduates by Major",
      xaxis = list(title = "Percentage"),
      barmode = "group"
    )
  })


  # WOMEN vs MEN pie chart - JERRY'S SECTION
  output$pieplot <- renderPlotly({
    num_men <- read.csv("data/women-stem.csv",
      stringsAsFactors = FALSE
    ) %>%
      filter(major == input$pie_widget_one) %>%
      pull(
        Men
      )

    num_women <- read.csv("data/women-stem.csv",
      stringsAsFactors = FALSE
    ) %>%
      filter(major == input$pie_widget_one) %>%
      pull(
        Women
      )

    gender <- c("Men", "Women")
    
    gender_color <- c("Men" = "#d35d6e", "Women" = "#6886c5")

    num <- c(num_men, num_women)

    df <- data.frame(gender, num)

    plot_ly(df,
      labels = ~gender, values = ~num, type = "pie", textposition = "inside",
      textinfo = "label+percent",
      insidetextfont = list(color = "#FFFFFF"), hoverinfo = "text",
      text = ~ paste0(gender, ": ", num),
      marker = list(colors = gender_color, line = list(
        color = "#FFFFFF",
        width = 1
      )),
      showlegend = FALSE
    ) %>%
      layout(
        title = ~ paste0(
          "Percentage of Men and Women in ",
          input$pie_widget_one
        )
      )
  })




  # Leon's part: line plot
  # print the slider bar for recent-grad
  output$lineplot_range <- renderPrint({
    input$lineplot_sliderBar
  })

  # display the line plot associated with recent_grad dataset
  output$lineplot_recent_widget <- renderPlotly({
    filtered_recent <- line_plot_data %>%
      filter(medianRecent >= input$lineplot_recent_sliderBar[1] &
        medianRecent <= input$lineplot_recent_sliderBar[2])
    # generate the plot
    lineplot_widget_recent <- plot_ly(filtered_recent,
      x = ~Major_category, y = ~medianRecent,
      name = "Median Salary of Recent graduates",
      type = "scatter", mode = "lines",
      line = list(color = "rgb(205, 12, 24", width = 3)
    ) %>%
      layout(
        title = "Median of Recent Graduates by Majors - Under 28-years-old",
        xaxis = list(title = "Major Categories"),
        yaxis = list(title = "Median Salaries in dollar")
      )
  })

  # display the line plot associated with grad_students dataset
  output$lineplot_grad_widget <- renderPlotly({
    filtered_grad <- line_plot_data %>%
      filter(medianGrad >= input$lineplot_grad_sliderBar[1] &
        medianGrad <= input$lineplot_grad_sliderBar[2])
    # generate the plot
    lineplot_widget_grad <- plot_ly(filtered_grad,
      x = ~Major_category, y = ~medianGrad,
      name = "Median Salary of Graduates",
      type = "scatter", mode = "lines",
      line = list(
        color = "rgb(22, 96, 167)",
        width = 3, mode = "lines"
      )
    ) %>%
      layout(
        title = "Median of Graduates by Majors - Above 25-years-old",
        xaxis = list(title = "Major Categories"),
        yaxis = list(title = "Median Salaries in dollar")
      )
  })

  # conclusion gender disparities
  output$gender_disparities <- renderPlotly({
    gender_df <- read.csv("data/women-stem.csv", stringsAsFactors = FALSE) %>%
      mutate(
        percent_diff = abs((1 - ShareWomen) - ShareWomen),
        ShareMen = 1 - ShareWomen
      ) %>%
      top_n(5, percent_diff)
    major <- gender_df %>%
      pull(Major)
    percent_men <- gender_df %>%
      pull(ShareMen)
    percent_women <- gender_df %>%
      pull(ShareWomen)
    df_gender <- data.frame(major, percent_men, percent_women)

    plot_ly(df_gender,
      x = ~major, y = ~percent_men, type = "bar",
      name = "Men",
      marker=list(color = c("#d35d6e"))
    ) %>%
      add_trace(y = ~percent_women, name = "Women",  
                marker = list(color = c("#6886c5"))) %>%
      layout(
        title = "Top Five Majors of Gender Disparities",
        xaxis = list(title = ""),
        yaxis = list(title = "Percentage"), barmode = "stack"
      )
  })

  output$employment_rate <- renderPlotly({
    scatter_plot_data <- read.csv("data/all-ages.csv",
                                  stringsAsFactors = FALSE) %>%
      group_by(Major_category) %>%
      summarize(
        employed = sum(Employed, na.rm = TRUE) / 1000,
        unemployed = sum(Unemployed, na.rm = TRUE) / 1000,
        median = Median / 1000
      )

    scatter_plot <- ggplot(data = scatter_plot_data) +
      geom_point(
        mapping = aes(
          x = employed, y = unemployed, color = Major_category,
          size = median
        )
      ) +
      labs(
        title = "The Post-Graduation Employment Staus by College Major
        Categories",
        subtitle = "",
        color = "Major Category",
        size = "Dot size is median salary(1=$1000)",
        x = "Number of employed students (1 = 1000 students)",
        y = "Number of unemployed students (1 = 1000 students)"
      )
    scatter_plot
  })

  output$salary_difference <- renderPlotly({
    major_salary <- line_plot_data %>%
      pull(Major_category)
    median_recent <- line_plot_data %>%
      pull(medianRecent)
    median_grad <- line_plot_data %>%
      pull(medianGrad)
    df_salary <- data.frame(major_salary, median_recent, median_grad)

    plot_ly(df_salary,
      x = ~major_salary, y = ~median_recent, type = "bar",
      name = "Recent Graduate",
      marker=list(color = c("#d35d6e"))
  
    ) %>%
      add_trace(y = ~median_grad, name = "Graduate",  marker = 
                  list(color = c("#6886c5"))) %>%
      layout(
        title = "Median Salary Difference Between Recent Graduate and
             Graduate", xaxis = list(title = "Median Salary"),
        yaxis = list(title = "Salary"), barmode = "group"
      )
  })
}

