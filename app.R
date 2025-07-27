library(ggplot2)
library(shiny)
library(dplyr)
library(rsconnect)

ui <- fluidPage(
  titlePanel("mtcars Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var_cont", "Continuous variable:",
                  choices = c("mpg", "disp", "hp", "drat", "wt", "qsec")),
      selectInput("var_disc", "Discrete variable:",
                  choices = c("cyl", "vs", "am", "gear", "carb"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("data_table")),
        tabPanel("Summary", verbatimTextOutput("summary_output")),
        tabPanel("BoxPlot", plotOutput("box_plot")),
        tabPanel("BarPlot", plotOutput("bar_plot")),
        tabPanel("Histogram", plotOutput("hist_plot"))
      )
    )
  )
)

server <- function(input, output) {
  output$data_table <- renderTable({
    mtcars |> select(all_of(c(input$var_cont, input$var_disc)))
  })
  
  output$summary_output <- renderPrint({
    mtcars |> select(all_of(c(input$var_cont, input$var_disc))) |> summary()
  })
  
  output$box_plot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$var_disc, y = input$var_cont)) +
      geom_boxplot(fill = "blue") +
      labs(title = "Boxplot")
  })
  
  output$bar_plot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$var_disc)) +
      geom_bar(fill = "orange") +
      labs(title = "Bar Plot")
  })
  
  output$hist_plot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$var_cont)) +
      geom_histogram(binwidth = 2, fill = "green", color = "white") +
      labs(title = "Histogram")
  })
}

shinyApp(ui = ui, server = server)