#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)



incomesa <- read_csv("/Users/Shared/Data 505/openml_1590.csv")

# Load data 
data(incomesa)

# Define ui 
ui <- fluidPage(
  # Set the app theme to "darkly"
  theme = shinytheme("darkly"),

  # Application title
  titlePanel("Income Levels above or below 50K"),
  
  # Sidebar with a slider input the age
  sidebarLayout(
    sidebarPanel(
      sliderInput("age",
                  h3("Select age:"),
                  min = 17,
                  max = 90,
                  value = 47), 

      selectInput("country", 
                  h4("Compare another country's pathway to the United States:"), 
                  choices = unique(incomesa$`native-country`)[-133],
                  selected = "Mexico")
    ),
    
    # Show scatterplots 
    mainPanel(
      plotlyOutput("gapPlot"), 
      plotOutput("pathPlot")
    )
  )
)

# Define server 
server <- function(input, output) {
  
  output$gapPlot <- renderPlotly({
    # generate plot based on input$age from ui.R
    p <- incomesa %>%
      filter(age == input$age) %>%
      ggplot(aes(x = `hours-per-week`, y = `age`, 
                 color = `education`, size = 3, 
                 text = `education`)) +
      geom_point() +
      labs(x = "hours per week", y = "age", 
           caption = "(Based on data from Hans Rosling - gapminder.com)", 
           color = 'education', size = "fnlwgt") + 
      ylim(3, 100) +
      xlim(min(incomesa$`hours-per-week`), max(incomesa$`hours-per-week`))
    
    ggplotly(p)
  })
  
  output$pathPlot <- renderPlot({
    # generate plot based on input$country from ui.R
    thisCountry <- input$country
    incomesa %>%
      filter(`native-country` %in% c("United-States", thisCountry)) %>%
      ggplot(aes(x = `fnlwgt`, y = `age`, 
                 color = `native-country`, size = `age`, 
                 alpha = `age`)) +
      geom_point() +
      labs(x = "fnlwgt", y = "age", 
           caption = "(Based on data from Hans Rosling - gapminder.com)", 
           color = 'native-country', size = "fnlwgt", 
           title = paste("United States vs ", thisCountry, sep = "")) + 
      ylim(3, 100) +
      xlim(min(incomesa$fnlwgt), max(incomesa$fnlwgt))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
