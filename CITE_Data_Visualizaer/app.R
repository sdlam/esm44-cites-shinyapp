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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CITES Wildlife Data Visualization"),

 
    # radio buttons for widget 1
    radioButtons("radio", label = h3("Widget 1"),
                 choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                 selected = 1),
    
    hr(),
    fluidRow(column(3, verbatimTextOutput("value")))
    
)
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
