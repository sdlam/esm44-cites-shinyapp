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
library(janitor)
library(here)
library(bslib)

wildlife_trade <- read_csv(here("data", "cites_wildlife_data.csv")) %>% 
  clean_names()

## create user interface 
ui <- fluidPage(theme = bs_theme(bootswatch = "superhero"),
  navbarPage(
    "Wildlife Trade Visualization", #app title
    tabPanel("Data", 
            mainPanel(
              "This app analyses CITES data on global, legal wildlife trade information from 2016 to 2017.", 
              tags$img(src = "Elephant.jpeg"), 
              "Image source: CITES Elephants page"
            ) #end mainPanel
             ), #end tabPanel
    tabPanel("Widget 1",
             sidebarLayout(
               sidebarPanel(
                 'widget 1 goes here',
                 checkboxGroupInput(inputId = "wildlife_dist",
                                    label = "Choose Term:",
                                    choices = c("Importer", "Exporter")
                                    ) # end checkboxGroupInput
               ), #end of sidebarPanel
               mainPanel(
                 "output goes here"
                 ) # end of mainPanel
             ), #end of sidebarLayout
            ), #end of tabPanel for widget 1
    tabPanel("Widget 2",
             sidebarLayout(
               sidebarPanel(
                 "Widget 2 goes here",
                 checkboxGroupInput(inputId = "wildlife_dist",
                                    label = "Choose Term:",
                                    choices = c("Live","Bodies","Specimens")
                                    ) #end checkboxGroupOutput
               ),#end sidebarPanel 
               mainPanel(
                 "output goes here"
               ) #end of mainPanel 
             ) #end sidebarLayout
             ), # end of tabPanel widget 2
    tabPanel("Widget 3",
             sidebarLayout(
               sidebarPanel(
                 "Widget 3 goes here",
                 selectInput("select",label = h3("Select Species"),
                             choices = list("Species 1","Species 2",
                                            "Species 3","Species 4","Species 5")
                             ) # end selectInput
                 ), #end sidebarPanel
               mainPanel( 
                 "output goes here"
               ) #end of mainPanel
             ) #end sidebarLayout
             ), #end tabPanel for widget 3
    tabPanel("Widget 4",
             sidebarLayout(
               sidebarPanel(
                 "Widget 4 goes here",
                 radioButtons("radio", 
                              inputId = "trade_purpose", 
                              label = h3("Select Trade Purpose"),
                              choices = list("Commercial","Personal","Scientific", "Hunting Trophy", "Circus/Traveling Exibition")
                              ) # end radioButtons
               ),#end sidebarPanel
               mainPanel(
                 "output goes here",
                 plotOutput(outputId = 'purpose_plot')
               ) #end of mainPanel
             ) #end sidebarLayout
             ) #end tabPanel widget 4
  ) # this is end of navbarPage 

) #end ui


## create server function: 

server <- function(input, output) {
  purpose_select <-  reactive({
    wildlife_trade %>% 
      filter(purpose == input$trade_purpose)
  }) #end purpose_select reactive
  
  output$purpose_plot <- renderPlot({
    ggplot(data = purpose_select(), aes(x = genus)) +
      geom_bar()
  })
}

# Combine into an app
shinyApp(ui = ui, server = server)
