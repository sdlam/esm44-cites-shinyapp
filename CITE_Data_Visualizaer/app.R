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

wildlife <- read.csv("cites_wildlife_data.csv", stringsAsFactors = FALSE) 

## create user interface 

ui <- fluidPage(
  navbarPage(
    "Wildlife Trade Visualization",
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
             ), # end of tabPanel
    tabPanel("Widget 3"),
    tabPanel("Widget 4")
  ) # this is end of navbarPage 

) #end ui


## create server function: 

server <- function(input, output){}

    

# Combine into an app
shinyApp(ui = ui, server = server)
