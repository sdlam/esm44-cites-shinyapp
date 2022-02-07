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

wildlife <- read_csv(here("data", "cites_wildlife_data.csv"))



## create user interface 

ui <- fluidPage(
  navbarPage(
    "Wildlife Trade Visualization", #app title
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
                 radioButtons("radio", label = h3("Select Product"),
                              choices = list("Product 1","Product 2","Product 3")
                              ), # end radioButtons
                 mainPanel(
                   "output goes here"
                 )
               ) #end sidebarPanel
             ) #end sidebarLayout
             ) #end tabPanel widget 4
  ) # this is end of navbarPage 

) #end ui


## create server function: 

server <- function(input, output){}

    

# Combine into an app
shinyApp(ui = ui, server = server)
