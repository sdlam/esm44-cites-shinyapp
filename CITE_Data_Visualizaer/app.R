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
library(sf)
library(DT)

#read in data
wildlife_trade <- read_csv(here("data", "cites_wildlife_data.csv")) %>% 
  clean_names()

#wrangle data for Widget 1: count imports/exports by country and attach to global sf
import_sum <- wildlife_trade %>% #import counts
  group_by(importer) %>% 
  summarize(import_count = n()) %>% 
  mutate(code = importer)
export_sum <- wildlife_trade %>% #export counts 
  group_by(exporter) %>% 
  summarize(export_count = n()) %>% 
  mutate(code = exporter)
country_codes <- read_csv('https://raw.githubusercontent.com/sdlam/ISO-3166-Countries-with-Regional-Codes/master/slim-2/slim-2.csv') %>% 
  select(country = name, code = 'alpha-2') #read in country code data to merge CITES data with geometry
world_sf <- read_sf(here('ne_50m_admin_0_countries', 'ne_50m_admin_0_countries.shp')) #read in geometry
world_subset_sf <- world_sf %>% #add country codes to geometry
  clean_names() %>% 
  select(country = sovereignt) %>% 
  merge(country_codes, by = 'country')
world_import_sf <- merge(world_subset_sf, import_sum, by = 'code') #merge geometry and CITES data
import_export_sf <- merge(world_import_sf, export_sum, by = 'code') %>% #make filterable for widget
  pivot_longer(cols = c("import_count", "export_count"))


#top3 wildlife terms for widget 3 
elephants <- read_csv(here('data','elephants.csv'))
oryx <- read_csv(here('data','oryx.csv'))
pythons <- read_csv(here('data','python.csv'))

top3 <- rbind(elephants, oryx, pythons) %>% 
  distinct() %>% 
  clean_names()

elephant_terms <- top3 %>%  
  filter(taxon == "Loxodonta africana") %>% 
  group_by(year,taxon, term) %>% 
  summarize(count = n()) %>% 
  slice_max(count, n = 5)

python_terms <- top3 %>% 
  filter(taxon =="Python reticulatus") %>% 
  group_by(year, taxon, term) %>% 
  summarize(count = n()) %>% 
  slice_max(count, n = 5)

oryx_terms <- top3 %>% 
  filter(taxon == "Oryx dammah") %>% 
  group_by(year, taxon, term) %>% 
  summarize(count = n()) %>% 
  slice_max(count, n = 5)

top3_terms <- rbind(elephant_terms, oryx_terms, python_terms) %>% 
  distinct() %>% 
  clean_names()


#wrangle for widget 2: select just columns we want to display from data frame 

#wrangle for widget 3: 


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
                 radioButtons("radio",
                              inputId = "import_export",
                              label = "Global Wildlife Importer and Exporters",
                              choices = c("Importers" = "import_count", "Exporters" = "export_count")
                                    ) # end radioButtons Input
               ), #end of sidebarPanel
               mainPanel(
                 "output goes here",
                 plotOutput(outputId = 'import_export_map')
                 ) # end of mainPanel
             ), #end of sidebarLayout
            ), #end of tabPanel for widget 1
    tabPanel("Widget 2",
             sidebarLayout(
               sidebarPanel(
                 "Widget 2 goes here",
                 checkboxGroupInput("checkGroup", 
                              inputId = "trade_purpose", 
                              label = h3("Select Trade Purpose"),
                              choices = list("Commercial" = "T",
                                             "Personal" = "P",
                                             "Scientific" = "S",
                                             "Hunting Trophy" = "H",
                                             "Circus/Traveling Exibition" = "Q")
                              ) # end CheckboxGroupInput
               ),#end sidebarPanel
               mainPanel(
                 "output goes here",
                 dataTableOutput("purpose_table")
               ) #end of mainPanel
             ) #end sidebarLayout
             ), #end tabPanel widget 4
    tabPanel("Widget 3",
             sidebarLayout(
               sidebarPanel(
                 "Visual Exploration of Traded Animal Products",
                 selectInput("select",
                             inputId = "pick_species",
                             label = h3("Select Species"),
                             choices = unique(top3_terms$taxon)
                 ) # end selectInput
                
               ), #end sidebarPanel
               mainPanel(
                 h3("Explore Top Traded Products for 3 Popular Traded Species"),
                 plotOutput('term_plot')
               ) #end of mainPanel
             ) #end sidebarLayout
    ), #end tabPanel for widget 3
  ) # this is end of navbarPage 

) #end ui


## create server function: 

server <- function(input, output) {

#Widget 1 reactive and output  
  import_export_select <- reactive({ #widget 1 reactive and output
    import_export_sf %>% 
     filter(name == input$import_export)
  }) #end import_export reactive
 
  output$import_export_map <- renderPlot({
    ggplot(data = import_export_select()) +
    geom_sf(aes(fill = value), color = 'white', size = 0.1) +
    scale_fill_gradient() +
    theme_void()
  })#end import_export_map output 

#Widget 2 reactive and output 

  purpose_select <-  reactive({
    wildlife_trade %>% 
      select(taxon:exporter, term, purpose) %>% 
      filter(purpose %in% c(input$trade_purpose))
  }) #end purpose_select reactive

 # purpose_select <-  reactive({
 #   get(input$trade_purpose)
 #   wildlife_trade %>% 
  #    select(taxon:exporter, term, purpose) %>% 
  #    filter(purpose %in% c(input$trade_purpose))
  #}) #end purpose_select reactive

  
  output$purpose_table <- DT::renderDataTable({
   purpose_filter <- subset(wildlife_trade, purpose == input$trade_purpose)
  })#end purpose plot output
  
  #Widget 3 reactive and output
  term_reactive <- reactive({
    top3_terms %>% 
      filter(taxon %in% input$pick_species)
  
  }) # end term_plot reactive
  
  #start output for term_plot plot
  output$term_plot <- renderPlot("Species Plot",{
    #start with elephant plot
    if(input$pick_species == "Loxodonta africana"){
    plot = ggplot(data = elephant_terms, 
                  aes(x = year, 
                      y = count)) +
      geom_line(aes(color = term)) +
      theme_minimal() +
      labs(title = "Time Series of Top Traded Elephant Products",
           x = "Year", y = "Count of Terms")} #end elephant plot output
    #start python plot
     if(input$pick_species == "Python reticulatus"){
      plot = ggplot(data = python_terms, 
                    aes(x = year, 
                        y = count)) +
        geom_line(aes(color = term)) +
        theme_minimal() +
        labs(title = "Time Series of Top Traded Reticulated Python Products",
             x = "Year", y = "Count of Terms")} # end python plot output
    #start oryx plot
    if(input$pick_species == "Oryx dammah"){
      plot = ggplot(data = oryx_terms, 
                    aes(x = year, 
                        y = count)) +
        geom_line(aes(color = term)) +
        theme_minimal() +
        labs(title = "Time Series of Top Traded Oryx  Products",
             x = "Year", y = "Count of Terms")} # end oryx plot output
    
    
     }) #end term_plot plot output
} #end all server

# Combine into an app
shinyApp(ui = ui, server = server)
