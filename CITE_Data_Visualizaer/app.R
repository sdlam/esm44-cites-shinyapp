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
library(plotly)

#read in data
wildlife_trade <- read_csv(here("data", "cites_trade_data.csv")) %>% 
  clean_names()
elephants <- read_csv(here('data','elephants.csv'))
oryx <- read_csv(here('data','oryx.csv'))
pythons <- read_csv(here('data','python.csv'))

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

#wrangle for widget 2: select just columns we want to display from data frame 
purpose_trade <- wildlife_trade %>% 
  select(year, taxon, class, order, family, genus, importer, exporter, term, purpose)
  
#top3 wildlife terms for widget 3 
char_megafauna <- rbind(elephants, oryx, pythons) %>%
  distinct() %>% 
  clean_names()

elephant_terms <- char_megafauna %>%  
  filter(taxon == "Loxodonta africana") %>% 
  group_by(year,taxon, term) %>% 
  summarize(count = n()) %>% 
  slice_max(count, n = 5)

python_terms <- char_megafauna %>% 
  filter(taxon =="Python reticulatus") %>% 
  group_by(year, taxon, term) %>% 
  summarize(count = n()) %>% 
  slice_max(count, n = 5)

oryx_terms <- char_megafauna %>% 
  filter(taxon == "Oryx dammah") %>% 
  group_by(year, taxon, term) %>% 
  summarize(count = n()) %>% 
  slice_max(count, n = 5)

megafauna_terms <- rbind(elephant_terms, oryx_terms, python_terms) %>% 
  distinct() %>% 
  clean_names()

## create user interface 
ui <- fluidPage(theme = bs_theme(bootswatch = "yeti"),
  navbarPage(
    "Wildlife Trade Visualization", #app title
    tabPanel("Overview", 
            mainPanel(
              "The global wildlife trade is at the heart of the tension between conservation of biodiversity and human development. ",
              "Whether for medicine, food, culture, or construction, a large proportion of our economy and way of life is reliant on wildlife products",
              "International wildlife trade has both positive and negative consequences, depending on the species being traded.",
              "It can enhance rural economies and provide incentives for sustainable use and management of species.",
              "However, it can lead to overharvest, and can negatively impact ecosystems. High value trade can also marginalize poor communities and can create dependence on unsustainable levels of harvest.",
              br(),
              br(),
              "This app uses CITES wildlife trade data from 2012 to 2021 to visualize patterns and trends within the legal international wildlife trade. ",
              "App created by Sarah Lam and Ali Martin, 2022",
              br(),
              br(),
              tags$img(src = "Elephant.jpeg"), 
              "Image source: CITES Elephants page"
            ) #end mainPanel
             ), #end tabPanel
    tabPanel("Import/Export Map",
             sidebarLayout(
               sidebarPanel(
                 'Global Wildlife Importer and Exporters',
                 radioButtons("radio",
                              inputId = "import_export",
                              label = "Select Exchange",
                              choices = c("Importers" = "import_count", "Exporters" = "export_count")
                                    ) # end radioButtons Input
               ), #end of sidebarPanel
               mainPanel(
                 "Map of Imports and Exports for Wildlife Species",
                 plotOutput(outputId = 'import_export_map'),
                 br(),
                 p('This widget shows a global map of the countries with the highest amount of imports and exports of wildlife species and products')
                 ) # end of mainPanel
             ), #end of sidebarLayout
            ), #end of tabPanel for widget 1
    tabPanel("Top Trade Purpose Data",
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
                 "Purposes of Traded Wildlife",
                 dataTableOutput("purpose_table"),
                 br(),
                 p("This widget provides an interactive table to visualize the different purposes of traded wildlife species. ")
               ) #end of mainPanel
             ) #end sidebarLayout
             ), #end tabPanel widget 4
    tabPanel("Traded Animal Products",
             sidebarLayout(
               sidebarPanel(
                 "Visual Exploration of Traded Animal Products",
                 radioButtons("radio",
                             inputId = "pick_species",
                             label = h3("Select Species"),
                             choices = c("African Elephant" = "Loxodonta africana",
                                         "Reticulated Python" = "Python reticulatus",
                                         "Oryx" = "Oryx dammah")
                             ) # end selectInput
               ), #end sidebarPanel
             mainPanel(
                 h3("Explore Top Traded Products for 3 Popular Traded Species"),
                 plotlyOutput("term_plot"),
                 br(),
                 p("This widget visualizes the top traded animal products for three of the most traded wildlife species over the course of ten years from 2012 to 2022. Source: ")
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

#Widget 2 output 
  output$purpose_table <- DT::renderDataTable({
   purpose_filter <- subset(purpose_trade, purpose == input$trade_purpose)
  })#end purpose plot output
  
# Widget 3 reactive and output
 #term reactive
  term_reactive <- reactive({
    top3_terms %>% 
      filter(taxon == c(input$pick_species))
    }) # end term reactive
  
  #start output for term_plot plot
  output$term_plot <- renderPlotly({
    ggplot(data = term_reactive(), aes(x = year, y = count)) +
      geom_line(aes(color = term)) +
      theme_minimal() +
      labs(title = "Time Series of Top Traded Wildlife Products for Well Known Species",
           x = "Year", y = "Count of Terms")
     }) #end term_plot plot output
} #end all server

# Combine into an app
shinyApp(ui = ui, server = server)
