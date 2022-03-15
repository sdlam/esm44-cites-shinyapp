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
library(leaflet)
library(tmap)
library(paletteer)

#read in data
wildlife_trade <- read_csv(here("data", "cites_trade_data.csv")) %>% 
  clean_names()
elephants <- read_csv(here('data','elephants.csv'))
oryx <- read_csv(here('data','oryx.csv'))
pythons <- read_csv(here('data','python.csv'))
# import taxon data
taxon_imports <- read_csv(here("data","taxon_imports.csv")) %>% 
  clean_names() 
taxon_exports <- read_csv(here("data","taxon_exports.csv")) %>% 
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

## wrangle data for widget 1: Graphs
taxon_imports_clean <-  select(taxon_imports, -c('taxon','filters')) %>% 
  mutate(exchange_status = "import_count") %>% 
  mutate(common_name =
           case_when(taxonomic_group == 'Psittacus erithacus' ~ 'Grey Parrot',
                     taxonomic_group == 'Poicephalus senegalus' ~ 'Senegal Parrot',
                     taxonomic_group == 'Panthera pardus' ~ 'Leopard',
                     taxonomic_group == 'Lutra lutra' ~ 'Eurasian Otter'))

taxon_exports_clean <- select(taxon_exports, -c('taxon','filters')) %>% 
  mutate(exchange_status = "export_count") %>% 
  mutate(common_name =
           case_when(taxonomic_group == 'Caiman crocodilus fuscus' ~ 'Brown Caiman',
                     taxonomic_group == 'Caretta caretta' ~ 'Loggerhead Sea Turtle',
                     taxonomic_group == 'Boa constrictor' ~ 'Boa Constrictor',
                     taxonomic_group == 'Testudo horsfieldii' ~ "Horsefield's Tortoise"))

## only get top 4 exports
top4_exports <- taxon_exports_clean %>%
  slice_max(n = 4, quantity) 
## join dataframe for widget
import_export_top4 <- full_join(taxon_imports_clean, top4_exports)

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
ui <- fluidPage(theme = bs_theme(bootswatch = "sandstone"),
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
              br(),
              "Image source: CITES Elephants page"
            ) #end mainPanel
             ), #end tabPanel
    tabPanel("Import/Export Map",
             sidebarLayout(
               sidebarPanel(
                 'Global Wildlife Importer and Exporters',
                 radioButtons(
                              inputId = "import_export",
                              label = "Select Exchange",
                              choices = c("Importers" = "import_count", "Exporters" = "export_count"),
                              selected = "import_count"
                                    ) # end radioButtons Input
               ), #end of sidebarPanel
               mainPanel(
                 "Map of Imports and Exports for Wildlife Species",
                 plotlyOutput(outputId = 'import_export_map'),
                 br(),
                 plotOutput(outputId = "import_export_graph"),
                 p('This widget shows a global map of the countries with the highest amount of imports and exports of wildlife species and products')
                 ) # end of mainPanel
             ), #end of sidebarLayout
            ), #end of tabPanel for widget 1
    tabPanel("Top Trade Purpose Data",
             sidebarLayout(
               sidebarPanel(
                 "Trade Purpose data",
                 selectInput("select",
                             inputId = "pick_term",
                             label = h3("Select Trade Term"),
                             choices = c("Live" = "live",
                                         "Leather Product" = "leather products (small)", 
                                         "Caviar" = "caviar",                
                                         "Cosmetics" = "cosmetics",               
                                         "Trophies" = "trophies", 
                                         "Skins" = "skins",                    
                                         "Specimens" = "specimens",                
                                         "Ivory Carvings" = "ivory carvings",          
                                         "Roots" = "roots",                    
                                         "Skin Pieces" = "skin pieces")
                 ), # end selectInput
                 checkboxGroupInput("checkGroup", 
                              inputId = "trade_purpose", 
                              label = h3("Select Trade Purpose"),
                              choices = list("Commercial" = "T",
                                             "Personal" = "P",
                                             "Scientific" = "S",
                                             "Hunting Trophy" = "H",
                                             "Circus/Traveling Exibition" = "Q",
                                             "Judicial/Forensic" = "L", 
                                             "Captive Breeding" = "B", 
                                             "Zoo" = "Z",
                                             "Educational" = "E",
                                             "Reintroduction to Wild" = "N",
                                             "Medical" = "M")
                              ) # end CheckboxGroupInput
               ),#end sidebarPanel
               mainPanel(
                 "Terms and Purposes of Traded Wildlife",
                 plotOutput(outputId = "top_term_plot"),
                 dataTableOutput("purpose_table"),
                 br(),
                 p("This widget provides an interactive table to visualize the most common purposes of traded wildlife species. ")
               ) #end of mainPanel
             ) #end sidebarLayout
             ), #end tabPanel widget 2
    tabPanel("Traded Animal Products",
             sidebarLayout(
               sidebarPanel(
                 "Visual Exploration of Traded Animal Products",
                 selectInput("select",
                             inputId = "pick_species",
                             label = h3("Select Species"),
                             choices = c("African Elephant" = "Loxodonta africana",
                                         "Reticulated Python" = "Python reticulatus",
                                         "Oryx" = "Oryx dammah")
                             ) # end selectInput
               ), #end sidebarPanel
             mainPanel(
                 h3("Explore Top Traded Products for 3 Popular Traded Species"),
                 plotlyOutput(outputId = "term_plot"),
                 br(),
                 p("This widget visualizes the top traded animal products for three of the most traded wildlife species over the course of ten years from 2012 to 2022. Elephants, Pythons, and Oryxes are highly sought after in the wildlife trade industry for their animal products. ")
               ) #end of mainPanel
             ) #end sidebarLayout
    ), #end tabPanel for widget 3
  ) # this is end of navbarPage 
) #end ui


## create server function: 

server <- function(input, output) {
### Widget 1 reactive and output  
## Widget 1 Map Reactive 
  import_export_select <- reactive({ #widget 1 reactive and output
    import_export_sf %>% 
     filter(name == input$import_export)
  }) #end import_export reactive
## Widget 1 map output 
 output$import_export_map <- renderPlotly({
    ggplot(data = import_export_select()) +
     geom_sf(data = world_sf, color = "grey4", size = 0.1) +
     geom_sf(aes(fill = value), color = 'white', size = 0) +
     scale_fill_gradient(high = "#132B43", low = "lightcyan2") +
     labs(fill = "Count") +
     theme_void()
   }) #end output for map
  
##  tab 1 Import/export Graph reactive
  import_export_taxon <- reactive({
    import_export_top4 %>% 
      filter(exchange_status == input$import_export)
  }) #end import graph reactive
## tab 1 import/export graph output 
  output$import_export_graph <- renderPlot({
    ggplot(data = import_export_taxon(), 
           aes(x = reorder(taxonomic_group, quantity), y = quantity, fill = common_name)) +
      geom_col() +
      scale_fill_manual(values=c('azure','lightcyan2','lightskyblue3','lightskyblue4')) +
      labs(x = "Taxonomic group", y = "Count of Imported Individuals") +
      ggtitle("Most Imported Species for 2021-2022") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + NULL
  }) ## END IMPORT EXPORT FOR WIDGET 1

#Tab 2 
  #top terms widget reactive
  top_term_reactive <- reactive({
    wildlife_trade %>% 
      filter(term == input$pick_term) %>% 
      group_by(taxon) %>%  
      summarize(count = n()) %>% 
      slice_max(count, n = 5)
  }) # end term reactive
  
  #start output for top term plot
  output$top_term_plot <- renderPlot({
    ggplot(data = top_term_reactive(), aes(x = count, y = reorder(taxon, count))) +
      geom_col(aes(fill = count)) +
      scale_fill_gradient(low = "azure1", high = "cadetblue4") +
      theme_minimal() +
      labs(title = "Most Commonly Traded Species for Given Trade Term",
           x = "Count of Terms", y = "Species Taxon")
  }) #end term_plot plot output
  
  #filter widget output 
  output$purpose_table <- DT::renderDataTable({
   purpose_filter <- subset(purpose_trade, purpose == input$trade_purpose)
  })#end purpose plot output
  
# Widget 3 reactive and output
 #term reactive
  term_reactive <- reactive({
    megafauna_terms %>% 
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
