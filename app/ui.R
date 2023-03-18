library(bslib)
library(leaflet)
library(DT)
library(ggplot2)
library(scales)

# Define UI --------------------------------------------------------------------
ui <- fluidPage(
  # App title
  titlePanel("Shiny App for International Trade"),
  
  navbarPage(
    title = "Network Analytics",
    
    tabPanel("Trade Network Map",  
             sidebarLayout(
               sidebarPanel(
                 #Adding logo to sidebar
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/World_Trade_Organization_%28logo_and_wordmark%29.svg/2560px-World_Trade_Organization_%28logo_and_wordmark%29.svg.png",width="100%"),
                 #Introducing Inputs for the user to select type of trade, country, minimum trade value and years to plot 
                 selectInput(inputId = "trader", 
                             label = "Select Imports or Exports:",
                             choices = c("Exports"="reporter_name","Imports"="partner_name"), 
                             selected = "reporter_name"
                 ),
                 selectInput(inputId = "countrymap", 
                             label = "Select Country:",
                             choices = l.countries, 
                             selected = "Ghana"
                 ),
                 sliderInput(inputId = "MinWeight",
                             label = "Minimum Trade Value (USD):",
                             min = 0,
                             max = 20000000000,
                             value = 0,
                             step = 1000000,
                             width = "90%"
                 ),
                 sliderInput(inputId = "year_range",
                             label = "Select Year Range:",
                             min = 2000,
                             max = 2021,
                             value = c(2018, 2021),
                             step = 1,
                             sep = "")
               ),
               # Output: Show network
               mainPanel(
                 leafletOutput("map1"),
                 DTOutput("datamap")
               )
             )
    ),
    
    tabPanel("Compare Countries",
             sidebarLayout(
               sidebarPanel(
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/World_Trade_Organization_%28logo_and_wordmark%29.svg/2560px-World_Trade_Organization_%28logo_and_wordmark%29.svg.png", width = "100%"),
                 selectInput("country", "Country:", choices = c("", sort(unique(dt.merged$reporter_name))), selected = NULL),
                 selectInput("country2", "Country 2:", choices = c("", sort(unique(dt.merged$reporter_name))), selected = NULL),
                 selectInput("country3", "Country 3:", choices = c("", sort(unique(dt.merged$reporter_name))), selected = NULL),
                 selectInput("column", "KPI:", choices = c("Export Value" = "export_value_usd",
                                                           "Average Export Value per Partner" = "avg_export_value_usd",
                                                           "Import Value" = "import_value_usd",
                                                           "Number of Exporting Partners" = "num_exporting_partners",
                                                           "Number of Importing Partners" = "num_importing_partners",
                                                           "Average Import Value per Partner" = "avg_import_value_usd",
                                                           "Trade Balance" = "trade_balance")),
                 sliderInput("year", "Year:", 
                             min = min(dt.merged$year), 
                             max = max(dt.merged$year), 
                             value = c(min(dt.merged$year), max(dt.merged$year)), 
                             step = 1,
                             sep = "")
               ),
               mainPanel(
                 leafletOutput("map"),
                 plotOutput("plot")
               )
             )
    ),
    tabPanel("Community Analysis",
             sidebarLayout(
               sidebarPanel(
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/World_Trade_Organization_%28logo_and_wordmark%29.svg/2560px-World_Trade_Organization_%28logo_and_wordmark%29.svg.png", width = "100%"),
                 # Set year range
                 selectInput(inputId = "CommYear",
                             label = "Select Year Range:",
                             choices = c('', levels(as.factor(dt.trade$year))),
                             selected = '2021',
                             multiple = FALSE),
                 h6("The map visualization takes some time to load, please wait"),
                 htmlOutput("CommTextKPI")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Worldmap Plot",
                            leafletOutput("CommMap"),
                            htmlOutput("CommTextMap")
                   ),
                   tabPanel("Network Plot", 
                            plotOutput("CommNetwork"),
                            dataTableOutput("CommCountries")
                   ),
                   tabPanel("Modularity over Time",
                            plotOutput("CommModularity"),
                            htmlOutput("CommTextModularity")
                   ),
                   tabPanel("Walktrap Algorithm", 
                            htmlOutput("CommTextWalktrap")
                   )
                 )
               )
             )
    ),
    tabPanel("Descriptives",
             sidebarLayout(
               sidebarPanel(
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/World_Trade_Organization_%28logo_and_wordmark%29.svg/2560px-World_Trade_Organization_%28logo_and_wordmark%29.svg.png", width = "100%"),
                 selectInput(
                   inputId = "desc_yearInput",
                   label = "Year:",
                   choices = c("", levels(as.factor(dt.trade$year))),
                   selected = "2021",
                   multiple = FALSE
                 ),
                 selectInput(
                   inputId = "des_continentInput",
                   label = "Select a continent:",
                   choices = c(
                     "Asia",
                     "Europe",
                     "Africa",
                     "North America",
                     "South America",
                     "Australia"
                   ),
                   selected = c(
                     "Asia",
                     "Europe",
                     "Africa",
                     "North America",
                     "South America",
                     "Australia"
                   ),
                   multiple = TRUE
                 ),
                 DTOutput("kpis_table")
               ),
               mainPanel(
                 h4("Column names of data.trade:"),
                 textOutput("column_names"),
                 plotOutput("degreeDist"),
                 plotOutput(outputId = "continent_count"),
                 plotOutput(outputId = "kpi_chart"),
               )
             )),
    tabPanel("Data",
             fluidRow(
               column(
                 width = 3,
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/World_Trade_Organization_%28logo_and_wordmark%29.svg/2560px-World_Trade_Organization_%28logo_and_wordmark%29.svg.png", width = "100%"),
               ),
               column(
                 width = 3,
                 selectInput(
                   inputId = "filter_partner",
                   label = "Partner:",
                   choices = c("", levels(as.factor(dt.trade$partner_name))),
                   selected = "",
                   multiple = FALSE
                 )
               ),
               column(
                 width = 3,
                 selectInput(
                   inputId = "filter_reporter",
                   label = "Reporter:",
                   choices = c("", levels(as.factor(dt.trade$reporter_name))),
                   selected = "",
                   multiple = FALSE
                 )
               ),
               column(
                 width = 3,
                 selectInput(
                   inputId = "filter_year",
                   label = "Year:",
                   choices = c("", levels(as.factor(dt.trade$year))),
                   selected = "",
                   multiple = FALSE
                 )
               ),
               style = "margin-bottom: 10px;",
             ),
             DTOutput("data_table",width = "100%")
    ),
    
    
    theme = bs_theme(
      bg = "white",
      fg = "black",
      primary = "maroon",
      base_font = font_google("Montserrat")
    )
  )
)