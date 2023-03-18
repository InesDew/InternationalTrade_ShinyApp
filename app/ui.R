library(bslib)
library(leaflet)
library(DT)
library(ggplot2)
library(scales)
library(shinythemes)

# Define UI --------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    title = "International Trade Analysis",
    tabPanel("Trade Network Map",
             sidebarLayout(
               sidebarPanel(
                 #Adding logo to sidebar
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/World_Trade_Organization_%28logo_and_wordmark%29.svg/2560px-World_Trade_Organization_%28logo_and_wordmark%29.svg.png",width="100%"),
                 #Introducing Inputs for the user to select type of trade, country, minimum trade value and years to plot 
                 selectInput(inputId = "trader.map", 
                             label = "Select Imports or Exports:",
                             choices = c("Exports"="reporter_name","Imports"="partner_name"), 
                             selected = "reporter_name"
                 ),
                 selectInput(inputId = "country.map", 
                             label = "Select Country:",
                             choices = l.countries, 
                             selected = "Ghana"
                 ),
                 sliderInput(inputId = "weight.map",
                             label = "% of Maximum Trade Value:",
                             min = 0,
                             max = 100,
                             value = 0,
                             step = 1,
                             width = "90%"
                 ),
                 sliderInput(inputId = "year.map",
                             label = "Select Year Range:",
                             min = 2000,
                             max = 2021,
                             value = c(2018, 2021),
                             step = 1,
                             sep = "")
               ),
               # Output: Show network
               mainPanel(
                 leafletOutput("network.map"),
                 htmlOutput("text.map"),
                 DTOutput("centralities.map")
               )
             )
    ),
    
    tabPanel("Compare Countries",
             sidebarLayout(
               sidebarPanel(
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/World_Trade_Organization_%28logo_and_wordmark%29.svg/2560px-World_Trade_Organization_%28logo_and_wordmark%29.svg.png", width = "100%"),
                 selectizeInput(
                   inputId = "comp_countryInput",
                   label = "Country:",
                   choices = c("", levels(as.factor(dt.trade$reporter_name))),
                   selected = "Portugal",
                   multiple = TRUE,
                   options = list(maxItems = 3)
                 ),
                 selectInput("column", "KPI:", choices = c("Export Value in USD" = "export_value_usd",
                                                           "Import Value in USD" = "import_value_usd",
                                                           "Number of Exporting Partners" = "num_exporting_partners",
                                                           "Number of Importing Partners" = "num_importing_partners",
                                                           "Average Export Value per Partner" = "avg_export_value_usd",
                                                           "Average Import Value per Partner" = "avg_import_value_usd",
                                                           "Trade Balance in USD" = "trade_balance")),
                 sliderInput("year", "Year:", 
                             min = min(dt.merged$year), 
                             max = max(dt.merged$year), 
                             value = c(min(dt.merged$year), max(dt.merged$year)), 
                             step = 1,
                             sep = "")
               ),
               mainPanel(
                 leafletOutput("map"),
                 plotOutput("plot"),
                 p("Example:",
                   "The export value in USD from 2003 to 2020 for Germany, Italy, and Portugal refers to the monetary value of goods and services exported by these countries over that period of time.",
                   "It can be observed that Germany has significantly its exports in the period under review, while Italy and Portugal remain almost at the same level.",
                   "In comparison, it is interesting to see that the number of export partners of Germany and Italy is almost the same and that of Portugal is only a little lower.")

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
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/World_Trade_Organization_%28logo_and_wordmark%29.svg/2560px-World_Trade_Organization_%28logo_and_wordmark%29.svg.png", width = "80%"),
                 selectInput(
                   inputId = "des_continentInput",
                   label = "Select a continent:",
                   choices = c(
                     "Europe",
                     "Asia",
                     "Africa",
                     "North America",
                     "South America",
                     "Oceania",
                     "Antarctica"
                   ),
                   selected = c(
                     "Europe",
                     "Asia",
                     "Africa",
                     "North America",
                     "South America",
                     "Oceania",
                     "Antarctica"
                   ),
                   multiple = TRUE
                 ),
                 sliderInput(
                   "desc_yearInput",
                   "Year:",
                   min = min(dt.trade$year),
                   max = max(dt.trade$year),
                   value = c(min(dt.trade$year), max(dt.trade$year)),
                   step = 1,
                   sep = ""
                 ),
                 h4("Column names of data.trade:"),
                 textOutput("column_names"),
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Degree Distribution",plotOutput("degreeDist")),
                   tabPanel("Countries per Continent",plotOutput(outputId = "continent_count")),
                   tabPanel("Edge Value Distribution",plotOutput(outputId = "kpi_chart")),
                 ),
                 DTOutput("kpis_table")
               )
             )),
    tabPanel(
      "Data",
      tabsetPanel(
      tabPanel("Data Overview",
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
            choices = c("", levels(as.factor(
              dt.trade$partner_name
            ))),
            selected = "",
            multiple = FALSE
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "filter_reporter",
            label = "Reporter:",
            choices = c("", levels(as.factor(
              dt.trade$reporter_name
            ))),
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
      DTOutput("data_table", width = "100%")
    ),
    tabPanel("Data Source",
      htmlOutput("data.source.info"),
      DTOutput("data.columns")
    )
      )
    )
  )
)