# Load packages ----------------------------------------------------------------
library(shiny)
library(data.table)
library(ggplot2)
library(igraph)
library(DT)
library(rjson)
library(bslib)
library(leaflet)

# Load data --------------------------------------------------------------------
dt.trade <- fread("../data/cleaned_trade_data.csv")
json_data <- fromJSON(file = '../data/sample.json')

#Prepare data for Compare Countries--------------------------------------------

# Aggregate the data by year and  reporter_name to get the export value for each country
dt.export <- dt.trade[, .(export_value_1000_usd = sum(trade_value_1000_usd), num_exporting_partners = .N, reporter_lat = unique(reporter_lat)
                          ,reporter_long = unique(reporter_long)), by = c("year", "reporter_name")]

# Calculate the average export value per country per year
dt.export[, avg_export_value_1000_usd := export_value_1000_usd / num_exporting_partners]

# Aggregate the data by year and partner_name to get the import value for each country
dt.import <- dt.trade[, .(import_value_1000_usd = sum(trade_value_1000_usd), 
                          num_importing_partners = .N), 
                      by = c("year", "partner_name")]

# Calculate the average import value per country per year
dt.import[, avg_import_value_1000_usd := import_value_1000_usd / num_importing_partners]


# Merge import and export datatable
dt.merged <- merge(dt.export, dt.import, 
                   by.x = c("reporter_name", "year"), 
                   by.y = c("partner_name", "year"))

# Calculate trade balance
dt.merged$trade_balance <- dt.merged$export_value_1000_usd - dt.merged$import_value_1000_usd

# Helper Functions -------------------------------------------------------------
# Create graph from trade data
create_trade_graph <- function(dt, year_input, continent_input) {
  # Subset data to selected year
  dt.year <- dt[year == year_input]
  
  # Get edge list
  dt.edgelist <- dt.year[, c('reporter_name', 'partner_name')]
  
  # Convert to igraph
  m <- as.matrix(dt.edgelist)
  g <- graph_from_edgelist(m, directed = TRUE)
  
  # Set edge weights
  edge.attributes(g)$weight <- dt.year$trade_value_usd
  
  # Set vertex attributes
  V(g)$continent <-  unlist(json_data[V(g)$name])
  
  # Filter vertices by continent
  if (!is.null(continent_input)) {
    g <- induced_subgraph(g, V(g)$continent %in% continent_input)
  }
  
}

# Define UI --------------------------------------------------------------------
ui <- fluidPage(
  # App title
  titlePanel("Shiny App for International Trade"),
  
  navbarPage(
    title = "Network Analytics",
    tabPanel("Compare Countries",
             sidebarLayout(
               sidebarPanel(
                 selectInput("country", "Country:", choices = c("", sort(unique(dt.merged$reporter_name))), selected = NULL),
                 selectInput("country2", "Country 2:", choices = c("", sort(unique(dt.merged$reporter_name))), selected = NULL),
                 selectInput("country3", "Country 3:", choices = c("", sort(unique(dt.merged$reporter_name))), selected = NULL),
                 selectInput("column", "KPI:", choices = c("Export Value" = "export_value_1000_usd",
                                                           "Average Export Value per Partner" = "avg_export_value_1000_usd",
                                                           "Import Value" = "import_value_1000_usd",
                                                           "Number of Exporting Partners" = "num_exporting_partners",
                                                           "Number of Importing Partners" = "num_importing_partners",
                                                           "Average Import Value per Partner" = "avg_import_value_1000_usd",
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
    tabPanel("Clusteranalyse",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "filter_reporter",
                   label = "Reporter:",
                   choices = c("", levels(as.factor(
                     dt.trade$reporter_name
                   ))),
                   selected = "",
                   multiple = FALSE
                 ),
               ),
               mainPanel(
                 # add output here
                 )
             )),
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
                 
                 actionButton("submit_desc", "Submit")
               ),
               mainPanel(
                 plotOutput("degreeDist"),
                 plotOutput(outputId = "kpi_chart"),
                 DTOutput("kpis_table")
                 # Add output tables and plots here
               )
             )),
    tabPanel("Data",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "filter_reporter",
                   label = "Reporter:",
                   choices = c("", levels(as.factor(
                     dt.trade$reporter_name
                   ))),
                   selected = "",
                   multiple = FALSE
                 ),
                 selectInput(
                   inputId = "filter_partner",
                   label = "Partner:",
                   choices = c("", levels(as.factor(
                     dt.trade$partner_name
                   ))),
                   selected = "",
                   multiple = FALSE
                 ),
                 selectInput(
                   inputId = "filter_year",
                   label = "Year:",
                   choices = c("", levels(as.factor(dt.trade$year))),
                   selected = "",
                   multiple = FALSE
                 ),
               ),
               mainPanel(DTOutput("data_table"))
             )),
    
    theme = bs_theme(
      bg = "white",
      fg = "black",
      primary = "maroon",
      base_font = font_google("Montserrat")
    )
  )
)
# Define server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  # Data table output
  output$data_table <- renderDT({
    # Filter data
    filtered_data <- dt.trade
    if (input$filter_reporter != "") {
      filtered_data <-
        filtered_data[reporter_name %in% input$filter_reporter]
    }
    if (input$filter_partner != "") {
      filtered_data <-
        filtered_data[partner_name %in% input$filter_partner]
    }
    if (input$filter_year != "") {
      filtered_data <- filtered_data[year == input$filter_year]
    }
    
    datatable(
      filtered_data[, c(
        "reporter_name",
        "partner_name",
        "trade_value_usd",
        "reporter_continent",
        "partner_continent",
        "year"
      )],
      filter = "top"
    )
  })
  
  # Histogram output
  output$degreeDist <- renderPlot({
    # Create trade graph
    g <- create_trade_graph(dt.trade, input$desc_yearInput, input$des_continentInput)
    
    # Plot degree distribution
    hist(
      degree(g),
      main = "Degree Distribution of selected Countries",
      xlab = "Degree",
      ylab = "Count",
      col = "#48ADF0",
      border = "black",
      breaks = seq(0, max(degree(g)) + 1, by = 1)
    )
  })
  
  # KPI table output
  output$kpis_table <- renderDT({
    # Create trade graph
    g <- create_trade_graph(dt.trade, input$desc_yearInput, input$des_continentInput)
    
    # Calculate KPIs and store in a data frame
    kpi_df <- data.frame(
      NumNodes = vcount(g),
      NumEdges = ecount(g),
      MeanDegree = mean(degree(g)),
      MedianDegree = median(degree(g)),
      AvgEdgeValueUSD = mean(E(g)$weight),
      MedianEdgeValueUSD = median(E(g)$weight),
      MinEdgeValueUSD = min(E(g)$weight),
      MaxEdgeValueUSD = max(E(g)$weight),
      StdDevEdgeValueUSD = sd(E(g)$weight)
    )
    # Transpose the table
    kpi_df_t <- t(kpi_df)
    
    kpi_df_t
  })
  
  # KPI chart output
  output$kpi_chart <- renderPlot({
    # Create trade graph
    g <- create_trade_graph(dt.trade, input$desc_yearInput, input$des_continentInput)
    # 6. Create a histogram of edge values
    plot <- hist(
      E(g)$weight,
      main = "Edge Value Distribution",
      xlab = "Trade Value (USD)",
      ylab = "Count",
      col = "#48ADF0",
      border = "black",
      breaks = seq(0, max(E(g)$weight) + 1e9, by = 1e9),
      xlim = c(0, max(E(g)$weight))
    )
    
    plot
  })

  #Compare Countries------------------------------------------------------------
  
  # Create a reactive data frame to filter the data based on the user inputs
  filtered_data <- reactive({
    dt.merged[reporter_name == input$country & year >= input$year[1] & year <= input$year[2]]
  })
  
  # Create reactive data frames for the second and third country selections
  filtered_data2 <- reactive({
    if (!is.null(input$country2)) {
      dt.merged[reporter_name == input$country2 & year >= input$year[1] & year <= input$year[2]]
    } else {
      NULL
    }
  })
  
  filtered_data3 <- reactive({
    if (!is.null(input$country3)) {
      dt.merged[reporter_name == input$country3 & year >= input$year[1] & year <= input$year[2]]
    } else {
      NULL
    }
  })
  
  # Create the plot based on the filtered data
  output$plot <- renderPlot({
    if (!is.null(filtered_data2()) & !is.null(filtered_data3())) {
      ggplot() +
        geom_line(data = filtered_data(), aes_string(x = "year", y = input$column, color = "'Country 1'")) +
        geom_line(data = filtered_data2(), aes_string(x = "year", y = input$column, color = "'Country 2'")) +
        geom_line(data = filtered_data3(), aes_string(x = "year", y = input$column, color = "'Country 3'")) +
        labs(x = "Year",
             y = input$column) +
        scale_color_manual(name = "Country", 
                           values = c("Country 1" = "red", "Country 2" = "blue", "Country 3" = "green"),
                           labels = c(input$country, input$country2, input$country3))
    } else if (!is.null(filtered_data2())) {
      ggplot() +
        geom_line(data = filtered_data(), aes_string(x = "year", y = input$column, color = "'Country 1'")) +
        geom_line(data = filtered_data2(), aes_string(x = "year", y = input$column, color = "'Country 2'")) +
        labs(x = "Year",
             y = input$column) +
        scale_color_manual(name = "Country", values = c("Country 1" = "red", "Country 2" = "blue"),
                           labels = c(input$country, input$country2))
    } else {
      ggplot(filtered_data(), aes_string(x = "year", y = input$column, color = "'Country 1'")) +
        geom_line() +
        labs(x = "Year",
             y = input$column) +
        scale_color_manual(name = "Country", 
                           values = c("Country 1" = "red"),
                           labels = c(input$country))
    }
  })
  
  # Create the map based on the filtered data
  output$map <- renderLeaflet({
    # Create a data frame with only the selected countries' coordinates
    selected_countries <- unique(dt.merged[reporter_name %in% c(input$country, input$country2, input$country3),
                                           c("reporter_name","reporter_lat", "reporter_long")])
    # Create a leaflet map centered on the selected countries or the world
    if (nrow(selected_countries) > 0) {
      leaflet() %>%
        addProviderTiles("Stamen.Toner", options = providerTileOptions(noWrap = TRUE, zoomSnap = 0, 
                                                                       attributionControl = FALSE, 
                                                                       backgroundColor = "#f2f2f2",opacity = 
                                                                         0.4)) %>%
        addCircleMarkers(data = selected_countries, lng = ~reporter_long, lat = ~reporter_lat, label = ~reporter_name, radius = 10,
                         color = "red",
                         fillColor = "red",
                         fillOpacity = 0.8,
                         stroke = FALSE)
    } else {
      leaflet() %>%
        addProviderTiles("Stamen.Toner", options = providerTileOptions(noWrap = TRUE, zoomSnap = 0, 
                                                                       attributionControl = FALSE, 
                                                                       backgroundColor = "#f2f2f2",opacity = 
                                                                         0.4)) %>%
        setView(lng = 0, lat = 0, zoom = 2)
    }
  })
}



# Create a Shiny app object ----------------------------------------------------
shinyApp(ui = ui, server = server)
