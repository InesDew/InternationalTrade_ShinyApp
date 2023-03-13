# Load packages ----------------------------------------------------------------
library(shiny)
library(data.table)
library(ggplot2)
library(igraph)
library(DT)
library(rjson)
library(bslib)

# Load data --------------------------------------------------------------------
dt.trade <- fread("../data/cleaned_trade_data.csv")
json_data <- fromJSON(file = '../data/sample.json')

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
  V(g)$continent <- unlist(json_data[V(g)$name])
  V(g)$continent[!V(g)$name %in% dt.year$reporter_name] <- NA
  
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
  
}

# Create a Shiny app object ----------------------------------------------------
shinyApp(ui = ui, server = server)
