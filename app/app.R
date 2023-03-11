# Load packages ----------------------------------------------------------------

library(shiny)
library(data.table)
library(ggplot2)
library(igraph) 

# Load data --------------------------------------------------------------------

dt.trade <- fread("../data/cleaned_trade_data.csv")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      # Select a year
      selectInput(
        inputId = "yearInput",
        label = "Year:",
        choices = c(levels(as.factor(dt.trade$year))),
        selected = "2017",
        multiple = FALSE
      )
    ),
    
    # Output: Show dataset
    plotOutput("network")
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$network <- renderPlot({
    # 1. Make edge list
    dt.trade.year <- dt.trade[year == input$yearInput]
    dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
    
    # 2. Convert edge list to an igraph network
    # igraph wants our data in matrix format
    m.trade <- as.matrix(dt.trade.edgelist) 
    g.trade <- graph_from_edgelist(m.trade, directed=TRUE)
    
    # 3. Set the weight of the edges (trade_value_usd)
    edge.attributes(g.trade)$weight <- dt.trade.year$trade_value_usd
    # Sum the weights for duplicate edges (we would need this if we select multiple years) TODO
    
    # 4. Add attributes to the vertices (continent)
    library(rjson)
    json_data <- fromJSON(file='../data/sample.json')
    V(g.trade)$continent <-  unlist(json_data[V(g.trade)$name])
    
    # 5. Plot igraph
    plot(g.trade, vertex.size = 5, vertex.label.cex = 0.3, edge.arrow.size=0.4)
  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)