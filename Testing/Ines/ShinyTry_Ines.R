# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(data.table)
library(igraph)

# Load data --------------------------------------------------------------------

dt.trade <- fread("cleaned_trade_data.csv")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(
        inputId = "countryInput",
        label = "Reporting country:",
        choices = c(levels(as.factor(dt.trade$reporter_name))),
        multiple = FALSE
      ),
      # Select variable for x-axis
      selectInput(
        inputId = "yearInput",
        label = "Year:",
        choices = c(levels(as.factor(dt.trade$year))),
        selected = "2017",
        multiple = FALSE
      )
    ),
    
    # Output: Show dataset
    DT::dataTableOutput("table"),
    plotOutput("network")
  )
)
    
    # Define server ----------------------------------------------------------------
    
    server <- function(input, output, session) {
      # Filter data based on selections
      output$table <- DT::renderDataTable(DT::datatable({
        data <- dt.trade
          data <- data[data$reporter_name == input$countryInput,]
          data <- data[data$year == input$yearInput,]
        data
      }))
      
      # Plot the network with weights
      output$network <- renderPlot({
        data <- dt.trade
        data <- data[data$reporter_name == input$countryInput,]
        data <- data[data$year == input$yearInput,]
        
        # Set the vertices
        dt.all.reporters <- data[, list(name=reporter_name, type=FALSE)]
        dt.all.partners <- data[, list(name=partner_name, type=TRUE)]
        dt.all.vertices <- rbind(dt.all.reporters, dt.all.partners)
        
        # Create & plot the graph
        g <- graph.data.frame(data[, list(reporter_name, partner_name)], directed=TRUE, vertices=dt.all.vertices)
        
        # Add weights to the graph & plot
        g <- set_edge_attr(g, "weight", value=data$trade_value_usd)

        plot(g, vertex.size = 5)
        })
      }
    # Create a Shiny app object ----------------------------------------------------
    
    shinyApp(ui = ui, server = server)
    