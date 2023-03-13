library(shiny)
library(ggplot2)
library(data.table)
library(leaflet)


# Read the data
dt.trade <- fread("../data/cleaned_trade_data.csv")

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


# Define the UI
ui <- fluidPage(titlePanel("Trade Data"),
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
)

# Define the server
server <- function(input, output) {
  
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
  


# Run the app
shinyApp(ui, server)


