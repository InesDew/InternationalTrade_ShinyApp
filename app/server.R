library(igraph)
library(rgdal)

# Define server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  # Map output
  output$network.map <- renderLeaflet({
    
    # Set the parameters of the inputs to select years, minimum trade, reporter 
    # and partner name. This is done with an if statement depending if user 
    # selected Exports or Imports 
    
    if (input$trader.map == "reporter_name") {
      dt.trade.country <- dt.trade[dt.trade$reporter_name == input$country.map, ]
      dt.trade.country.year <- dt.trade.country[dt.trade.country$year >= 
                                                  input$year.map[1] &
                                                  dt.trade.country$year <= 
                                                  input$year.map[2], ]
      dt.trade.country.year <- dt.trade.country.year[dt.trade.country.year
                                                     $trade_value_usd / 
                                                       max(dt.trade.country.year
                                                           $trade_value_usd) * 
                                                       100 >= input$weight.map, ]
      
      
    } else {
      dt.trade.country <- dt.trade[ dt.trade$partner_name == input$country.map, ]
      dt.trade.country.year <- dt.trade.country[dt.trade.country$year >= 
                                                  input$year.map[1] &
                                                  dt.trade.country$year <= 
                                                  input$year.map[2], ]
      dt.trade.country.year <- dt.trade.country.year[dt.trade.country.year$
                                                       trade_value_usd / 
                                                       max(dt.trade.country.year$
                                                             trade_value_usd) * 
                                                       100 >= input$weight.map, ]
      
    }
    
    #creating dataframe from the data table and defining from and to parameters
    df <- data.frame("from" = dt.trade.country.year$reporter_name,
                     "to" = dt.trade.country.year$partner_name)
    
    #checking if there is data from the inputs demanded my the user, 
    #if not, map shows a message saying that no data is available
    
    if (nrow(df) == 0) {
      popup <- paste0("No data available for selected country")
      return(leaflet() %>% addTiles() %>% setView(lng = 0, lat = 0, zoom = 2) %>%
               addPopups(lng = 0, lat = 0, popup = popup))
    }
    
    # Creating graph with data frame where the vertices are the dt.meta 
    #data table containing all the countries with their longitude and latitude 
    #as previously explained 
    g <- graph_from_data_frame(df, directed = TRUE, vertices = dt.meta)
    
    # Setting weights as the trade value 
    g <- set_edge_attr(g, "weight", 
                       value = dt.trade.country.year$trade_value_usd)
    # Sum the weights for duplicate edges (we would need this 
    #if we select multiple years)
    g <- simplify(g, remove.multiple = TRUE, 
                  edge.attr.comb = list(weight = "sum", year = "concat"))
    
    # Sum the weights for duplicate edges (we would need this 
    #if we select multiple years)
    g <- simplify(g, remove.multiple = TRUE, 
                  edge.attr.comb = list(weight = "sum", year = "concat"))
    
    # Remove vertices with degree 0
    g <- delete.vertices(g, which(degree(g) == 0))
    
    # Retrieving data frame form the graph to get the coordinates of the 
    #vertices by summing longitude and latitude
    gg <- get.data.frame(g, "both")
    vert <- gg$vertices
    coordinates(vert) <- ~lon + lat
    
    # Creating spatial lines from the edges of the graph with the weight of 
    #the trade value as an attribute
    
    # Retrieving the edges of the graph as a data frame from the output of 
    #get.data.frame(g, "both").
    edges <- gg$edges
    
    # Retrieving the weights of the edges from the attribute named "weight" 
    #in the graph g.
    weights <- E(g)$weight
    
    # Creating a list of SpatialLines objects from the vertices data frame vert 
    #using the edges data frame. For each row i in the edges data frame, 
    #it extracts the "from" and "to" vertex names, retrieves the corresponding 
    #rows from vert, creates a SpatialLines object from these vertices, and 
    #stores it in the list.
    
    edges <- lapply(1:nrow(edges), function(i) {
      as(rbind(vert[vert$name == edges[i, "from"], ],
               vert[vert$name == edges[i, "to"], ]),
         "SpatialLines")
    })
    
    #This loop assigns unique IDs to the SpatialLines objects in the list edges 
    #using the spChFIDs function. The new IDs are character strings generated 
    #from the index i.
    
    for (i in seq_along(edges)) {
      edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
    }
    
    # Combining the SpatialLines objects in the list edges into a single 
    #object of class SpatialLines.
    edges <- do.call(rbind, edges)
    
    # Creating a data frame df.edges with two columns: "id" and "weight". 
    #The "id" column contains sequential integers, and the "weight" column 
    #contains the weights of the edges.
    df.edges <- data.frame(id = seq_along(edges), weight = weights)
    
    # Combining the SpatialLines object edges with the data frame df.edges to 
    #create a SpatialLinesDataFrame. The resulting object contains both the 
    #spatial information (the lines connecting the vertices) and the attribute 
    #information (the weights of the edges)
    edges <- SpatialLinesDataFrame(edges, data = df.edges)
    
    #defining color palette for weights on the map used below
    pal <- colorNumeric(palette = "YlOrRd", domain = c(min(df.edges$weight),
                                                       max(df.edges$weight)))
    
    #long and lat for setting zoom of the map
    dt.zoom <- dt.meta[dt.meta$name == input$country.map, ]
  
    
    leaflet(vert) %>%
      
      #Format of the map
      addProviderTiles("Stamen.Toner", 
                       options = providerTileOptions(noWrap = TRUE, 
                                                     zoomSnap = 0, 
                                                                     
                                                     attributionControl = FALSE, 
                                                                     
                                                     backgroundColor = "#f2f2f2",
                                                                     
                                                     opacity = 0.4)) %>%
      #Adding red circles on edges
      addCircleMarkers(data = vert, radius = 2,
                       color = "red",
                       fillColor = "red",
                       fillOpacity = 0.8,
                       stroke = FALSE) %>%
      
      #Adding lines connecting edges with a color scheme representing the 
      #different weights of the trades
      addPolylines(data = edges, weight = 2, color = ~pal(weight)) %>%
      
      #Adding legend for color of the lines representing the weights of exports
      #and import's trade value
      addLegend(position = "bottomright", pal = pal, values = df.edges$weight, 
                title = "Trade Value (USD)", 
                labFormat = labelFormat(suffix = " USD", digits = 0)) %>%
      
      setView(lng = dt.zoom$lon, lat = dt.zoom$lat, zoom = 2)
    
  })
  
  output$text.map <- renderUI({
    
    HTML(paste(" ", "<br>", "This tab allows us to explore the international 
    trade network of a selected country and time frame displayed on the map above. 
    The user can choose to visualise either imports or exports of the selected 
    country. For instance, by default, Ghana was selected as the country, and 
    the map exhibits its export trade through lines connecting to all its export
    destinations. These lines are color-coded to indicate the increase in trade 
    value. A minimum trade value can be selected making it easier to interpret 
    the high-value trades. For Ghana, we can deduce that its most significant 
               export partner is India.",
               " ", "<br>",
               " ", "<br>"))
    })
  
  #Centrality Table Output
  output$centralities.map <- renderDT({
    
    dt.trade.year <- dt.trade[dt.trade$year >=
                                input$year.map[1] & dt.trade$year <= 
                                input$year.map[2], ]
    dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
    dt.trade.edgelist
    
    
    # 2. Convert edge list to an igraph network
    # igraph wants our data in matrix format
    m.trade <- as.matrix(dt.trade.edgelist) 
    g.trade <- graph_from_edgelist(m.trade, directed=TRUE)
    
    
    # 3. Set the weight of the edges (trade_value_usd) & add the year as an 
    # attribute
    E(g.trade)$weight <- dt.trade.year$trade_value_usd
    # Sum the weights for duplicate edges (we would need this if we select 
    # multiple years)
    g <- simplify(g.trade, remove.multiple = TRUE, 
                  edge.attr.comb = list(weight = "sum", year = "concat"))
    
    
    # Extract the corresponding row from the data frame
    
    df.map <- data.frame(
      DegreeCentrality = degree(g, v = input$country.map),
      ClosenessCentrality = closeness(g, v = input$country.map),
      BetweennessCentrality = betweenness(g, v = input$country.map),
      EigenvectorCentrality = round(eigen_centrality(g)$vector[input$country.map],
                                    4),
      ClusteringCoefficient = round(transitivity(g, v = input$country.map), 4)
    )
    
    names(df.map) <- c("Degree Centrality", "Closeness Centrality", 
                       "Betweenness Centrality", "Eigenvector Centrality", 
                       "Clustering Coefficient")
    
    # Transpose the table
    df.map.t <- t(df.map)
    
    df.map.t
  }, options = list(searching = FALSE, lengthChange = FALSE, 
                    dom = 't', paging = FALSE))
  
  
  # Data table output
  output$table.overview.data <- renderDT({
    # Filter data
    dt.filtered.data <- dt.trade
    
    if (input$partner.data != "") {
      dt.filtered.data <-
        dt.filtered.data[dt.filtered.data$partner_name %in% 
                           input$partner.data, ]
    }
    if (input$reporter.data != "") {
      dt.filtered.data <-
        dt.filtered.data[dt.filtered.data$reporter_name %in% 
                           input$reporter.data, ]
    }
    if (input$year.data != "") {
      dt.filtered.data <- dt.filtered.data[dt.filtered.data$year == 
                                             input$year.data, ]
    }
    
    datatable(
      dt.filtered.data[, c(
        "partner_name",
        "reporter_name",
        "trade_value_usd",
        "partner_continent",
        "reporter_continent",
        "year"
      )],
      filter = "top",
      width = "100%",
      options = list(pageLength = 10, scrollX = TRUE),
      class = 'cell-border stripe',
    )
  })
  
  output$continent.count.des <- renderPlot ({
    g <- create.trade.graph(dt.trade, input$continent.des)
    
    # Count number of nodes per continent
    df <- as.data.frame(table(V(g)$continent))
    
    ggplot(df, aes(x = reorder(Var1, Freq), y = Freq, fill = "#58B99D")) +
      geom_col(color = "black", size = 0.5) +
      scale_y_continuous(limits = c(0, 100)) +
      labs(x = "Continent", y = "Frequency") +
      scale_fill_manual(values = c("#58B99D"))
  })
  
  # Histogram output
  output$degree.dist.des <- renderPlot({
    # Create trade graph
    g <- create.trade.graph(dt.trade, input$continent.des)
    
    # Plot degree distribution
    hist(
      degree(g),
      main = "Degree Distribution of selected Countries",
      xlab = "Degree",
      ylab = "Frequency",
      col = "#58B99D",
      breaks = seq(0, max(degree(g)) + 30, by = 30)
    )
  })
  
  output$text.degree.dist <- renderUI({
    
    HTML(paste(" ", "<br>", "This histogram displays the degree distribution of 
    trade for the selected countries. It illustrates how frequently countries 
    have different numbers of connections (degrees) in the trade graph.For 
    instance, if we select European countries, the histogram will be left-skewed, 
    indicating that most countries have a high degree. This is due to the fact 
               that these countries are more industrialized.",
               " ", "<br>",
               " ", "<br>"))
  })
  
  # KPI table output
  output$table.overview.des <- renderDT({
    # Create trade graph
    g <- create.trade.graph(dt.trade, input$continent.des)
    
    # Calculate KPIs and store in a data frame
    df.kpi <- data.frame(
      "Number_of_Vertices" = vcount(g),
      "Number_of_Edges" = ecount(g),
      "Mean_Degree_Vertices" = mean(degree(g)),
      "Median_Degree_Vertices" = median(degree(g)),
      "Average_Edge_Value_USD" = mean(E(g)$weight),
      "Median_Edge_Value_USD" = median(E(g)$weight),
      "Min_Edge_Value_USD" = min(E(g)$weight),
      "Max_Edge_Value_USD" = max(E(g)$weight),
      "Standard_Deviation_Edge_Value_USD" = sd(E(g)$weight),
      "Average_Path_Length" = mean_distance(g),
      "Average_Clustering_Coefficient" = transitivity(g, type = "global"),
      "Diameter" = diameter(g)
    )
    
    # Transpose the table
    df.kpi.t <- t(df.kpi)
    
    # Make column names more readable
    rownames(df.kpi.t) <- tools::toTitleCase(gsub("_", " ", rownames(df.kpi.t)))
    
    df.kpi.t
  }, options = list(searching = FALSE, lengthChange = FALSE, 
                    dom = 't', paging = FALSE))
  
  
  
  output$kpi.chart.des <- renderPlot({
    # Create trade graph
    g <- create.trade.graph(dt.trade, input$continent.des)
    
    # Prepare data for ggplot
    df.trade.data <- data.frame(trade_value = E(g)$weight)
    
    # Plot the histogram of trade amounts using a log2 scale
    ggplot(df.trade.data, aes(x = trade_value)) +
      geom_histogram(bins = 30, fill = "#58B99D", color = "black") +
      scale_x_continuous(trans = "log2", labels = scales::comma) +
      xlab("Trade Amount ($) (log2 scale)") +
      ylab("Frequency")
  })
  
  
  
  
  #Compare Countries------------------------------------------------------------
  
  # Create a reactive data frame to filter the data based on the user inputs
  df.filter.data <- reactive({
    l.country <- sort(input$country.comp)
    
    dt.merged[reporter_name == 
                l.country[1] & year >= input$year.comp[1] & 
                year <= input$year.comp[2]]
  })
  
  # Create reactive data frames for the second and third country selections
  df.filter.data.2 <- reactive({
    l.country <- sort(input$country.comp)
    if (!is.null(l.country[2])) {
      dt.merged[reporter_name == l.country[2] & year >= input$year.comp[1] & 
                  year <= input$year.comp[2]]
    } else {
      NULL
    }
  })
  
  df.filter.data.3 <- reactive({
    l.country <- sort(input$country.comp)
    if (!is.null(l.country[3])) {
      dt.merged[reporter_name == l.country[3] & 
                  year >= input$year.comp[1] & year <= input$year.comp[2]]
    } else {
      NULL
    }
  })
  
  # Create the plot based on the filtered data
  output$plot.comp <- renderPlot({
    l.country <- sort(input$country.comp)
    if (!is.null(df.filter.data.2()) & !is.null(df.filter.data.3())) {
      ggplot() +
        geom_line(data = df.filter.data(), 
                  aes_string(x = "year", 
                             y = input$column, 
                             color = "'Country 1'"), size = 2) +
        geom_line(data = df.filter.data.2(), 
                  aes_string(x = "year", 
                             y = input$column, 
                             color = "'Country 2'"), size = 2) +
        geom_line(data = df.filter.data.3(), 
                  aes_string(x = "year", y = input$column, 
                             color = "'Country 3'"), size = 2) +
        labs(x = "Year",
             y = label.map[input$column]) +
        scale_color_manual(name = "Country", 
                           values = c("Country 1" = "#F8766D", 
                                      "Country 2" = "#619CFF", 
                                      "Country 3" = "#00BA38"),
                           
                           labels = c(l.country[1], l.country[2], l.country[3]))
    } else if (!is.null(df.filter.data.2())) {
      ggplot() +
        geom_line(data = df.filter.data(), 
                  aes_string(x = "year", y = input$column, 
                             color = "'Country 1'")) +
        geom_line(data = df.filter.data.2(), 
                  aes_string(x = "year", y = input$column, 
                             color = "'Country 2'")) +
        labs(x = "Year",
             y = label.map[input$column]) +
        scale_color_manual(name = "Country", 
                           values = c("Country 1" = "#F8766D", 
                                      "Country 2" = "#00BA38"),
                           labels = c(l.country[1], l.country[2]))
    } else {
      ggplot(df.filter.data(), aes_string(x = "year", 
                                          y = input$column, 
                                          color = "'Country 1'")) +
        geom_line() +
        labs(x = "Year",
             y = label.map[input$column]) +
        scale_color_manual(name = "Country", 
                           values = c("Country 1" = "#F8766D"),
                           labels = c(l.country[1]))
    }
  })
  
  # Create the map based on the filtered data
  output$map.comp <- renderLeaflet({
    # Create a data frame with only the selected countries' coordinates
    df.selected.countries <- unique(dt.merged[reporter_name %in% 
                                                input$country.comp,
                                           c("reporter_name", "reporter_lat", 
                                             "reporter_long")])
    # Create a leaflet map centered on the selected countries or the world
    if (nrow(df.selected.countries) > 0) {
      leaflet() %>%
        addProviderTiles("Stamen.Toner", 
                         options = providerTileOptions(noWrap = TRUE, 
                                                       zoomSnap = 0, 
                                                       attributionControl = FALSE, 
                                                       backgroundColor = "#f2f2f2",
                                                       opacity = 0.4)) %>%
        
        addCircleMarkers(data = df.selected.countries, lng = ~reporter_long, 
                         lat = ~reporter_lat, label = ~reporter_name, 
                         radius = 10,
                         color = colorFactor(palette = c("#F8766D",
                                                         "#619CFF",
                                                         "#00BA38"), 
                                  domain = 1:3)(1:nrow(df.selected.countries)),
                         fillColor = colorFactor(palette = c("#F8766D", 
                                                             "#619CFF", 
                                                             "#00BA38"), 
                                  domain = 1:3)(1:nrow(df.selected.countries)),
                         fillOpacity = 0.8,
                         stroke = FALSE)
    } else {
      leaflet() %>%
        addProviderTiles("Stamen.Toner", 
                         options = providerTileOptions(noWrap = TRUE,
                                                      zoomSnap = 0, 
                                                    attributionControl = FALSE, 
                              backgroundColor = "#f2f2f2", opacity = 0.4)) %>%
        
      setView(lng = 0, lat = 0, zoom = 2) %>%
      leafletOptions(margin = c(100, 50, 50, 50))
    }
  })
  
  output$example.comp <- renderUI({
    HTML(paste(
      " ", "<br>",
      "As an example we compare countries Germany, Italy and Portugal over the 
      time period 2000-2021. We observe that Germany has significantly 
      increased its exports in the period under review, while Italy and Portugal
      remained almost at the same level. The number of export partners of Germany
      and Italy is very similar, while Portugal has significantly less export
      partners. These first KPIs can be brought together by analysing the average
      export value per partner. Germany performs significantly better then Italy 
      and Portugal and has, on average, the highest trade value in USD per export 
      partner. Similar analysis can be made using the import KPIs.",
      " ", "<br>",
      " ", "<br>",
      "The trade balance is defined as the difference between the value of a 
      country's exports and the value of a country's imports for a given period.
      We observe that portugal has a negative trade balance for the whole 21st 
      century, meaning that the country imports more than it exports which can 
      be concerning if it continues. Italy has a very steady and slightly positive 
      trade balance, meaning that the country exports more than it imports. 
      Germany, on the other hand, reports a highly positive trade balance over 
      the whole time period. A positive trade balance is seen as favourable and 
      an indication of economic strenght.",
      " ", "<br>",
      " ", "<br>"
    ))
  })
  
  output$map.comm <- renderLeaflet({
    # 1. Make edge list
    dt.trade.year <- dt.trade[dt.trade$year == input$year.comm, ]
    dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
    
    # 2. Convert edge list to an igraph network
    # igraph wants our data in matrix format
    m.trade <- as.matrix(dt.trade.edgelist) 
    g.trade <- graph_from_edgelist(m.trade, directed=TRUE)
    
    # 3. Set vertex attributes using set_vertex_attr()
    l.reporters <- as.list(unique(dt.trade.year$reporter_name))
    l.partners <- as.list(unique(dt.trade.year$partner_name))
    l.countries <- as.list(unique(append(l.reporters, l.partners)))
  
    dt.country.coordinates <- dt.trade.year %>% distinct(partner_name, .keep_all
                                                         = TRUE)
   
    dt.country.coordinates <- dt.country.coordinates[, c("partner_name", 
                                                         "partner_lat", 
                                                         "partner_long")]
    
    dt.meta <- dt.country.coordinates %>% rename("name" = "partner_name", 
                                                    "lat" = "partner_lat", 
                                                    "lon" = "partner_long")
    
    dt.meta <- dt.meta[match(V(g.trade)$name, dt.meta$name), ]
    V(g.trade)$lat <- dt.meta$lat
    V(g.trade)$lon <- dt.meta$lon
    
    # 4. Set the weight of the edges & add the year as an attribute
    E(g.trade)$weight <- dt.trade.year$trade_value_usd
    # Remove vertices with degree 0
    g.trade <- delete.vertices(g.trade, which(degree(g.trade) == 0))
    
    # 5. Community detection: Walktrap algorithm
    walktrap_communities <- walktrap.community(g.trade, 
                                               weights = E(g.trade)$weight)
    # Get the community membership vector
    membership_vec <- membership(walktrap_communities)
    # Add the community as an attribute to the vectors
    V(g.trade)$community <- membership_vec[V(g.trade)$name]
    
    # extract the vertices and edges from the g graph object and store 
    #them as a data frame
    gg <- get.data.frame(g.trade, "both")
    # assign vert variable with dataframe info on the vertices + add the spatial
    #coordinates of the points to vert
    vert <- gg$vertices
    coordinates(vert) <- ~lon + lat
    
    # assign edges and weights variable
    edges <- gg$edges
    weights <- E(g.trade)$weight
    
    # import the edges SpatialPointsDataFrame that we preprocessed
    path_file <- paste0("src/edges_", input$year.comm, ".shp")
    # The dsn argument refers to the directory containing the shapefile,
    # and layer refers to the name of the shapefile without the .shp extension
    dsn <- dirname(path_file)
    layer <- basename(tools::file_path_sans_ext(path_file))
    edges <- readOGR(dsn = dsn, layer = layer)
    
    # plot map
    community_colors <- hue_pal()(length(unique(vert$community)))
    
    leaflet(vert) %>% 
      addProviderTiles("Stamen.Toner", options = 
                         providerTileOptions(backgroundColor = "#f2f2f2",
                                             opacity = 0.4)) %>% 
      addCircleMarkers(data = vert, radius = 4, 
                       color = community_colors[vert$community], 
                       fillColor = community_colors[vert$community], 
                       fillOpacity = 0.8,
                       stroke = FALSE)
  })
  
  output$network.comm <- renderPlot({
    # 1. Make edge list
    dt.trade.year <- dt.trade[dt.trade$year == input$year.comm, ]
    dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
    
    # 2. Convert edge list to an igraph network
    # igraph wants our data in matrix format
    m.trade <- as.matrix(dt.trade.edgelist) 
    g.trade <- graph_from_edgelist(m.trade, directed=TRUE)
    
    # 3. Set vertex attributes using set_vertex_attr()
    l.countries <- as.list(unique(dt.trade.year$reporter_name))
    dt.country.coordinates <- dt.trade.year %>% distinct(partner_name, 
                                                         .keep_all = TRUE)
    dt.country.coordinates <- dt.country.coordinates[, c("partner_name", 
                                                         "partner_lat", 
                                                         "partner_long")]
    
    dt.meta <- dt.country.coordinates %>% rename("name" = "partner_name", 
                                                 "lat" = "partner_lat", 
                                                 "lon" = "partner_long")
    
    vertex_attrs <- as.list(dt.meta[, -1]) # exclude the 'name' column
    V(g.trade)$lat <- vertex_attrs$lat
    V(g.trade)$lon <- vertex_attrs$lon
    
    # 4. Set the weight of the edges & add the year as an attribute
    E(g.trade)$weight <- dt.trade.year$trade_value_usd
    # Remove vertices with degree 0
    g.trade <- delete.vertices(g.trade, which(degree(g.trade) == 0))
    
    # 5. Community detection algorithms
    # Run Walktrap algorithm
    walktrap.communities <- walktrap.community(g.trade, 
                                               weights = E(g.trade)$weight)
    
    plot(walktrap.communities, g.trade, vertex.size = 5, 
         vertex.label.cex = 0.3, edge.arrow.size=0.3, 
         vertex.color = walktrap.communities$membership, edge.color = 'grey')
  })
  
  output$textKPI.comm <- renderUI({
    # 1. Make edge list
    dt.trade.year <- dt.trade[dt.trade$year == input$year.comm, ]
    dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
    
    # 2. Convert edge list to an igraph network
    # igraph wants our data in matrix format
    m.trade <- as.matrix(dt.trade.edgelist) 
    g.trade <- graph_from_edgelist(m.trade, directed=TRUE)
    
    # 3. Set vertex attributes using set_vertex_attr()
    l.countries <- as.list(unique(dt.trade.year$reporter_name))
    dt.country.coordinates <- dt.trade.year %>% distinct(partner_name,
                                                         .keep_all = TRUE)
    
    dt.country.coordinates <- dt.country.coordinates[, c("partner_name", 
                                                         "partner_lat", 
                                                         "partner_long")]
    
    dt.meta <- dt.country.coordinates %>% rename("name" = "partner_name", 
                                                 "lat" = "partner_lat", 
                                                 "lon" = "partner_long")
    
    l.vertex.attrs <- as.list(dt.meta[, -1]) # exclude the 'name' column
    V(g.trade)$lat <- l.vertex.attrs$lat
    V(g.trade)$lon <- l.vertex.attrs$lon
    
    # 4. Set the weight of the edges  & add the year as an attribute
    E(g.trade)$weight <- dt.trade.year$trade_value_usd
    # Remove vertices with degree 0
    g.trade <- delete.vertices(g.trade, which(degree(g.trade) == 0))
    
    # 5. Community detection: Walktrap algorithm
    walktrap.communities <- walktrap.community(g.trade, 
                                               weights = E(g.trade)$weight)
    
    # get the average path length for each community
    f.apl <- function(m) round(average.path.length(induced_subgraph(g.trade, 
                          V(g.trade)[walktrap.communities$membership == m])), 2)
    
    apl.comm <- lapply(unique(walktrap.communities$membership), f.apl)
    apl.all <- round(average.path.length(g.trade), 2)
    
    HTML(paste(" ", "<br>",
               "Number of communities detected: ", "<br>", 
               length(unique(walktrap.communities$membership)), "<br>",
               " ", "<br>",
               "Modularity score: ", "<br>", 
               round(modularity(walktrap.communities, g.trade), 2), "<br>",
               " ", "<br>",
               "Sizes of the communities: ", "<br>", 
               paste(unname(sizes(walktrap.communities)), 
                     collapse = ", "), "<br>",
               " ", "<br>",
               "Average path length of the total trade network: ", 
               "<br>", apl.all, "<br>",
               " ", "<br>",
               "Average path length per community: ", "<br>", 
               paste(unname(apl.comm), collapse = ", "), "<br>"))
  })
  
  output$textMap.comm <- renderUI({
    HTML(paste(" ", "<br>",
               "In 2021, the walktrap algorithm detected 3 communities in our 
               international trade network, consisting of 153, 73, and 1 
               vertices. We observe that the communities are roughly divided by 
               the Tropic of Cancer. The green community situates itself around 
               the Mediterranean Sea, and the blue community consists only of 
               South Georgia and the South Sandwich Islands.",
               " ", "<br>",
               " ", "<br>",
               "The modularity score compares the number of edges within the 
               detected communities to the number of edges that would be 
               expected by chance. The modularity score of 0.26 thus signifies 
               that our detected communities are of higher quality than randomly
               selected communities.",
               " ", "<br>",
               " ", "<br>"))
  })
  
  output$textWalktrap.comm <- renderUI({
    HTML(paste(" ", "<br>",
               "We utilized the Walktrap Algorithm, developed by Pascal Pons, to 
               detect communities in our trade network. The Walktrap algorithm is 
               a hierarchical clustering algorithm that identifies communities via 
               random walks and is based on the idea that short random walks tend 
               to stay in the same community. Unlike the Louvain clustering 
               algorithm, the Walktrap algorithm takes weights into consideration 
               during the process of identifying communities. Furthermore, the 
               algorithm interprets the weights of edges as connection strengths 
               between vertices, as opposed to the distance between two vertices, 
               as is the case with the Edge-betweenness clustering algorithm. 
               The Walktrap algorithm is generally considered a non-overlapping 
               community detection algorithm, meaning that each node in the graph 
               is assigned to a single community and there is no overlap between 
               the communities. The algorithm is primarily employed in the case 
               of undirected graphs; however, since two countries are connected 
               when they trade with each other, regardless of the direction, we 
               determined the Walktrap algorithm to be the optimal selection for 
               our community analysis.",
               " ", "<br>",
               " ", "<br>"))
  })
  
  output$countries.comm <- renderDataTable({
    # 1. Make edge list
    dt.trade.year <- dt.trade[dt.trade$year == input$year.comm, ]
    dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
    
    # 2. Convert edge list to an igraph network
    # igraph wants our data in matrix format
    m.trade <- as.matrix(dt.trade.edgelist) 
    g.trade <- graph_from_edgelist(m.trade, directed=TRUE)
    
    # 3. Set vertex attributes using set_vertex_attr()
    l.reporters <- as.list(unique(dt.trade.year$reporter_name))
    l.partners <- as.list(unique(dt.trade.year$partner_name))
    l.countries <- as.list(unique(append(l.reporters, l.partners)))
    
    dt.country.coordinates <- dt.trade.year %>% distinct(partner_name, 
                                                         .keep_all = TRUE)
    
    dt.country.coordinates <- dt.country.coordinates[, c("partner_name", 
                                                         "partner_lat", 
                                                         "partner_long")]
    
    dt.meta <- dt.country.coordinates %>% rename("name" = "partner_name", 
                                                 "lat" = "partner_lat", 
                                                 "lon" = "partner_long")
    
    dt.meta <- dt.meta[match(V(g.trade)$name, dt.meta$name), ]
    V(g.trade)$lat <- dt.meta$lat
    V(g.trade)$lon <- dt.meta$lon
    
    # 4. Set the weight of the edges & add the year as an attribute
    E(g.trade)$weight <- dt.trade.year$trade_value_usd
    # Remove vertices with degree 0
    g.trade <- delete.vertices(g.trade, which(degree(g.trade) == 0))
    
    # 5. Community detection algorithms
    # Run Walktrap algorithm
    walktrap.communities <- walktrap.community(g.trade, 
                                               weights = E(g.trade)$weight)
    # Get the community membership vector
    membership.vec <- membership(walktrap.communities)
    
    # Add the community as an attribute to the vectors
    V(g.trade)$community <- membership.vec[V(g.trade)$name]
    
    # convert vertex data to a data frame
    df.vertex <- get.data.frame(g.trade, what= c("vertices"))
    community.table <- df.vertex %>%
      group_by(community) %>%
      summarise(vertex_names = list(name)) %>%
      ungroup() %>%
      mutate(vertex_names = sapply(vertex_names, paste, collapse = ", "))
    community.table
  })
  
  output$modularity.comm <- renderPlot({
    # Create a dataframe to store the modularity scores for each year
    df.modularity <- data.frame(year = numeric(), modularity_score = numeric())
    
    # Loop over each year and compute the modularity score
    for (element in 2000:2021) {
      # 1. Make edge list
      dt.trade.year <- dt.trade[dt.trade$year == element, ]
      dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
      # 2. Convert edge list to an igraph network
      m.trade <- as.matrix(dt.trade.edgelist) 
      g.trade <- graph_from_edgelist(m.trade, directed=TRUE)
      # 3. Set the weight of the edges & add the year as an attribute
      E(g.trade)$weight <- dt.trade.year$trade_value_usd
      # 4. Remove vertices with degree 0
      g.trade <- delete.vertices(g.trade, which(degree(g.trade) == 0))
      # 5. Community detection: Walktrap algorithm
      walktrap.communities <- walktrap.community(g.trade, 
                                                 weights = E(g.trade)$weight)
      modularity <- modularity(walktrap.communities, g.trade)
      
      # 6. Store the modularity score in the dataframe
      df.modularity <- rbind(df.modularity, data.frame(year = element, 
                                                modularity_score = modularity))
    }
    
    # Plot the modularity scores over time
    ggplot(df.modularity, aes(x = year, y = modularity_score)) +
      geom_line(color = "#58B99D", size = 2) +
      scale_x_continuous(breaks = seq(2000, 2021, by = 2)) +
      xlab("Year") +
      ylab("Modularity Score")
  })
  
  output$textModularity.comm <- renderUI({
    HTML(paste(" ", "<br>",
               "<ul> <li> An increase in modularity score over time may suggest 
               that the communities in the network are becoming more distinct 
               and separated from each other, potentially due to changes in trade 
               policies, economic conditions, or cultural factors. This could 
               indicate that there are well-defined clusters of countries that 
               predominantly trade with each other. </li>",
               "<li> A decrease in modularity score over time may indicate that 
               the communities in the network are becoming less distinct and more 
               interconnected, potentially due to factors such as increasing 
               globalization, economic integration, or changes in trade patterns. 
               This could suggest that countries are trading with a wider range 
               of partners and that there are fewer clear clusters of trade 
               relationships within the network. </li> </ul>",
               " ", "<br>",
               "It is important to note that changes in modularity score cannot 
               be solely attributed to any single factor, and the specific reasons 
               for these changes may be influenced by a complex interplay of 
               various factors.",
               " ", "<br>",
               " ", "<br>"))
  })
  
  output$source.info.data <- renderUI({
    HTML(paste(" ", "<br>",
               "The data used in this ShinyApp is sourced from the <a href=", 
               "https://www.kaggle.com/datasets/appetukhov/international-trade-database", 
               ">International Trade Database on Kaggle</a>. The Kaggle dataset 
               contains comprehensive information on international trade 
               relationships from 1988 to 2021, including trade values, reporter 
               and partner country names, and geographical information. This rich 
               dataset serves as the foundation for the app's visualizations and 
               analyses. The information in the dataset is collected from the <a
               href=", "https://wits.worldbank.org/Default.aspx?lang=en", ">
               World Integrated Trade Solution (WITS) </a> platform, a software 
               developed by the World Bank, in close collaboration with United 
               Nations Conference on Trade and Development (UNCTAD), 
               International Trade Center (ITC), United Nations Statistical 
               Division (UNSD), and World Trade Organization (WTO).",
               " ", "<br>",
               " ", "<br>"))
  })
  
  output$source.column.data <- renderDT({
    # Create data table with column names and descriptions
    attr(dt.trade$reporter_iso_3, "description") <- "ISO3 code of the reporting 
    country"
    attr(dt.trade$reporter_name, "description") <- "Name of the reporting 
    country"
    attr(dt.trade$partner_iso_3, "description") <- "ISO3 code of the partner 
    (destination) country"
    attr(dt.trade$partner_name, "description") <- "Name of the partner 
    (destination) country"
    attr(dt.trade$year, "description") <- "Reporting year"
    attr(dt.trade$trade_flow_name, "description") <- "Trade flow direction"
    attr(dt.trade$trade_value_1000_usd, "description") <- "Value of traded goods 
    in 1000 USD"
    attr(dt.trade$trade_value_usd, "description") <- "Value of traded goods in 
    USD"
    attr(dt.trade$reporter_continent, "description") <- "Continent of the 
                                                          reporting country"
    attr(dt.trade$partner_continent, "description") <- "Continent of the partner 
                                                          (destination) country"
    attr(dt.trade$reporter_lat, "description") <- "Latitude of the reporting 
                                                                        country"
    attr(dt.trade$partner_lat, "description") <- "Latitude of the partner 
                                                          (destination) country"
    attr(dt.trade$reporter_long, "description") <- "Longitude of the reporting 
                                                                        country"
    attr(dt.trade$partner_long, "description") <- "Longitude of the partner 
                                                          (destination) country"
    
    # Extract column names and descriptions
    l.col.names <- names(dt.trade)
    l.col.desc <- sapply(l.col.names, function(x) attr(dt.trade[[x]], 
                                                       "description"))
    
    # Create data frame with column names and descriptions
    df <- data.frame(column = l.col.names, description = l.col.desc, 
                     row.names = NULL)
  })
  
}