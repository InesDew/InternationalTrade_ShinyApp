library(igraph)
library(rgdal)

# Define server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  # Map output
  output$network.map <- renderLeaflet({
    
    # Set the parameters of the inputs to select years, minimum trade, reporter and partner name
    # This is done with an if statement depending if user selected Exports or Imports 
    if(input$trader.map=="reporter_name"){
      dt.trade.country <- dt.trade[ dt.trade$reporter_name == input$country.map, ]
      dt.trade.country.year <- dt.trade.country[dt.trade.country$year >= input$year.map[1] &
                                                  dt.trade.country$year <= input$year.map[2], ]
      dt.trade.country.year <- dt.trade.country.year[dt.trade.country.year$trade_value_usd/
                                                       max(dt.trade.country.year$trade_value_usd) * 
                                                       100 >= input$weight.map, ]
      
      
    } else {
      dt.trade.country <- dt.trade[ dt.trade$partner_name == input$country.map, ]
      dt.trade.country.year <- dt.trade.country[dt.trade.country$year >= input$year.map[1] &
                                                  dt.trade.country$year <= input$year.map[2], ]
      dt.trade.country.year <- dt.trade.country.year[dt.trade.country.year$trade_value_usd/
                                                       max(dt.trade.country.year$trade_value_usd) * 
                                                       100 >= input$weight.map, ]
      
    }
    
    #creating dataframe from the data table and defining from and to parameters
    df <- data.frame("from" = dt.trade.country.year$reporter_name,
                     "to"= dt.trade.country.year$partner_name)
    
    #checking if there is data from the inputs demanded my the user, if not, map shows a message saying that no data is available
    
    if (nrow(df) == 0) {
      popup <- paste0("No data available for selected country")
      return(leaflet() %>% addTiles() %>% setView(lng = 0, lat = 0, zoom = 2) %>%
               addPopups(lng = 0, lat = 0, popup = popup))
    }
    
    # Creating graph with data frame where the vertices are the meta data table containing all the countries with their longitude and latitude as previously explained 
    g <- graph_from_data_frame(df, directed = TRUE, vertices = meta)
    
    # Setting weights as the trade value 
    g <- set_edge_attr(g, "weight", value = dt.trade.country.year$trade_value_usd)
    
    # Sum the weights for duplicate edges (we would need this if we select multiple years)
    g <- simplify(g, remove.multiple = TRUE, edge.attr.comb=list(weight="sum", year="concat"))
    
    # Remove vertices with degree 0
    g <- delete.vertices(g, which(degree(g) == 0))
    
    # Retrieving data frame form the graph to get the coordinates of the vertices by summing longitude and latitude
    gg <- get.data.frame(g, "both")
    vert <- gg$vertices
    coordinates(vert) <- ~lon + lat
    
    # Creating spatial lines from the edges of the graph with the weight of the trade value as an attribute
    
    # Retrieving the edges of the graph as a data frame from the output of get.data.frame(g, "both").
    edges <- gg$edges
    
    # Retrieving the weights of the edges from the attribute named "weight" in the graph g.
    weights <- E(g)$weight
    
    # Creating a list of SpatialLines objects from the vertices data frame vert using the edges data frame. For each row i in the edges data frame, it extracts the "from" and "to" vertex names, retrieves the corresponding rows from vert, creates a SpatialLines object from these vertices, and stores it in the list.
    
    edges <- lapply(1:nrow(edges), function(i) {
      as(rbind(vert[vert$name == edges[i, "from"], ],
               vert[vert$name == edges[i, "to"], ]),
         "SpatialLines")
    })
    
    #This loop assigns unique IDs to the SpatialLines objects in the list edges using the spChFIDs function. The new IDs are character strings generated from the index i.
    
    for (i in seq_along(edges)) {
      edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
    }
    
    # Combining the SpatialLines objects in the list edges into a single object of class SpatialLines.
    edges <- do.call(rbind, edges)
    
    # Creating a data frame edges_df with two columns: "id" and "weight". The "id" column contains sequential integers, and the "weight" column contains the weights of the edges.
    edges_df <- data.frame(id = seq_along(edges), weight = weights)
    
    # Combining the SpatialLines object edges with the data frame edges_df to create a SpatialLinesDataFrame. The resulting object contains both the spatial information (the lines connecting the vertices) and the attribute information (the weights of the edges)
    edges <- SpatialLinesDataFrame(edges, data = edges_df)
    
    #defining color palette for weights on the map used below
    pal <- colorNumeric(palette = "YlOrRd", domain = c(min(edges_df$weight), max(edges_df$weight)))
    
    #long and lat for setting zoom of the map
    dt.zoom <- meta[meta$name==input$country.map,]
  
  
    
    leaflet(vert) %>%
      
      #Format of the map
      addProviderTiles("Stamen.Toner", options = providerTileOptions(noWrap = TRUE, zoomSnap = 0, 
                                                                     attributionControl = FALSE, 
                                                                     backgroundColor = "#f2f2f2",opacity = 
                                                                       0.4)) %>%
      #Adding red circles on edges
      addCircleMarkers(data = vert, radius = 2,
                       color = "red",
                       fillColor = "red",
                       fillOpacity = 0.8,
                       stroke = FALSE) %>%
      
      #Adding lines connecting edges with a color scheme representing the different weights of the trades
      addPolylines(data = edges, weight = 2, color = ~pal(weight)) %>%
      
      #Adding legend for color of the lines representing the weights of exports and import's trade value
      addLegend(position = "bottomright", pal = pal, values = edges_df$weight, 
                title = "Trade Value (USD)", labFormat = labelFormat(suffix = " USD", digits = 0)) %>%
      
      setView(lng=dt.zoom$lon,lat=dt.zoom$lat,zoom=2)
    
  })
  
  output$text.map <- renderUI({
    
    HTML(paste(" ", "<br>","This tab allows us to explore the international trade network of a selected
               country and time frame displayed on the map above. The user can choose to visualise either
               imports or exports of the selected country. For instance, by default, Ghana was selected as
               the country, and the map exhibits its export trade through lines connecting to all its export
               destinations. These lines are color-coded to indicate the increase in trade value. A minimum 
               trade value can be selected making it easier to interpret the high-value trades. For Ghana, we 
               can deduce that its most significant export partner is India.",
               " ", "<br>",
               " ", "<br>"))
    })
  
  #Centrality Table Output
  output$centralities.map <- renderDT({
    
    dt.trade.year <- dt.trade[dt.trade$year >= input$year.map[1] & dt.trade$year <= input$year.map[2], ]
    dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
    dt.trade.edgelist
    
    
    # 2. Convert edge list to an igraph network
    # igraph wants our data in matrix format
    m.trade <- as.matrix(dt.trade.edgelist) 
    g.trade <- graph_from_edgelist(m.trade, directed=TRUE)
    
    
    # 3. Set the weight of the edges (trade_value_usd) & add the year as an attribute
    E(g.trade)$weight <- dt.trade.year$trade_value_usd
    # Sum the weights for duplicate edges (we would need this if we select multiple years)
    g <- simplify(g.trade, remove.multiple = TRUE, edge.attr.comb=list(weight="sum", year="concat"))
    
    
    # Extract the corresponding row from the data frame
    
    df.map <- data.frame(
      DegreeCentrality = degree(g, v= input$country.map),
      ClosenessCentrality = closeness(g, v= input$country.map),
      BetweennessCentrality = betweenness(g, v= input$country.map),
      EigenvectorCentrality = round(eigen_centrality(g)$vector[input$country.map],4),
      ClusteringCoefficient = round(transitivity(g, v= input$country.map),4)
    )
    
    names(df.map) <- c("Degree Centrality", "Closeness Centrality","Betweenness Centrality", "Eigenvector Centrality", "Clustering Coefficient")
    
    # Transpose the table
    df.map.t <- t(df.map)
    
    df.map.t
  }, options = list(searching = FALSE, lengthChange = FALSE, dom = 't', paging = FALSE))
  
  
  # Data table output
  output$data_table <- renderDT({
    # Filter data
    filtered_data <- dt.trade
    
    if (input$filter_partner != "") {
      filtered_data <-
        filtered_data[filtered_data$partner_name %in% input$filter_partner, ]
    }
    if (input$filter_reporter != "") {
      filtered_data <-
        filtered_data[filtered_data$reporter_name %in% input$filter_reporter, ]
    }
    if (input$filter_year != "") {
      filtered_data <- filtered_data[filtered_data$year == input$filter_year, ]
    }
    
    datatable(
      filtered_data[, c(
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
  
  output$column_names <- renderText({
    paste(colnames(dt.trade), collapse = ", ")
  })
  
  output$continent_count <- renderPlot ({
    g <- create_trade_graph(dt.trade, input$desc_yearInput, input$des_continentInput)
    
    # Count number of nodes per continent
    df <- as.data.frame(table(V(g)$continent))
    
    # Plot number of nodes per continent
    ggplot(df, aes(x = Var1, y = Freq)) +
      geom_col() +
      labs(x = "Continent", y = "Number of nodes")
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
  
  output$CommMap <- renderLeaflet({
    # 1. Make edge list
    dt.trade.year <- dt.trade[dt.trade$year == input$CommYear, ]
    dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
    
    # 2. Convert edge list to an igraph network
    # igraph wants our data in matrix format
    m.trade <- as.matrix(dt.trade.edgelist) 
    g.trade <- graph_from_edgelist(m.trade, directed=TRUE)
    
    # 3. Set vertex attributes using set_vertex_attr()
    l.reporters <- as.list(unique(dt.trade.year$reporter_name))
    l.partners <- as.list(unique(dt.trade.year$partner_name))
    l.countries <- as.list(unique(append(l.reporters, l.partners)))
    dt.country.coordinates <- dt.trade.year %>% distinct(partner_name, .keep_all = TRUE)
    dt.country.coordinates <- dt.country.coordinates[, c("partner_name", "partner_lat", "partner_long")]
    meta <- dt.country.coordinates %>% rename("name" = "partner_name", "lat" = "partner_lat", "lon" = "partner_long")
    
    meta <- meta[match(V(g.trade)$name, meta$name), ]
    V(g.trade)$lat <- meta$lat
    V(g.trade)$lon <- meta$lon
    
    # 4. Set the weight of the edges (trade_value_usd) & add the year as an attribute
    E(g.trade)$weight <- dt.trade.year$trade_value_usd
    # Remove vertices with degree 0
    g.trade <- delete.vertices(g.trade, which(degree(g.trade) == 0))
    
    # 5. Community detection: Walktrap algorithm
    walktrap_communities <- walktrap.community(g.trade, weights = E(g.trade)$weight)
    # Get the community membership vector
    membership_vec <- membership(walktrap_communities)
    # Add the community as an attribute to the vectors
    V(g.trade)$community <- membership_vec[V(g.trade)$name]
    
    # extract the vertices and edges from the g graph object and store them as a data frame
    gg <- get.data.frame(g.trade, "both")
    # assign vert variable with dataframe info on the vertices + add the spatial coordinates of the points to vert
    vert <- gg$vertices
    coordinates(vert) <- ~lon + lat
    
    # assign edges and weights variable
    edges <- gg$edges
    weights <- E(g.trade)$weight
    
    # import the edges SpatialPointsDataFrame that we preprocessed
    path_file <- paste0("src/edges_", input$CommYear, ".shp")
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
  
  output$CommNetwork <- renderPlot({
    # 1. Make edge list
    dt.trade.year <- dt.trade[dt.trade$year == input$CommYear, ]
    dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
    
    # 2. Convert edge list to an igraph network
    # igraph wants our data in matrix format
    m.trade <- as.matrix(dt.trade.edgelist) 
    g.trade <- graph_from_edgelist(m.trade, directed=TRUE)
    
    # 3. Set vertex attributes using set_vertex_attr()
    l.countries <- as.list(unique(dt.trade.year$reporter_name))
    dt.country.coordinates <- dt.trade.year %>% distinct(partner_name, .keep_all = TRUE)
    dt.country.coordinates <- dt.country.coordinates[, c("partner_name", "partner_lat", "partner_long")]
    meta <- dt.country.coordinates %>% rename("name" = "partner_name", "lat" = "partner_lat", "lon" = "partner_long")
    
    vertex_attrs <- as.list(meta[, -1]) # exclude the 'name' column
    V(g.trade)$lat <- vertex_attrs$lat
    V(g.trade)$lon <- vertex_attrs$lon
    
    # 4. Set the weight of the edges (trade_value_usd) & add the year as an attribute
    E(g.trade)$weight <- dt.trade.year$trade_value_usd
    # Remove vertices with degree 0
    g.trade <- delete.vertices(g.trade, which(degree(g.trade) == 0))
    
    # 5. Community detection algorithms
    # Run Walktrap algorithm
    walktrap_communities <- walktrap.community(g.trade, weights = E(g.trade)$weight)
    
    plot(walktrap_communities, g.trade, vertex.size = 5, vertex.label.cex = 0.3, edge.arrow.size=0.3, vertex.color = walktrap_communities$membership, edge.color = 'grey')
  })
  
  output$CommTextKPI <- renderUI({
    # 1. Make edge list
    dt.trade.year <- dt.trade[dt.trade$year == input$CommYear, ]
    dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
    
    # 2. Convert edge list to an igraph network
    # igraph wants our data in matrix format
    m.trade <- as.matrix(dt.trade.edgelist) 
    g.trade <- graph_from_edgelist(m.trade, directed=TRUE)
    
    # 3. Set vertex attributes using set_vertex_attr()
    l.countries <- as.list(unique(dt.trade.year$reporter_name))
    dt.country.coordinates <- dt.trade.year %>% distinct(partner_name, .keep_all = TRUE)
    dt.country.coordinates <- dt.country.coordinates[, c("partner_name", "partner_lat", "partner_long")]
    meta <- dt.country.coordinates %>% rename("name" = "partner_name", "lat" = "partner_lat", "lon" = "partner_long")
    
    vertex_attrs <- as.list(meta[, -1]) # exclude the 'name' column
    V(g.trade)$lat <- vertex_attrs$lat
    V(g.trade)$lon <- vertex_attrs$lon
    
    # 4. Set the weight of the edges (trade_value_usd) & add the year as an attribute
    E(g.trade)$weight <- dt.trade.year$trade_value_usd
    # Remove vertices with degree 0
    g.trade <- delete.vertices(g.trade, which(degree(g.trade) == 0))
    
    # 5. Community detection: Walktrap algorithm
    walktrap_communities <- walktrap.community(g.trade, weights = E(g.trade)$weight)
    
    # get the average path length for each community
    f_apl <- function(m) round(average.path.length(induced_subgraph(g.trade, V(g.trade)[walktrap_communities$membership == m])), 2)
    apl_comm <- lapply(unique(walktrap_communities$membership), f_apl)
    apl_all <- round(average.path.length(g.trade), 2)
    
    HTML(paste(" ", "<br>",
               "Number of communities detected: ", "<br>", length(unique(walktrap_communities$membership)), "<br>",
               " ", "<br>",
               "Modularity score: ", "<br>", round(modularity(walktrap_communities, g.trade), 2), "<br>",
               " ", "<br>",
               "Sizes of the communities: ", "<br>", paste(unname(sizes(walktrap_communities)), collapse = ", "), "<br>",
               " ", "<br>",
               "Average path length of the total trade network: ", "<br>", apl_all, "<br>",
               " ", "<br>",
               "Average path length per community: ", "<br>", paste(unname(apl_comm), collapse = ", "), "<br>"))
  })
  
  output$CommTextMap <- renderUI({
    HTML(paste(" ", "<br>",
               "In 2021, the walktrap algorithm detected 3 communities in our 
               international trade network, consisting of 153, 73, and 1 vertices. 
               We observe that the communities are roughly divided by the Tropic 
               of Cancer. The green community situates itself around the 
               Mediterranean Sea, and the blue community consists only of South 
               Georgia and the South Sandwich Islands.",
               " ", "<br>",
               " ", "<br>",
               "The modularity score compares the number of edges within the 
               detected communities to the number of edges that would be expected 
               by chance. The modularity score of 0.26, indicates that they are 
               of higher quality than randomly selected communities.",
               " ", "<br>",
               " ", "<br>"))
  })
  
  output$CommTextWalktrap <- renderUI({
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
  
  output$CommCountries <- renderDataTable({
    # 1. Make edge list
    dt.trade.year <- dt.trade[dt.trade$year == input$CommYear, ]
    dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
    
    # 2. Convert edge list to an igraph network
    # igraph wants our data in matrix format
    m.trade <- as.matrix(dt.trade.edgelist) 
    g.trade <- graph_from_edgelist(m.trade, directed=TRUE)
    
    # 3. Set vertex attributes using set_vertex_attr()
    l.reporters <- as.list(unique(dt.trade.year$reporter_name))
    l.partners <- as.list(unique(dt.trade.year$partner_name))
    l.countries <- as.list(unique(append(l.reporters, l.partners)))
    dt.country.coordinates <- dt.trade.year %>% distinct(partner_name, .keep_all = TRUE)
    dt.country.coordinates <- dt.country.coordinates[, c("partner_name", "partner_lat", "partner_long")]
    meta <- dt.country.coordinates %>% rename("name" = "partner_name", "lat" = "partner_lat", "lon" = "partner_long")
    
    meta <- meta[match(V(g.trade)$name, meta$name), ]
    V(g.trade)$lat <- meta$lat
    V(g.trade)$lon <- meta$lon
    
    # 4. Set the weight of the edges (trade_value_usd) & add the year as an attribute
    E(g.trade)$weight <- dt.trade.year$trade_value_usd
    # Remove vertices with degree 0
    g.trade <- delete.vertices(g.trade, which(degree(g.trade) == 0))
    
    # 5. Community detection algorithms
    # Run Walktrap algorithm
    walktrap_communities <- walktrap.community(g.trade, weights = E(g.trade)$weight)
    # Get the community membership vector
    membership_vec <- membership(walktrap_communities)
    # Add the community as an attribute to the vectors
    V(g.trade)$community <- membership_vec[V(g.trade)$name]
    
    vertex_df <- get.data.frame(g.trade, what= c("vertices")) # convert vertex data to a data frame
    community_table <- vertex_df %>%
      group_by(community) %>%
      summarise(vertex_names = list(name)) %>%
      ungroup() %>%
      mutate(vertex_names = sapply(vertex_names, paste, collapse = ", "))
    community_table
  })
  
  output$CommModularity <- renderPlot({
    # Create a dataframe to store the modularity scores for each year
    modularity_df <- data.frame(year = numeric(), modularity_score = numeric())
    
    # Loop over each year and compute the modularity score
    for (element in 2000:2021) {
      # 1. Make edge list
      dt.trade.year <- dt.trade[dt.trade$year == element, ]
      dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
      # 2. Convert edge list to an igraph network
      m.trade <- as.matrix(dt.trade.edgelist) 
      g.trade <- graph_from_edgelist(m.trade, directed=TRUE)
      # 3. Set the weight of the edges (trade_value_usd) & add the year as an attribute
      E(g.trade)$weight <- dt.trade.year$trade_value_usd
      # 4. Remove vertices with degree 0
      g.trade <- delete.vertices(g.trade, which(degree(g.trade) == 0))
      # 5. Community detection: Walktrap algorithm
      walktrap_communities <- walktrap.community(g.trade, weights = E(g.trade)$weight)
      modularity <- modularity(walktrap_communities, g.trade)
      
      # 6. Store the modularity score in the dataframe
      modularity_df <- rbind(modularity_df, data.frame(year = element, modularity_score = modularity))
    }
    
    # Plot the modularity scores over time
    ggplot(modularity_df, aes(x = year, y = modularity_score)) +
      geom_line() +
      scale_x_continuous(breaks = seq(2000, 2021, by = 2)) +
      xlab("Year") +
      ylab("Modularity Score")
  })
  
  output$CommTextModularity <- renderUI({
    HTML(paste(" ", "<br>",
               "We analyze the modularity score of our trade network over time 
               to identify patterns indicating a breakdown in community structure. 
               A decrease in score may result from factors like globalization or 
               new trade relationships. The modularity score showed a steady 
               decrease in 2000-2013, with a steeper drop in 2013-2015. The crash 
               in modularity score, and thus an increase in globalization, may 
               be explained by trade liberalization, e-commerce, and emerging 
               markets like China, India, and Brazil. From 2015-2018, the 
               modularity score increased from 0.22 to 0.29, suggesting a 
               decrease in globalization, possibly due to protectionist policies 
               by the UK (Brexit), the US (Trump’s tariffs), and China's economic 
               slowdown.",
               " ", "<br>",
               " ", "<br>"))
  })
}