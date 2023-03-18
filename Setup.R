# In this file we preprocess the edges spatial object we use in the Communities
# analysis part of our app. This is done because the loading of the map 
# visualization was taking up to 2 minutes.

library(rgdal)
library(sf)
library(data.table)
library(igraph)
library(dplyr)  

data.file.path <- "app/src/cleaned_trade_data.csv"
dt.trade <- fread(data.file.path)

for (year in seq(2000, 2021)) {
  file.name <- paste0("edges_", year)
    # 1. Make edge list
    dt.trade.year <- dt.trade[dt.trade$year == year, ]
    dt.trade.edgelist <- dt.trade.year[ , c('reporter_name', 'partner_name')]
    
    # 2. Convert edge list to an igraph network
    # igraph wants our data in matrix format
    m.trade <- as.matrix(dt.trade.edgelist) 
    g.trade <- graph_from_edgelist(m.trade, directed = TRUE)
    
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
    
    # 4. Set the weight of the edges (trade_value_usd) & add the year as an attr
    E(g.trade)$weight <- dt.trade.year$trade_value_usd
    # Remove vertices with degree 0
    g.trade <- delete.vertices(g.trade, which(degree(g.trade) == 0))
    
    # 5. Community detection: Walktrap algorithm
    l.walktrap.communities <- walktrap.community(g.trade, weights = E(g.trade)$weight)
    # Get the community membership vector
    membership <- membership(l.walktrap.communities)
    # Add the community as an attribute to the vectors
    V(g.trade)$community <- membership[V(g.trade)$name]
    
    # extract the vertices and edges from the g graph object and store them in df
    gg <- get.data.frame(g.trade, "both")
    # assign vert variable with dataframe info on the vertices + 
    # add the spatial coordinates of the points to vert
    vert <- gg$vertices
    coordinates(vert) <- ~lon + lat
    
    # assign edges and weights variable
    edges <- gg$edges
    weights <- E(g.trade)$weight
    
    # Create a list of spatial objects
    edges <- lapply(1:nrow(edges), function(i) 
    {as(rbind(vert[vert$name == edges[i, "from"], ], 
              vert[vert$name == edges[i, "to"], ]), 
        "SpatialLines")
    })
    
    # assign unique IDs to each of the SpatialLines objects
    for (i in seq_along(edges)) {
      edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
    }
    
    # combine all of the SpatialLines objects + new SpatialLinesDataFrame object 
    # using the combined SpatialLines object and the edges_df data frame.
    edges <- do.call(rbind, edges)
    df.edges <- data.frame(id = seq_along(edges), weight = weights)
    edges <- SpatialLinesDataFrame(edges, data = df.edges)
    
    # We save the SpatialPointsDataFrame edges
    writeOGR(edges, dsn = "app/src", layer = file.name, driver = "ESRI Shapefile")

}
