library(rjson)
library(data.table)
library(magrittr)
library(dplyr) 

# Load data --------------------------------------------------------------------
dt.trade <- fread("src/cleaned_trade_data.csv")
json_data <- fromJSON(file = 'src/continents.json')

#Prepare data for Map Network--------------------------------------------

#Creating a list of unique countries SHOULD WE CHANGE THIS TO PARTNER NAME ?????????
l.countries <- as.list(unique(dt.trade$reporter_name))

#Creating a data table with the unique names of countries
dt.country.coordinates <- dt.trade %>% distinct(partner_name, .keep_all = TRUE)

#Selecting only the columns of country name , longitude and latitude and renaming them columns for future plotting
dt.country.coordinates <- dt.country.coordinates[, c("partner_name", "partner_lat", "partner_long")]
meta <- dt.country.coordinates %>% rename("name" = "partner_name", "lat" = "partner_lat", "lon" = "partner_long")

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
  g
}