install.packages("shiny")
library(shiny)

# Load the dataset
trade_data <- read.csv("trade_1988_2021.csv")

# Filter the data for the year 2021
trade_data_2021 <- subset(trade_data, Year == 2021)

# Calculate the total export for each country
total_export <- aggregate(Value ~ Reporter, data = trade_data_2021, sum)

# Plot the total export for each country
library(ggplot2)
ggplot(total_export, aes(x = Reporter, y = Value)) +
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("Total Export") +
  ggtitle("Total Export for Each Country in 2021")
