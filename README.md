# International Trade ShinyApp

## Contributor
- Carl Krogmann 55361
- Ines Dewever 55944
- Monica Navas 54577
- Jannis Schmidt 54616 

---
## Links: 

- https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/
- https://rstudio-education.github.io/shiny-course/

---
## Color Guide:
- Primary: `#6B63F6 ` ![#6B63F6](https://placehold.co/15x15/6B63F6/6B63F6.png)
- Secondary: `#ED7185` ![#ED7185](https://placehold.co/15x15/ED7185/ED7185.png)

---
## Table of Contents
1. Trade Network
2. KPI over the years
3. The virus spread
4. Descriptives

---
## Trade Network - Monica
![image](/src/images/image4.png)
Modeled after the shiny flights app ‘airport network’.

### Input:
- [x] Country (only 1)
- [x] Export / Import
- [x] Year (only 1 for now, a slider of a range of years if possible)

### Output:
- [ ] A world map that shows all import/export connections of the selected country. We should show the weight of the trade connection (weight = value in USD). A few ideas to show this weight:
    - you click on a connection and you have a label with partner_name & the value in USD
    - you get a data table under the map that shows: our selected country, partner_name & corresponding value in USD
    - The color of the connection corresponds to the value in USD and you have a color scale under the map like this:

- [ ] A data table with the following information for the selected country
    - [ ] Degree Centrality (also split into in-degree centrality and out-degree centrality)
    - [ ] Closeness centrality
    - [ ] Betweenness centrality
    - [ ] Eigenvector centrality
    - [ ] Clustering coefficient
- EXTRA: Be able to select a second country (keep import/export and years selected the same) and show the same information for that country (map, weights, centralities) at the same time as the first selection. Kind of like here on shiny flights:
![image](/src/images/image2.png)

---
## KPI over the years - Jannis
![image](/src/images/image1.png)
Multi line chart like above with x-axis years & y-axis a value.

### Input:
- [ ] Countries to compare (max 5.)
- [ ] A year range for the x-axis
- [ ] A KPI to plot for the y-axis (value of export in USD, the value of import in USD, Amount of partners in export/import, the average value of export/import in USD to a partner country

### Output:
- [ ] A line plot of the evolution of the KPI for the selected countries in the selected time range

---
## The virus spread - Ines
Plot the SI infection/virus/fake news/… model for one or multiple countries
![image](/src/images/image3.png)

### Input:
- [ ] Selected countries (up to 5)
- [ ] Transmission rate
### Output:
- [ ] A plot with an x-axis time
- [ ] y-axis with the number of countries infected
- [ ] If you hover over the lines you should see a label with the country & (x,y) values

---
## Descriptives - Carl
Show descriptives for a selected continent

### Input: 
- [ ] Select a continent, select the whole network

### Output:
- [ ] a map that lights up the selected continent
- [ ] Degree distribution plot
- [ ] continent X exports to … countries and receives imports from … countries
- [ ] Datatable
    - [ ] Number of nodes/vertices
    - [ ] Number of edges
    - [ ] The average degree of countries
    - [ ] Median degree of countries
    - [ ] The average value in USD of edges
    - [ ] The median value in USD of edges
    - [ ] The minimum value in USD of edges
    - [ ] The maximum value in USD of edges
    - [ ] The standard deviation of value in USD of edges




