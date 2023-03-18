# International Trade ShinyApp

## Table of Contents

1. [Overview](#overview)
2. [Contributors](#contributors)
3. [Links](#links)
4. [Installation](#installation)
5. [Setup](#setup)
6. [Deployment to Shinyapps.io](#deployment-to-shinyappsio)
7. [Project Structure](#project-structure)
8. [Data](#data)
9. [License](#license)
10. [Acknowledgments](#acknowledgments)

## Overview

The International Trade ShinyApp is an interactive tool for visualizing and analyzing trade relationships between countries. The app leverages the powerful visualization and data processing capabilities of R to help users better understand the complexities of international trade networks.

## Contributors

- Carl Krogmann (55361)
- Ines Dewever (55944)
- Monica Navas (54577)
- Jannis Schmidt (54616)

## Links

- [ShinyApp](https://carlkrg.shinyapps.io/InternationalTrade/)
- [GitHub Repository](https://github.com/InesDew/InternationalTrade_ShinyApp)

---
## Installation

Ensure you have R version 4.2.2 (2022-10-31) or later installed. Then, install the required R packages:

    install.packages(c("shiny", "bslib", "leaflet", "DT", "ggplot2", "scales", "igraph", "rgdal", "rjson", "data.table", "magrittr", "dplyr"))

Load the packages in your R session:

- library(shiny)
- library(bslib)
- library(leaflet)
- library(DT)
- library(ggplot2)
- library(scales)
- library(igraph)
- library(rgdal)
- library(rjson)
- library(data.table)
- library(magrittr)
- library(dplyr)

## Setup

1. Install the required R packages as mentioned in the Installation section.
2. Place the raw data file `trade_1988_2021.csv` in the project's root directory, if not already available.
3. Run the `Setup.ipynb` file.
4. Run the `Setup.R` file.
5. Navigate to the `app/` folder and run the `app.R` file.

## Deployment to Shinyapps.io

To deploy the app to shinyapps.io, run the following commands in the R console:

    rsconnect::setAccountInfo(name='carlkrg', token='6A423720F63892ACDD9472D9C77A68E4', 
    secret='6hMMwyBlv2R0iJoZKGu1GgQ73gPIXo/lT7nYwX/G')

    rsconnect::deployApp(appName = "InternationalTrade", "/path/to/your/InternationalTrade_ShinyApp/app")

Adjust the local path to the Shiny app project on your local machine if necessary.

---
## Project Structure

The project's structure is organized into folders and files with specific purposes:


| File/Folder         | Description                                                   |
|---------------------|---------------------------------------------------------------|
| LICENSE             | License information for the project                           |
| Setup.ipynb         | Jupyter notebook for initial data setup                       |
| Setup.R             | R script for preprocessing and generating required data       |
| trade_1988_2021.csv | Raw data file containing international trade information      |
| README.md           | This file, containing documentation and instructions          |
| Testing (folder)    | (Optional) Contains any testing or additional development files|
| app (folder)        | Folder containing the Shiny app files                         |
| - app.R             | Main Shiny app file, including UI and server components       |
| - server.R          | Server-side processing for the Shiny app                      |
| - ui.R              | User interface definition for the Shiny app                   |
| - global.R          | Global variables and functions for the Shiny app              |
| - src (folder)      | Folder containing data for the app to run, like cleaned data  |
| - rsconnect (folder)| Folder containing build files created for the deployment      |


## Data

The data used in this ShinyApp is sourced from the [International Trade Database on Kaggle](https://www.kaggle.com/datasets/appetukhov/international-trade-database). The Kaggle dataset contains comprehensive information on international trade relationships from 1988 to 2021, including trade values, reporter and partner country names, and geographical information. This rich dataset serves as the foundation for the app's visualizations and analyses. The information in the dataset is collected from the [World Integrated Trade Solution (WITS)](https://wits.worldbank.org/Default.aspx?lang=en) platform, a software developed by the World Bank, in close collaboration with United Nations Conference on Trade and Development (UNCTAD), International Trade Center (ITC), United Nations Statistical Division (UNSD), and World Trade Organization (WTO).

## License

Apache License, Version 2.0, January 2004
http://www.apache.org/licenses/

## Acknowledgments

Special thanks to OpenAI's AI model for its assistance in creating this README.