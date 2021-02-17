# GoT-Visualization-Project

A RShiny App using dplyr, ggplot, cartography and sf.
Which exploits the Game Of Thrones dataset to do data Visualizations.

**Note: Check the Project_Report.pdf  for more informations.**

## Installation:
Open terminal and type the following commands: 

```bash
git clone https://github.com/SkanderMarnissi/GoT-Visualization-Project/
```
After downloading:

```bash
cd GoT-Visualization-Project
```

Then install all packages/libraries that needs to be installed(You can find them in the app.R file at the beginning):

```r
# Load R packages
library(shiny)
library(shinythemes)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(sf)
library(tidyr)
library(rgeos)
library(rgdal)
library(sp)
library(cartography)
library(shinyWidgets)
```

## How it works?
In this project, the final output is a Shiny App that launches to a Home Page and implements 4 interfaces which describe all the dataset in different sections that we can navigate through thanks to a navigation bar at the top of the application interface.

**Note: You can find all the GUI(Graphical User Interfaces) files under .got/data/GoTRelease/ directory**

# Usage 
In order to process the program and make it work you need to:

### Step 1: install the required packages/libraries.

#### To install packages/libraries: 

In order to run your server you should execute this chunk of R code(That you can find on the top of the app.R file) in your IDLE: 

```r
# Load R packages
library(shiny)
library(shinythemes)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(sf)
library(tidyr)
library(rgeos)
library(rgdal)
library(sp)
library(cartography)
library(shinyWidgets)

```

### Step 2: Now execute the app.R file in your IDLE:
Just run the file in your IDLE.


**Note: you can find all the datasets under .got/data directory**



*SKANDER MARNISSI COPYRIGHT © 2020 - ALL RIGHTS RESERVED*