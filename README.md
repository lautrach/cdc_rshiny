# CDC Bottle Bioassay Data Analyzer

## Overview
This app is designed to improve the user experience with the CDC bottle bioassay kit by providing a more efficient and intuitive way to analyze and interpret insecticide resistance data. By offering built-in visualizations and direct text-based recommendations, the app reduces the need to repeatedly reference the manual, streamlining the analysis process. 


## Features
- Upload CSV or Excel files and process them in real time  
- Interactive plots on Percent Mortality over Time 
- Direct text-based recommendations with intesticide and time 
- Download results and figures  

## Installation
Make sure you have R (≥ 4.0.0) and the following R packages installed:
```r
install.packages(c("shiny", "tidyverse", "ggplot2", "leaflet", "mgcv"))
