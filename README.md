# Master-Thesis Model
This GitHub contains the scripts related to the Master Thesis: **_Integrating Livestock Movement Data in a Dynamic Model for Spread and Surveillance of New Infectious Agents in the Danish Pig Production_**

This study presents a dynamic model designed to simulate the spread of newly introduced infectious agents in the Danish Commercial pig production and the effect of surveillance. The model specifically evaluates two main aspects: the total number of infected herds at detection and the time from introduction to detection under various surveillance strategies, and movement frequencies.
wo types of testing included; Testing at slaughter and observation of symptoms in livestock.

The model can be executed directly from its user interface, providing an accessible way to run simulations and obtain results without requiring additional setup.

Additionally, several Data Analysis scripts are included with the project. These scripts allow users to delve deeper into the data, specifically enabling the exploration of different movement patterns and dynamics for each herd. By using these scripts, you can gain a more comprehensive understanding of the dynamics at play in the spread of infectious agents within the network. 

## Table of Contents

- [Installation and Dependencies](#installation)
- [Usage](#usage)
- [Sripts Description](#Sripts-Description)
- [Coresponding Author](#Coresponding-Author)

  
## Installation and Dependencies
Dowload the project to you local device and run the scripts from here
### Prerequisites
- To run this model, you need to have R installed (version 4.2.3) to ensure all packages work as intended.
- Run the `interface.R` - this will automatically set your working directory and install/load the necessary packages.
- If you wish to explore the additional Data Analysis scripts, just open and run them separately. They will be initialized correctly directly in the script.

### R Packages
The following R packages from `Load_pakages.R` are required to rund the model. The script is automaticaly sourced whereever needed. 

General packages:
+ `tidyverse`: Multiple tools for data manpulation and data science.
+ `dyplyr`: Tools for data manipulation within `tidyverse`.
+ `tidyr`: Tools to create tidy data by reshaping and organizing within `tidyverse`.
+ `lubridate`: Easier handling and manipulation of dates and times.
+ `readxl`: enable reading of Excel files.
+ `deSolve`: Solvers for initial value problems of differential equations (e.g SIR).
+ `minipack.lm`: Nonlinear least-squares optimization and curve fitting - estimator for Beta.
  
Package used in visualization:
+ `ggplot2`: A system for declaratively creating graphics.
+ `gghalves`: Adds half-violin and half-box plots to `ggplot2`.
+ `ggthemes`: Extra themes, scales, and geoms for `ggplot2`.
+ `cowplot`: Streamlined plot theme and plot annotations for `ggplot2`.
+ `patchwork`: Combine multiple plots into one patchwork of plots.
+ `gridextra`: Display multiple plots in a grid for `ggplot2`.
+ `RColorBrewer`: Color palettes inspired by color schemes from ColorBrewer.
+ `paletter`: Collection of many color palettes for R.
  
Packages for building Interface:
+ `shiny`: Easy interactive web applications with R.
+ `shinyjs`: Perform common JavaScript operations in Shiny apps.
  
Packages used in building network and creation of gif:
+ `network`: Basic infrastructure for representing, manipulating, and visualizing relational data
+ `statnet`: Tools for the representation, visualization, analysis, and simulation of network data.
+ `magick`: Advanced graphics and image-processing in R - combine pictures to GIF.
+ `av`: Tools to work with audio and video, including creating animated GIFs.
+ `scales`: Graphical scales for `ggplot2`.
+ `gtools`: Various R programming tools, including the `mixedsort` function for sorting images for GIF creation.

## Usage
1. **Running the Model:** Execute the main interface script to start the model. This script will guide you through the simulation process and allow you to adjust various parameters.
2. **Exploring Data Analysis Scripts:** Open and run the Data Analysis scripts to investigate movement patterns and infection dynamics for each herd. These scripts provide additional insights beyond the main model output.

## Files 
### Sripts Description
The repository include the following files
+ `interface.R` - Main script to run the model simulations.
  
**Initialize:**
+ `Load_packages.R` - Installs and load all necesary packages
+ `Define_Functions.R` - Defines all functions to be utalized in the model.
+ `LoadNClean_Data.R` - Load the available raw data from the Data/Raw folder, and manipulates it so its ready for simulation

**Modules:**
+ `RunningModel.R` - Model that sources all the modules and script used for running the model with the userset parameters. The script is sourced through `interface.R`
+ `ReduceMovements.R` - Merges moves in data together so the movement frequency is reduced to a specific percentage specified in `interface.R`. The volume of livestock is identical but shipped on the earliest of the combined moves. Movements to slaughter are not merged but kept constant.
+ `Module1_Transmission.R` - Simulate the transmission of an infectious agent in the industry, given the following parameters all set in `interface.R`; Transmission rate (beta), Movement frequency, Test intencity  and corresponding test specificity. Each iteration in the simulation represent a possible index herd of the infectious agent. The output is a full transmission pattern for each index herd, which is stored in the Folder `Data/Module1_transmission`. 


## Corresponding Author
Anna Sophie Bjerremand Jensen, Research Group for Genomic Epidemiology, Technical University of Denmark
