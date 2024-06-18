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
1. **Running the Model:** Execute the `interface.R` script to start the model. This script guides you through the simulation process and allows you to adjust various parameters for a customized analysis.
2. **Exploring Data Analysis Scripts:** Open and run the Data Analysis scripts to investigate movement patterns and infection dynamics for each herd. These scripts offer additional insights beyond the main model output.
3. **Sensitivity analysis:** Explore the significance of the date of introduction of the infectious agent as well as the effect of randomsness within the between-herd transmission. 

## Files 
The Project contain four main folders:
- `Data` : This folder contains the raw data _(note that this data is not published, so users will need access to it or obtain it elsewhere)_. It also includes various datasets produced by running the model, both user-generated and pre-run by the author. Users can still use the model without access to the raw data but with limited options for transmission rate and movement frequency, while maintaining full flexibility for surveillance parameters (test intensity and test specificity).
- `Model` : This folder contains all the scripts that comprise the model. See specific features within each script in the "Script Description" section. This folder also includes `interface.R`, which is used to run the entire model.
- `DataAnalysis`: This folder contains scripts not used in the actual model. It includes various data analysis scripts exploring movement patterns and contact networks for each herd in the dataset, including the creation of a GIF dynamically showing the development of a contact network for a given herd. It also evaluates the SIR model versus the logistic growth model to evaluate the two methods against each other. It also contains a sensitivity analysis assessing the impact of certain assumptions/choices made during the model implementation.
- `Results`: This folder include the folder `OutputModel` where the results (visual outputs) are stored when running the model. It also contains several results produced for the purpose of the Master Thesis. All figures/plots used in the thesis manuscript are accessible here.

### Sripts Description
The repository include the following files
+ `interface.R` - Main script to run the model simulations.
  
**Initialize:**
+ `Load_packages.R` - Installs and load all necesary packages
+ `Define_Functions.R` - Defines all functions utilized in the model.
+ `LoadNClean_Data.R` - Loads and cleans the available raw data from the `Data/Raw` folder, preparing it for simulation.

**Modules:**
+ `RunningModel.R` - Sources all modules and scripts used for running the model with user-set parameters. The script is sourced through `interface.R`
+ `ReduceMovements.R` - Merges movements in the data to reduce movement frequency to a specific percentage specified in `interface.R`. The volume of livestock is identical but moved with the earliest of the combined moves. Movements to slaughter are not merged and remain constant.
+ `Module1_Transmission.R` - Simulate the transmission of an infectious agent in the industry, given the parameters set in `interface.R`; transmission rate (beta), movement frequency, test intensity, and test specificity. Each iteration in the simulation represents a possible index herd of the infectious agent. The output is a full transmission pattern for each index herd, stored in the `Data/Module1_transmission` folder. 


## Corresponding Author
Anna Sophie Bjerremand Jensen, Research Group for Genomic Epidemiology, Technical University of Denmark
