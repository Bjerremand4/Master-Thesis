# -------------------------------------------------------------------------
#                   Installation and loading of packages 
# -------------------------------------------------------------------------

# Manually install and load all packages ----------------------------------
# Check if installation is needed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("patchwork")) install.packages("patchwork")
if (!require("lubridate")) install.packages("lubridate")
if (!require("readxl")) install.packages("readxl")
if (!require("magick")) install.packages("magick")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gghalves")) install.packages("gghalves")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("statnet")) install.packages("statnet")
if (!require("scales")) install.packages("scales")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("gtools")) install.packages("gtools")
if (!require("deSolve")) install.packages("deSolve")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("paletteer")) install.packages("paletteer")
if (!require("shiny")) install.packages("shiny")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("minpack.lm")) install.packages("minpack.lm")
if (!require("cowplot")) install.packages("cowplot")
if (!require("grid")) install.packages("grid")
if (!require("ggtext")) install.packages("ggtext")

# Load
library("tidyverse")
library("patchwork")
library("lubridate")
library("readxl")
library("magick")
library("ggplot2")
library("gghalves")
library("gridExtra")
library("dplyr")
library("tidyr")
library("statnet")
library("scales")
library("RColorBrewer")
library("gtools")
library("deSolve")
library("ggthemes")
library("paletteer")
library("shiny")
library("shinyjs")
library("minpack.lm")
library("cowplot")
library("grid")
library("ggtext")



# Load packages Old version-----------------------------------------------------------
# Function to unload packages (made by ChatGPT)
# unload_all_packages <- function() {
#   loaded_pkgs <- names(sessionInfo()$otherPkgs)
#   if (length(loaded_pkgs) > 0) {
#     lapply(paste('package:', loaded_pkgs, sep = ""), detach, character.only = TRUE, unload = TRUE)
#   }
# }
# 
# # Unload pre loaded packages
# unload_all_packages()

# List of all packages needed for the model 
# all_packages <- c(
#   "tidyverse",
#   "patchwork",
#   "lubridate",
#   "readxl",
#   "magick",
#   "ggplot2",
#   "gghalves",
#   "gridExtra",
#   "dplyr",
#   "tidyr",
#   "statnet",
#   "scales",
#   "RColorBrewer",
#   "gtools",
#   "deSolve",
#   "ggthemes",
#   "paletteer",
#   "shiny",
#   "shinyjs",
#   "minpack.lm",
#   "cowplot"
# )

# # Find packages that are not already installed 
# missing_packages <- all_packages[!sapply(all_packages, requireNamespace, quietly = TRUE)]
# 
# # Print a message for the user, about which packages are going to be installed
# if (length(missing_packages) > 0) {
#   cat("The Following pakages wil now be installed:\n")
#   cat(missing_packages, sep = "\n")
#   
#   # Installement of missing packages
#   install.packages(missing_packages, dependencies = TRUE)
# } else {
#   cat("All packages are already installed.\n")
# }
# 
# # Constrol and update specific packages to make it run smooth on macOS and windows 
# update_packages <- c("magrittr", "cli")
# for (pkg in update_packages) {
#   if (!requireNamespace(pkg, quietly = TRUE) || packageVersion(pkg) < "2.0.3") {
#     install.packages(pkg, dependencies = TRUE)
#   }
# }

# # Load all packages
# sapply(all_packages, function(pkg) {
#   library(pkg, character.only = TRUE)
# })

