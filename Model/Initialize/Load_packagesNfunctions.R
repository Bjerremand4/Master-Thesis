# Load Packages  ----------------------------------------------------------
# load_package <- function(package_name) {
#   # Check if the package is installed
#   if (!requireNamespace(package_name, quietly = TRUE)) {
#     # If not installed, install the package
#     install.packages(package_name, dependencies = TRUE)
#   }
#   
#   # Load the package
#   library(package_name, character.only = TRUE)
# }
# 
# 
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
#   "cowplot")
# 
# 
# 
# 
# lapply(all_packages, load_package)



# Load Packages -----------------------------------------------------------
# Load or install and load packages
# Load or install and load packages
load_or_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    if (!requireNamespace(package_name, quietly = TRUE)) {
      message(paste("Package", package_name, "could not be installed. Check your internet connection and try again."))
      return(NULL)
    }
  }
  require(package_name, character.only = TRUE)
}

# List of all packages
all_packages <- c(
  "tidyverse", "patchwork", "lubridate", "readxl", "magick",
  "ggplot2", "gghalves", "gridExtra", "dplyr", "tidyr", "statnet",
  "scales", "RColorBrewer", "gtools", "deSolve", "ggthemes",
  "paletteer", "shiny", "shinyjs", "minpack.lm", "cowplot"
)

# Load or install and load all packages
installed_packages <- lapply(all_packages, load_or_install_package)

# -------------------------------------------------------------------------
#                                FUNCTIONS 
# -------------------------------------------------------------------------

# Logistic function -------------------------------------------------------
Logistic_model <- function(N, I0, t, beta){
  I = (N*I0)/(I0+(N-I0)*exp(-beta*t))
  prev = I/N
  
  return(c(I, prev))
}

# Logistic function for time to reach sprecific I -------------------------
time_log <- function(N, I0, target_I, beta){
  C <- I0 / (I0 + (N - I0))
  t <- log(((N - I0) * target_I) / (I0 * (N - target_I))) / beta
  
  t[t < 0] <- 0
  t <- ceiling(t)
  
  return(t)
}


# Visual inspection -------------------------------------------------------
Visual_inspection <- function(prevalences, sensitivities, n_farms){
  p = c(1:n_farms)
  for (i in 1:n_farms){
    test_result = rbinom(length(sensitivities),1,sensitivities)
    idx = min(which(test_result > 0))
    p[i] = prevalences[idx] 
  }
  return(p)
}


# Find next monday, given any date  ---------------------------------------
next_monday <- function(current_date) {
  if(wday(current_date) == 2){      # If the current data is already a Monday shift it one day ahead to avoid getten the same date output
    current_date = current_date + days(1)
  }
  while (wday(current_date) != 2) {  # While current date is not Monday
    current_date <- current_date + days(1)  # Add 1 day to the current date
  }
  return(current_date)
}


# Function to read data from a file based on its extension
read_data <- function(file) {
  ext <- tools::file_ext(file)
  if (ext == "rds" | ext == "RDS") {
    return(readRDS(file))
  } else if (ext == "csv") {
    return(read.csv(file))
  } else if (ext == "txt") {
    return(read.table(file))
  } else {
    stop(paste("Unsupported file type:", ext))
  }
}