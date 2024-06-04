# -------------------------------------------------------------------------
#                   Installation and loading of packages 
# -------------------------------------------------------------------------
# Load packages -----------------------------------------------------------
# Function to unload packages (made by ChatGPT)
unload_all_packages <- function() {
  loaded_pkgs <- names(sessionInfo()$otherPkgs)
  if (length(loaded_pkgs) > 0) {
    lapply(paste('package:', loaded_pkgs, sep = ""), detach, character.only = TRUE, unload = TRUE)
  }
}

# Unload pre loaded packages
unload_all_packages()

# List of all packages needed for the model 
all_packages <- c(
  "tidyverse",
  "patchwork",
  "lubridate",
  "readxl",
  "magick",
  "ggplot2",
  "gghalves",
  "gridExtra",
  "dplyr",
  "tidyr",
  "statnet",
  "scales",
  "RColorBrewer",
  "gtools",
  "deSolve",
  "ggthemes",
  "paletteer",
  "shiny",
  "shinyjs",
  "minpack.lm",
  "cowplot"
)

# Find packages that are not already installed 
missing_packages <- all_packages[!sapply(all_packages, requireNamespace, quietly = TRUE)]

# Print a message for the user, about which packages are going to be installed
if (length(missing_packages) > 0) {
  cat("The Following pakages wil now be installed:\n")
  cat(missing_packages, sep = "\n")
  
  # Installement of missing packages
  install.packages(missing_packages, dependencies = TRUE)
} else {
  cat("All packages are already installed.\n")
}

# Constrol and update specific packages to make it run smooth on macOS and windows 
update_packages <- c("magrittr", "cli")
for (pkg in update_packages) {
  if (!requireNamespace(pkg, quietly = TRUE) || packageVersion(pkg) < "2.0.3") {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load all packages
sapply(all_packages, function(pkg) {
  library(pkg, character.only = TRUE)
})



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
