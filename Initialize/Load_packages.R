# Load pakages 
load_package <- function(package_name) {
  # Check if the package is installed
  if (!requireNamespace(package_name, quietly = TRUE)) {
    # If not installed, install the package
    install.packages(package_name, dependencies = TRUE)
  }
  
  # Load the package
  library(package_name, character.only = TRUE)
}


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
  "paletteer")




lapply(all_packages, load_package)


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

  while (wday(current_date) != 2) {  # While current date is not Monday
    current_date <- current_date + days(1)  # Add 1 day to the current date
  }
  return(current_date)
}




