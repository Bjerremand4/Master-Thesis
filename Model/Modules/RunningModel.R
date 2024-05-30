# -------------------------------------------------------------------------
#                              Running model 
# -------------------------------------------------------------------------

# Track time
Start_time = Sys.time()

# ----------------- Load packages, data and functions -----------------------
# Load and manage data  
source(file.path("Initialize/LoadNClean_Data.R"))

# Check if model is run with default
move_freq_default = (move_freq == 100 | move_freq == 50 | move_freq == 33)
beta_default = (b == 0.05 | b == 0.2)
test_default = ((n_test == 5 | n_test == 50) & (test_sensitivity == 0.85 | test_sensitivity == 0.7))



# --------------------------- Reduce movements --------------------------------
# If a non_preset movement frequency is set ReducedMovements scripts is run
if (rawData == T & move_freq_default == F){
  # Run Reduce movement script 
  source(file.path("Modules/ReduceMovements.R"))
} else if (rawData == F & move_freq != 100 & move_freq != 50 & move_freq != 33) {
  # Error message
  print("Reduce movements was tried without raw data, please try set movement frequency(move_freq) to 100, 50 or 33")
}



# --------------------------- Module1 Transmission -------------------------------- 
if (rawData == T & (move_freq_default == F | beta_default == F)){
  # Set move.data to the reduced data run before
  move.data = read_data(file.path(dirname(getwd()), paste0("Data/Reduced_moves/sub.move.data_", move_freq,"%.rds")))
  # Run transmission
  source(file.path("Modules/Module1_Transmission.R"))
} 

# Load transmission data corresponding to the chosen movement frequency
transmission.data = readRDS(file.path(dirname(getwd()), paste0("Data/1_Transmission/trans_move",move_freq,"%_beta", b, ".rds")))





# --------------------------- Module 2a Test Slaughter -------------------------------- 
if ( move_freq_default == T & beta_default == T & test_default == T){
  Farms_result = readRDS(file.path(dirname(getwd()),paste0("Data/2a_TestSlaughter/TestSlaughter",move_freq, "%_beta", b,"_sample", n_test, "_sens", test_sensitivity, ".rds")))
} else {
  source(file.path("Modules/Module2a_TestSlaughter.R"))
}




#--------------------------- Module 2b Visual Inspection --------------------------------
#This module is only run if the agent is a pathogen = clinical symptoms will appear in the animals
if (pathogen == T){
  if(move_freq_default == T & beta_default == T){
    Detection_result = readRDS(file.path(dirname(getwd()),paste0("Data/2b_VisualInspection/Detect_beta", b, "move_",move_freq,"%.rds")))
  } else {
    source(file.path("Modules/Module2b_VisualInspection.R"))
  }
}
# if (pathogen == T){ 
#   source(file.path("Modules/Module2b_VisualInspection.R"))
# }   

#------------------------- Module 3 Visualization --------------------------------
# Run visualization regardless of chosen parameters
source(file.path("Modules/Module3_Visualization.R"))

# Track time
End_time = Sys.time()
time_value <- sprintf("%.2f", as.numeric(difftime(End_time ,Start_time)))
time_unit <- attr(difftime(End_time ,Start_time), "units")
Time_model = paste(time_value, time_unit)

# Finish runing messages
print(paste("The model have finished running"))

