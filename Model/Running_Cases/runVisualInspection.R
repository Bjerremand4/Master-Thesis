### Initialize
# Load packages -----------------------------------------------------------
source(file.path("../Model/Initialize/Load_packages.R"))
source(file.path("../Model/Initialize/Define_Functions.R"))
rawData = FALSE # Change if you have access to row data
source(file.path("../Model/Initialize/LoadNClean_Data.R"))

frequencies = c(100, 50, 33)
betas = c(0.05, 0.2)
nr = 0

start_2b = Sys.time()
for (move_freq in frequencies){
  for (b in betas){
    # Allocate space to store detection date for each farm
    Detection_result = tibble(init_farm = numeric(),
                              detection_farm  = numeric(),
                              detection_prev = numeric(),
                              detection_date = Date(),
                              propagation = numeric(),
                              days_since_intro = numeric())
    
    # Get transmission data 
    transmission.data = readRDS(file.path("../Data/1_Transmission", paste0("trans_move",move_freq,"%_beta", b, ".rds")))
    
    
    for (farm in 1:length(herdID)){
      # Extract data for given farm
      trans.data = transmission.data[[farm]][[2]]
      
      
      # Find prevalence at detection for each farm in this transmission chain
      trans.data = trans.data %>% mutate(d_prev = Visual_inspection(p, s, nrow(trans.data)))
      
      # Use log function to find time at which the detection prevalence (d_prev) is reached
      trans.data = trans.data %>% 
        mutate(Time_discover = time_log(N = Capacity, I0 = init_inf, target_I = Capacity*d_prev, beta = b))
      
      # Find specific dates of detection
      trans.data = trans.data %>% mutate(detection_date = date_of_infection + Time_discover)
      
      # Find first date of detection
      d_date = trans.data %>% filter(detection_date == min(detection_date)) %>% pull(detection_date)
      # Ensure d_date is a single value
      if (length(d_date) > 1) {
        d_date <- d_date[1] # Take the first date if there are multiple
      }
      
      
      # Find propagation at time of detection
      farm_inf = transmission.data[[farm]][[2]]#[complete.cases(transmision.data[[as.character(init_farm)]][[2]]), ]
      propagation = farm_inf %>% filter(d_date >= date_of_infection) %>% nrow()
      
      # Fill in info in Detection_date date frame
      Detection_result = Detection_result %>% add_row(init_farm = trans.data$farmID[1],
                                                      detection_farm = trans.data %>% filter(detection_date == d_date) %>% slice(1) %>% pull(farmID),
                                                      detection_prev = trans.data %>% filter(detection_date == d_date) %>% slice(1) %>% pull(d_prev),
                                                      detection_date = d_date,
                                                      propagation = propagation,
                                                      days_since_intro = as.numeric(d_date - trans.data$date_of_infection[1]))
      
      print(paste0("Module2b interation: ", farm, " / ", length(herdID), " ", move_freq,"%",b))
    }
    # # Save
    saveRDS(Detection_result, file.path("../Data/2b_VisualInspection", paste0("Detect_beta", b, "move_",move_freq,"%.rds")))
    
  }
}

end_2b = Sys.time()
RunTime2b = end_2b - start_2b


