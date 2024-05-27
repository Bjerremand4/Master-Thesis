# -------------------------------------------------------------------------
#                       TEST MODEL VISUAL 
# -------------------------------------------------------------------------


# Define prevalences and sensitivities
p =  c(0.05, 0.07, 0.10, 0.15, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)
#s =  c(0.10, 0.15, 0.20, 0.50, 0.80, 0.90, 0.95, 0.99, 0.99, 0.99, 0.99, 0.99, 1.00)
s =  c(0.05, 0.07, 0.10, 0.20, 0.40, 0.50, 0.70, 0.75, 0.80, 0.85, 0.90, 0.99, 1.00)


# Allocate space to store detection date for each farm
Detection_result = tibble(init_farm = numeric(),
                        detection_farm  = numeric(),
                        detection_prev = numeric(),
                        detection_date = Date(),
                        propagation = numeric(),
                        days_since_intro = numeric())

start_2b = Sys.time()
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
                                              detection_farm = trans.data %>% filter(detection_date == d_date) %>% pull(farmID),
                                              detection_prev = trans.data %>% filter(detection_date == d_date) %>% pull(d_prev),
                                              detection_date = d_date,
                                              propagation = propagation,
                                              days_since_intro = as.numeric(d_date - trans.data$date_of_infection[1]))
  
  print(paste("Module2b interation:", farm, "/", length(herdID)))
}

end_2b = Sys.time()
RunTime2b = end_2b - start_2b

# # Save
saveRDS(Detection_result, file.path("../Data/2b_VisualInspection", paste0("Detect_beta", b, "move_",move_freq,"%.rds")))

# # Load previous run
# Detect_result = readRDS("../Data/2b_VisualInspection/Detect_vir.rds")
# 
# detect = Detect_result %>% group_by(days_since_intro) %>% count()
# 
# hist(Detection_date$days_since_intro)
# hist(Detection_date$propagation)
