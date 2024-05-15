# -------------------------------------------------------------------------
#                       TEST MODEL VISIUEL 
# -------------------------------------------------------------------------

# Load packages -----------------------------------------------------------
source(file.path("../Initialize/Load_packages.R"))


# Load and clean data  -----------------------------------------------------
source(file.path("../Initialize/LoadNClean_Data.R"))

# Import data from Transmission model
# transmision.data <- readRDS("../Data/transmision.data.rds")
#transmision.data <- readRDS("../Data/full_trans_beta005.rds")[[1]]
transmision.data <- readRDS("../Data/trans_beta_virus.rds")
#transmision.data <- readRDS("../Data/trans_bac_reduceMove66.rds")

# Define prevalences and sensitivities
p =  c(0.05, 0.07, 0.10, 0.15, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)
#s =  c(0.10, 0.15, 0.20, 0.50, 0.80, 0.90, 0.95, 0.99, 0.99, 0.99, 0.99, 0.99, 1.00)
s =  c(0.05, 0.07, 0.10, 0.20, 0.40, 0.50, 0.70, 0.75, 0.80, 0.85, 0.90, 0.99, 1.00)


# Allocate space to store detection date for each farm
Detection_date = tibble(init_farm = numeric(),
                        detection_farm  = numeric(),
                        detection_prev = numeric(),
                        detection_date = Date(),
                        propagation = numeric(),
                        days_since_intro = numeric())

start = Sys.time()

for (farm in 1:length(herdID)){
  # Extract data for given farm
  trans.data = transmision.data[[farm]][[2]]
  
  
  # Find prevalence at detection for each farm in this transmission chain
  trans.data = trans.data %>% mutate(d_prev = Visual_inspection(p, s, nrow(trans.data)))
  
  # Use log function to find time at which the detection prevalence (d_prev) is reached
  trans.data = trans.data %>% 
    mutate(Time_discover = time_log(N = Capacity, I0 = init_inf, target_I = Capacity*d_prev, beta = bac_beta))
  
  # Find specific dates of detection
  trans.data = trans.data %>% mutate(detection_date = date_of_infection + Time_discover)
  
  # Find first date of detection
  d_date = trans.data %>% filter(detection_date == min(detection_date)) %>% pull(detection_date)
  
  # Find propagation at time of detection
  farm_inf = transmision.data[[farm]][[2]]#[complete.cases(transmision.data[[as.character(init_farm)]][[2]]), ]
  propagation = farm_inf %>% filter(d_date >= date_of_infection) %>% nrow()

  # Fill in info in Detection_date date frame
  Detection_date = Detection_date %>% add_row(init_farm = trans.data$farmID[1],
                                              detection_farm = trans.data %>% filter(detection_date == d_date) %>% pull(farmID),
                                              detection_prev = trans.data %>% filter(detection_date == d_date) %>% pull(d_prev),
                                              detection_date = d_date,
                                              propagation = propagation,
                                              days_since_intro = as.numeric(d_date - trans.data$date_of_infection[1]))
}

end = Sys.time()

# # Save
# saveRDS(Detection_date, file.path("../Data", "Detect_vir.rds"))

# Load previous run
Detect_result = readRDS("../Data/Detect_vir.rds")

detect = Detect_result %>% group_by(days_since_intro) %>% count()

hist(Detection_date$days_since_intro)
hist(Detection_date$propagation)
