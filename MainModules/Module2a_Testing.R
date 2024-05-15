# -------------------------------------------------------------------------
#                       TEST MODEL AT ABATTOIRS
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------
source(file.path("../Initialize/Load_packages.R"))


# Load and clean data  -----------------------------------------------------
source(file.path("../Initialize/LoadNClean_Data.R"))

# Import data from Transmission model
# transmision.data <- readRDS("../Data/transmision.data.rds")
#transmision.data <- readRDS("../Data/trans_beta_bacteria2.rds")
#transmision.data <- readRDS("../Data/trans_bac_reduceMove.rds")
#transmision.data <- readRDS("../Data/trans_bac_reduceMove_slaughter.rds")
transmision.data <- readRDS("../Data/trans_bac_reduceMove66.rds")
#move.data = sub.move.data


# Remove all weekends including Fridays
# Find dates on all fridays, saturdays and sundays in 2020 - 2022
weekend_dates = c(unique(raw.data$DATE_MOVEMENT)[weekdays(unique(raw.data$DATE_MOVEMENT)) == "Friday"],
                  unique(raw.data$DATE_MOVEMENT)[weekdays(unique(raw.data$DATE_MOVEMENT)) == "Saturday"],
                  unique(raw.data$DATE_MOVEMENT)[weekdays(unique(raw.data$DATE_MOVEMENT)) == "Sunday"])

# Exclude weekends from data
move.data.week = move.data %>% filter(!(DATE_MOVEMENT %in% weekend_dates))

# Calculate the total daily amount of pigs sent for slaughter for the 10 largest slaughterhouses
daily_slaughter <- move.data %>% filter(RECEIVER_FARMTYPE == "15 68 Svineslagteri") %>% group_by(DATE_MOVEMENT) %>% summarise(daily_slaughter = sum(N_MOVED_PIGS))

# Extract all the Farmids working outside weekends 
week_IDs = unique(move.data.week %>% filter(Year < 2021) %>% pull(FARMID_SENDER))

# Allocate space to store result for each Farm
Farms_result = tibble(FarmID = numeric(),
                     Farmtype = character(),
                     Detection_date = Date(),
                     Propagation = numeric())

# Number of daily tests 
n_test = 5
test_sensitivity = 0.85

start = Sys.time()

for (farm in week_IDs){
  idx = which(farm == week_IDs)
  # set FarmID and type
  init_farm = farm
  farm_type = info_all_farms %>% filter(Farmid == init_farm) %>% pull(Farm_type)
  
  # Extract data corresponding to current FarmID
  inf_slaughter = transmision.data[[as.character(init_farm)]][[1]]
  
  
  # Storage for test results
  test_results = tibble(sample_date = inf_slaughter$inf_date[1], 
                        result_date = inf_slaughter$inf_date[1], 
                        result = 0)
  
  # Initialize detection, and date
  detection = F
  day = 0
  current.day = inf_slaughter$inf_date[1]
  
  while (detection == F & !is.na(current.day)){
    # New iteration 
    day = day +1
    
    # update date 
    current.day = inf_slaughter$inf_date[day]
    
    # Check test results from last week samples
    result = test_results %>% filter(current.day >= result_date) %>% summarise(n = sum(result)) %>% pull(n)
    
    if (result > 0){
      # Extract date of detection
      discover_date = test_results %>% filter(result == 1) %>% slice(1) %>% pull(result_date)
      # Extract number of farms infected at detection point (propagation)
      farm_inf = transmision.data[[as.character(init_farm)]][[2]]#[complete.cases(transmision.data[[as.character(init_farm)]][[2]]), ]
      propagation = farm_inf %>% filter(discover_date >= date_of_infection) %>% nrow()
      # Update detection status
      detection = T
      break
    }
    
    
    #Total number of pigs sent to slaughter current day
    slaughter_pigs = ifelse(is.na(current.day), 0, daily_slaughter %>% filter(DATE_MOVEMENT == current.day) %>% pull(daily_slaughter))
    
    
    if (slaughter_pigs > 0){
      # Find infected slaughter pigs 
      inf_slaughter_pigs = inf_slaughter$n_infected[day]
      
      # Take x samples --> test results
      test_prev = (inf_slaughter_pigs / slaughter_pigs)*test_sensitivity
      test_P = 1-((1-test_prev)^n_test)
      
      test_results = test_results %>% add_row(sample_date = current.day,
                                              result_date = next_monday(current.day),
                                              result = rbinom(n = 1, size = 1, prob = test_P))
    }
  }
  
  
  #Save result for given farm
  if (sum(test_results$result) > 0){
    Farms_result = Farms_result %>% add_row(FarmID = init_farm ,
                                            Farmtype = farm_type,
                                            Detection_date = discover_date,
                                            Propagation = propagation)
  }
  print(idx)  
}

end = Sys.time()

# Add the days before detection since introduction for each herd 
Farms_result = Farms_result %>% mutate(discover_days = as.numeric(difftime(Detection_date, as.Date("2020-01-01"))))

# Save the data
#saveRDS(Farms_result, file.path("../Data", "Slaughter_TestResult_reduce66%.rds"))
#Farms_result_50_slaughter <- readRDS("../Data/Slaughter_TestResult_reduce50%_slaughter.rds")


Farms_result_full = readRDS("../Data/Slaughter_TestResult_full.rds")
Farms_result_50 = readRDS("../Data/Slaughter_TestResult_reduce50%.rds")
Farms_result_50_slaughter <- readRDS("../Data/Slaughter_TestResult_reduce50%_slaughter.rds")


# Plot the number of infected herds at detection for all movements and 50% of movements
p_prop_full = ggplot(Farms_result_full, aes(x=Farmtype, y=Propagation, fill=Farmtype)) + 
  geom_half_violin(side = "r", alpha = 0.7) +
  geom_half_boxplot(side = "l", alpha = 0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Propagation at detection all movements",
       x = "Farmtype",
       y = "Number of infected Farms")

# Reduce moves but not to slaughterhouses 
p_prop_reduce_slaughter = ggplot(Farms_result_50_slaughter, aes(x=Farmtype, y=Propagation, fill=Farmtype)) + 
  geom_half_violin(side = "r", alpha = 0.7) +
  geom_half_boxplot(side = "l", alpha = 0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Propagation at detection 50% of movements normal slaughter",
       x = "Farmtype",
       y = "Number of infected Farms")

# Reduce all moves 
p_prop_reduce = ggplot(Farms_result_50, aes(x=Farmtype, y=Propagation, fill=Farmtype)) + 
  geom_half_violin(side = "r", alpha = 0.7) +
  geom_half_boxplot(side = "l", alpha = 0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Propagation at detection 50% of all movements",
       x = "Farmtype",
       y = "Number of infected Farms")

#Show both plots
grid.arrange(p_prop_full, p_prop_reduce_slaughter, p_prop_reduce, ncol = 3)

# Plot the number of days since agent introduction at detection for all movements and 50% of movements
p_days_full = ggplot(Farms_result_full, aes(x=Farmtype, y=discover_days, fill=Farmtype)) + 
  geom_half_violin(side = "r", alpha = 0.7) +
  geom_half_boxplot(side = "l", alpha = 0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Days at detection, all movements",
       x = "Farmtype",
       y = "Days since introduction of pathogen")

# Reduce moves not slaughter
p_days_reduce_slaughter = ggplot(Farms_result_50_slaughter, aes(x=Farmtype, y=discover_days, fill=Farmtype)) + 
  geom_half_violin(side = "r", alpha = 0.7) +
  geom_half_boxplot(side = "l", alpha = 0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Days at detection, 50% of movements normal slaughter",
       x = "Farmtype",
       y = "Days since introduction of pathogen")

# Reduce all moves including slaughter
p_days_reduce = ggplot(Farms_result_50, aes(x=Farmtype, y=discover_days, fill=Farmtype)) + 
  geom_half_violin(side = "r", alpha = 0.7) +
  geom_half_boxplot(side = "l", alpha = 0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Days at detection, 50% of movements",
       x = "Farmtype",
       y = "Days since introduction of pathogen")

# Display both plots
grid.arrange(p_days_full, p_days_reduce_slaughter, p_days_reduce, ncol = 3)

# Display all four plots
grid.arrange(p_prop_full, p_prop_reduce, 
             p_days_full, p_days_reduce, 
             ncol = 2, nrow = 2)

# # Display all four plots with same y-scale
# grid.arrange(p_prop_full, (p_prop_reduce+ ylim(0,165)), 
#              p_days_full, p_days_reduce, 
#              ncol = 2, nrow = 2)


# p2 = ggplot(Farms_result, aes(y=discover_days))+ 
#   geom_half_violin(side = "r", alpha = 0.7) +
#   geom_half_boxplot(side = "l", alpha = 0.7) +
#   labs(title = "Days before disover",
#        x = "",
#        y = "Days since pathogen introduction")
# 
# p_tran = ggplot(propagation, aes(y=spread))+ 
#   geom_half_violin(side = "r", alpha = 0.7) +
#   geom_half_boxplot(side = "l", alpha = 0.7) +
#   labs(title = "Sygdomsudbredelse med beta = 0.05",
#        x = "",
#        y = "Number of infected Farms")





# Arrange the plots side by side
grid.arrange(p11, p22, ncol = 2)
grid.arrange(p1, p2, ncol = 2)

grid.arrange((p1+ ylim(0,130)) , p_tran, ncol = 2)
grid.arrange((p11+ ylim(0,130)), p_trans1 , ncol = 2)





