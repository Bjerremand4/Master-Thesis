# -------------------------------------------------------------------------
#                       TRANSMISSION MODEL 
# -------------------------------------------------------------------------

# Load packages -----------------------------------------------------------
source(file.path("Initialize/Load_packagesNfunctions.R"))


# Load and clean data  -----------------------------------------------------
source(file.path("Initialize/LoadNClean_Data.R"))


# Make big output list to store output for each seed
# Generate 10 seeds
seeds = sample(1000:9999, 10, replace = TRUE)
saveRDS(seeds, file.path("../Data/Sensitivity", "seeds10_full.rds"))

# Geenerate and assigm names to output list
names_seed = as.character(seeds)
seed_output_list = vector("list", length(names_seed))
names(seed_output_list) <- names_seed


# Create names for each index Herd (herds registered in 2020 and not slaughters)
names = as.character(herdID)

# track time
Start = Sys.time()

for (seed in seeds){
  #### Create the empty output list  -------------------------------------------
  # Create a list with empty lists
  Output_list <- vector("list", length(names))
  # Assign names to the empty lists
  names(Output_list) <- names
  
  seed_nr = which(seeds == seed)
  
  # BIG LOOP ----------------------------------------------------------------
  for (herd in 1:length(herdID)){
    set.seed(seed)
    # Initialize model  -----------------------------------------------------
    # Choose the farm to introduce the pathogen 
    init_farm = herdID[herd] 
    
    # Initialize time 
    start.date <- as.Date("2020-02-01")    # Skipping january as there could be some lag time       
    current.event.date <- start.date 
    
    # allocate space to store info on infected farms
    farms_infected = data.frame(farmID = init_farm,
                                Capacity = info_all_farms$N_pigs[which(info_all_farms$Farmid == init_farm)],
                                date_of_infection = start.date,
                                farmID_spreader = NA,
                                init_inf = 1,
                                I = 1,
                                prev = 1 / info_all_farms$N_pigs[which(info_all_farms$Farmid == init_farm)])
    
    # Allocate space to keep track on when and how many infected pigs are sent to slaughter
    slaughter_infected = tibble(inf_date = as.Date(character()),
                                n_infected = numeric())
    
    # Find first event, only looking at movement from infected farms
    next.event.date = move.data %>% 
      filter(FARMID_SENDER %in% farms_infected$farmID) %>%
      filter(DATE_MOVEMENT >= current.event.date) %>% 
      pull(DATE_MOVEMENT) %>% first()
    
    # Days before next event, used for input in logistic model
    days <- as.numeric(difftime(next.event.date,      
                                current.event.date,
                                units = "days"))
    
    # Extract moves from infected farm only
    inf.moves = move.data %>% 
      filter(FARMID_SENDER %in% farms_infected$farmID & !(FARMID_RECEIVER %in% farms_infected$farmID))
    
    
    # Begin the transmission detection
    while (is.na(next.event.date) != T){
      
      # # Run SIR(logistic) for infected farm till next event
      result <- Logistic_model(N = farms_infected$Capacity, I0 = farms_infected$I, t = days, beta = b)
      # Update farms_infected dataframe with results
      farms_infected[, 6:7] <- result
      
      
      # Update current date
      current.event.date = next.event.date 
      
      # Find moves from infected farms at the current date 
      moves = inf.moves %>% filter(DATE_MOVEMENT == current.event.date)
      
      
      # Check for each move if there is spread to another farm or how many infected pigs are sent to slaughter
      for (move in 1:nrow(moves)){
        # Extract information at the given move
        farmID_sender = moves$FARMID_SENDER[move]
        farmID_reciver = moves$FARMID_RECEIVER[move]
        receiver_type = moves$RECEIVER_FARMTYPE[move]
        n_moved = moves$N_MOVED_PIGS[move]
        
        #Calculate number of infected within the moved pigs
        prevalence <- farms_infected %>% filter(farmID == farmID_sender) %>% pull(prev)
        n_spread = rbinom(n = 1, size = n_moved, prob = ifelse(abs(prevalence - 1) < 1e-10, 0.99999999999, prevalence))
        
        if (n_spread > 0){
          if (receiver_type == "15 68 Svineslagteri"){
            
            slaughter_infected = slaughter_infected %>% add_row(inf_date = current.event.date, n_infected = n_spread)
          } 
          
          if (receiver_type != "15 68 Svineslagteri") {
            Max_kap = info_all_farms %>% filter(Farmid == farmID_reciver) %>% pull(N_pigs)
            # Update infected farms
            farms_infected = farms_infected %>% add_row(farmID = farmID_reciver, 
                                                        Capacity = Max_kap,
                                                        date_of_infection = current.event.date,
                                                        farmID_spreader = farmID_sender,
                                                        init_inf = ifelse(n_spread < Max_kap, n_spread, Max_kap),
                                                        I = ifelse(n_spread < Max_kap, n_spread, Max_kap),
                                                        prev = (ifelse(n_spread < Max_kap, n_spread, Max_kap))/
                                                          info_all_farms %>% filter(Farmid == farmID_reciver) %>% pull(N_pigs))
            
            # Update infection moves to include moves from the newly infected Herd. 
            inf.moves = move.data %>% 
              filter(FARMID_SENDER %in% farms_infected$farmID & !(FARMID_RECEIVER %in% farms_infected$farmID))
          }
        }
      }
      
      # Find and update next event
      next.event.date = next.event.date = inf.moves %>% 
        filter(DATE_MOVEMENT > current.event.date) %>%
        pull(DATE_MOVEMENT) %>% first()
      
      # Update days till next event
      days <- as.numeric(difftime(next.event.date,      
                                  current.event.date,
                                  units = "days"))
    }
    
    
    # Collects the rows of infected pigs for slaughter if they are on the same date
    slaughter_infected = slaughter_infected %>% group_by(inf_date) %>% summarise(n_infected = sum(n_infected))
    
    # Save the information for the given farm and move on to the next 
    Output_list[[as.character(init_farm)]] = list(slaughter_infected, farms_infected)
    print(paste0("Seed ", seed_nr, " iteration ", herd, "/", length(herdID)))
  }

  seed_output_list[[as.character(seed)]] = Output_list
  
}


end = Sys.time()
saveRDS(seed_output_list, file.path("../Data/Sensitivity", "transmission_10seeds_full3years.rds") )

