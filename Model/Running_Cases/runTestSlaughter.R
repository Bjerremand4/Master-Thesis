# ----------------- Load packages, data and functions -----------------------
# Load and manage data  
source(file.path("Initialize/LoadNClean_Data.R"))
# Load packages and functions
source(file.path("Initialize/Load_packagesNfunctions.R"))

frequencies = c(100, 50, 33)
betas = c(0.05, 0.2)
samples = c(5,50)
sens = c(0.85, 0.7)
nr = 0

for (move_freq in frequencies){
  for (b in betas){
    for (n_test in samples){
      for (test_sensitivity in sens){
        nr = nr+1
        
        # Get transmission data for the right parameters 
        transmission.data = readRDS(file.path("../Data/1_Transmission", paste0("trans_move",move_freq,"%_beta", b, ".rds")))
        
        # Allocate space to store result for each Farm
        Farms_result = tibble(FarmID = numeric(),
                              Farmtype = character(),
                              Detection_date = Date(),
                              discover_days = numeric(),
                              Propagation = numeric())
        
        for (farm in herdID){
          idx = which(farm == herdID)
          # set FarmID and type
          init_farm = farm
          farm_type = info_all_farms %>% filter(Farmid == init_farm) %>% pull(Farm_type)
          
          # Extract data corresponding to current FarmID and remove weekend dates so samples wont be taken here
          inf_slaughter = transmission.data[[as.character(init_farm)]][[1]] %>% filter(!(inf_date %in% weekend_dates))
          
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
              farm_inf = transmission.data[[as.character(init_farm)]][[2]]#[complete.cases(transmision.data[[as.character(init_farm)]][[2]]), ]
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
              test_P = ifelse(1-((1-test_prev)^n_test) < 1, 1-((1-test_prev)^n_test), 0.9)    # Implemented as sometimes more infected pigs are sent than total sent pigs the given day? ?
              
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
                                                    discover_days = as.numeric(difftime(discover_date, as.Date("2020-02-01"))),
                                                    Propagation = propagation)
          } else {
            Farms_result = Farms_result %>% add_row(FarmID = init_farm ,
                                                    Farmtype = farm_type,
                                                    Detection_date = as.Date("2023-01-01"),
                                                    discover_days = as.numeric(difftime(as.Date("2023-01-01"), as.Date("2020-02-01"))),
                                                    Propagation = nrow(transmission.data[[as.character(init_farm)]][[2]]))
            
          }
          print(paste0("Transmission, iteration: ", idx, "/", length(herdID), " - sim nr ", nr))  
        }

        # Save the data
        saveRDS(Farms_result, file.path("../Data/2a_TestSlaughter", paste0("TestSlaughter",move_freq, "%_beta", b,"_sample", n_test, "_sens", test_sensitivity, ".rds"))) 
      }
    }
  }
}
slut = Sys.time()


