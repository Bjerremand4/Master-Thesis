# -------------------------------------------------------------------------
#               Track contacts initiated from each farm
# -------------------------------------------------------------------------
# Initialize -----------------------------------------------------------
source(file.path("../Model/Initialize/Load_packages.R"))
source(file.path("../Model/Initialize/Define_Functions.R"))
rawData = FALSE # Change if you have access to row data
source(file.path("../Model/Initialize/LoadNClean_Data.R"))

# Create names for each ID
names = as.character(sendIDs)
# Create a list with empty lists
contact_farms_outputlist <- vector("list", length(names))
# Assign names to the empty lists
names(contact_farms_outputlist) <- names

# Excluding movementns to slaughter
move.data = move.data %>% filter(DATE_MOVEMENT >= start.date & RECEIVER_FARMTYPE != "15 68 Svineslagteri")

start_contact = Sys.time() # Time Tracker

# BIG LOOP ----------------------------------------------------------------
for (herd in 1:length(sendIDs)){
  #set.seed(1234)
  # Initialize model  -----------------------------------------------------
  # Choose the farm to introduce the pathogen 
  init_farm = sendIDs[herd] 
  
  # Initialize time 
  start.date <- as.Date("2020-02-01")    # Skipping january as there could be some lag time  
  current.date <- start.date
  
  # allocate space to store info on infected farms
  contact_farms = tibble(farmID = init_farm,
                         date = start.date,
                         contactFarm = NA)

  # Find data for contact farms
  new_contact = move.data %>% 
    filter(FARMID_SENDER %in% contact_farms$farmID & !(FARMID_RECEIVER %in% contact_farms$farmID) & DATE_MOVEMENT >= current.date) 
  # The date of the first new contact
  contact.date = new_contact$DATE_MOVEMENT[1]
  
  # Begin the transmission detection
  while (nrow(new_contact) != 0){
    # update contacts
    contact_farms = contact_farms %>% add_row(farmID = new_contact$FARMID_RECEIVER[which(new_contact$DATE_MOVEMENT == contact.date)],
                                              date = new_contact$DATE_MOVEMENT[which(new_contact$DATE_MOVEMENT == contact.date)],
                                              contactFarm = new_contact$FARMID_SENDER[which(new_contact$DATE_MOVEMENT == contact.date)])
    
    # update current date
    current.date = contact.date
    
    # find new contacts
    new_contact = move.data %>% 
      filter(FARMID_SENDER %in% contact_farms$farmID & !(FARMID_RECEIVER %in% contact_farms$farmID) & DATE_MOVEMENT > current.date) 
    # The date of next new contact
    contact.date = new_contact$DATE_MOVEMENT[1]
  }
  
  # Save the information for the given farm and move on to the next 
  contact_farms_outputlist[[as.character(init_farm)]] = contact_farms
  print(paste0("contact, iteration: ", herd, "/", length(sendIDs) ))
}

end_contact = Sys.time()


# Save
saveRDS(contact_farms_outputlist, file.path("../Data/DataAnalysis/Full_contact_farms.rds"))

