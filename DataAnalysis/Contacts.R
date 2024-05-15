# -------------------------------------------------------------------------
#               Track contacts initiated from each farm
# -------------------------------------------------------------------------
# Initialize an empty list to store interactions for each farm
contact_farms_outputlist = list()
unique_contact = tibble(start_farm = double(), N_contacts = integer(), N_unique_contacts = integer())

# timing loop
start = Sys.time()

# Start a loop that runs through all farms in sendIDs (only farms that sends to others)
for (id in 1:length(sendIDs)){
  # update farmID
  farm = sendIDs[id]
  
  start_date = move.data$DATE_MOVEMENT[1]
  
  # Subset/reset data to only contain days within the set period
  period.data <- move.data
  
  # Initializing index
  index <- 1
  
  # Allocate space to collect all farms that have somehow been in contact with the chosen farm 
  contact_farms <- tibble(FarmID = farm, date = start_date, contactFarm = NA)
  
  while (index != 0){
    # Check if there's only one row left in period.data
    if (nrow(period.data) == 1) {
      break  # Break the while loop if there's only one row left
    }
    
    # Find next move from a relevant farm
    index <- ifelse(is.na(which(period.data$FARMID_SENDER %in% contact_farms$FarmID)[1]), 0, 
                    which(period.data$FARMID_SENDER %in% contact_farms$FarmID)[1])
    
    # Check if index is 0 before adding to contact_farms
    if (index != 0) {
      # Update contact tibble
      contact_farms <- contact_farms %>% add_row(FarmID = period.data$FARMID_RECEIVER[index],
                                                 date = period.data$DATE_MOVEMENT[index],
                                                 contactFarm = period.data$FARMID_SENDER[index])
      
      # Update data, such that all prior indexes are excluded 
      period.data <- period.data %>% slice((index+1):n())
      
      
    }
  }
  unique_contact = unique_contact %>% add_row(start_farm = farm,
                                              N_contacts = length(contact_farms$FarmID), 
                                              N_unique_contacts = length(unique(contact_farms$FarmID)))
  contact_farms_outputlist[[id]] = contact_farms
  print(id)
}

slut = Sys.time()

# Tid for loopet har kørt
slut - start
# Create names for each ID
Herd_names = as.character(sendIDs)

#### Create the empty output list  -------------------------------------------
# Create a list with empty lists
contact_farms_list <- vector("list", length(Herd_names))
# Assign names to the empty lists
names(contact_farms_list) <- Herd_names


move_data = move.data %>% filter(!(FARMID_RECEIVER %in% slaught_ID))

for (id in 1:length(sendIDs)){
  # Insert the initial farm
  contact_farms = tibble(farmID = sendIDs[id],
                         date = as.Date("2020-01-01"),
                         contactFarm = sendIDs[id])
  # Initialize
  flag = F
  date_lim = contact_farms$date
  
  while (flag == F){
    # Extract data 
    contact.data = move_data %>% filter(DATE_MOVEMENT >= date_lim) %>% 
      filter(FARMID_SENDER %in% contact_farms$contactFarm & !(FARMID_RECEIVER %in% contact_farms$contactFarm)) %>% slice(1)
    
    # If the are no more unique contacts set flag to positive
    if (nrow(contact.data) < 1){
      flag = T
    } else {
      date_lim = contact.data$DATE_MOVEMENT
      contact_farms = contact_farms %>% add_row(farmID = contact.data$FARMID_SENDER,
                                                date = contact.data$DATE_MOVEMENT,
                                                contactFarm = contact.data$FARMID_RECEIVER)
    }
    
  }
  
  contact_farms_list[[Herd_names[id]]] = contact_farms
  print(id) 
}


