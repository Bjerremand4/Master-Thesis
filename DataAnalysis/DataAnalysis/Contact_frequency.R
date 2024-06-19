# Direct Contact frequency over time 
# Initialize -----------------------------------------------------------
source(file.path("../Model/Initialize/Load_packages.R"))
source(file.path("../Model/Initialize/Define_Functions.R"))
rawData = FALSE # Change if you have access to row data
source(file.path("../Model/Initialize/LoadNClean_Data.R")) 

# load data ---------------------------------------------------------------
# Full data
movement.data <- move.data 

# removing all slaughter entries 
move.data <- subset(movement.data, RECEIVER_FARMTYPE != "15 68 Svineslagteri") %>% 
  filter(SENDER_N_PIGS > 100) %>%  filter(RECEIVER_N_PIGS > 100)


# Monthly frequency 
move.month = move.data %>% mutate(Month = format(DATE_MOVEMENT, "%b-%y"))
move.month = move.month %>% select(DATE_MOVEMENT, Month, FARMID_SENDER, FARMID_RECEIVER)


# Allocate space to store total number of contact pr month pr farm
Tot_unique_contacts = tibble(farmId = numeric(),
                             Month = character(),
                             Unique_contacts = numeric())

  
for (nr in 1:(length(herdID))){
  # Extract only row where the given farm is included
  mm = move.month %>% filter(FARMID_SENDER == herdID[nr] | FARMID_RECEIVER ==  herdID[nr]) 
  if (nrow(mm) > 0){
    # Create tibble 
    frek = tibble("farmId" = farmIDs[nr], 
                  "Month" = mm$Month, 
                  "farmP" = ifelse(mm$FARMID_SENDER == farmIDs[nr], mm$FARMID_RECEIVER, mm$FARMID_SENDER ))
    
    # Remove doblicate row aka non unique moves 
    frek = frek %>% distinct(Month,farmP, .keep_all = T) %>% mutate(Unique_contacts = 1)
    
    # Count moves pr month
    a = aggregate(Unique_contacts ~  farmId + Month, data = frek, sum)
    
    # Add it to the Tot_unique_contacts data frame
    Tot_unique_contacts = rbind(Tot_unique_contacts, a)
  }
  print(nr)
}

# Save the data
saveRDS(Tot_unique_contacts, file.path("../Data/DataAnalysis", "Tot_unique_contacts.rds"))

# Plot the unique contacts pr month pr farm 
tt = Tot_unique_contacts
tt$farmId = as.character(tt$farmId)
tt = tt %>% mutate(Date = as.Date(paste0("01-", Month), format = "%d-%b-%y")) %>% arrange(Date)


ggplot(data = tt, aes(x = Date, y = Unique_contacts, group = farmId, color = farmId)) + 
  geom_line()  +
  scale_color_viridis(option = "H",discrete = T) +
  theme(legend.position = "none") +
  labs(title = "Monthly unique contacts with other farm",
       x = "Time",
       y = "Number of unique contacts")

# Find farmId on the farms that in some months has over 12 unique contacts
tt_id = tt %>% filter(Unique_contacts > 25) %>% select(farmId) %>% unique()
tt[(tt$farmId %in% tt_id$farmId),] %>% 
ggplot( aes(x = Date, y = Unique_contacts, group = farmId)) + 
  geom_line(aes(color = farmId)) +
  labs(title = "Monthly unique contacts -  including only farms with over 12 unique contact in any given month",
       x = "Month",
       y = "Number of unique contacts") + 
  scale_color_viridis(option = "H",discrete = T) 
  scale_y_continuous(breaks = round(seq(0, max(tt$Unique_contacts)+10, by = 10),1))  
  
