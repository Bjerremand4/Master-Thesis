
# -------------------------------------------------------------------------
#                       DATA CLEANING 
# -------------------------------------------------------------------------

# If raw data exist load and clean it 
if (rawData == T){ # (file.exists(file.path(dirname(getwd()), "Data/Raw")))
  # Path to raw data folder
  rawData.path = file.path(dirname(getwd()), "Data/Raw")
  # Extract all file names
  raw.data.files = list.files(path = rawData.path, full.names = TRUE)
  # Read each file into a list of data frames using read_data function found in (Load_packagesNfunctions)
  data_list <- lapply(raw.data.files, read_data)
  # Combine all data frames into one data frame raw.data
  raw.data <- bind_rows(data_list)
  # Inserting a column with the year instead of whole date
  raw.data <- raw.data %>% mutate(Year = year(DATE_MOVEMENT))
  
  ### Data Cleaning -----------------------------------------------------------
  # All unique IDs in raw data 
  All_IDs = unique(c(raw.data$FARMID_SENDER, raw.data$FARMID_RECEIVER))
  
  #Remove herds that have not reported their capacity (capacity of 0 if not slaughter is counted as missing) 
  move.data = raw.data %>% filter((SENDER_N_PIGS > 0 & RECEIVER_N_PIGS > 0) | grepl("^15 68", RECEIVER_FARMTYPE)) %>% arrange(DATE_MOVEMENT)
  
  # Info on all herds in the raw data 
  info_all = distinct(rbind(raw.data %>% distinct("Farmid" = FARMID_SENDER, "Farm_type" = SENDER_FARMTYPE, "N_pigs" = SENDER_N_PIGS, Year),
                            raw.data %>% distinct("Farmid" = FARMID_RECEIVER, "Farm_type" = RECEIVER_FARMTYPE, "N_pigs" = RECEIVER_N_PIGS, Year))) %>% 
    arrange(Farmid, Year) %>% distinct(Farmid, .keep_all = T)
  
  # # calculations for rapport (removing 0 capacities)
  # # Unique ids included after removing 0 capacities
  # test_ids1 = unique(c(move.data$FARMID_SENDER,move.data$FARMID_RECEIVER))
  # # Type of the removed herds
  # info_all %>% filter(Farmid %in% setdiff(All_IDs, test_ids1)) %>% group_by(Farm_type) %>% count()
  
  
  # Remove small farms (capacity < 100)
  move.data = move.data %>% filter(SENDER_N_PIGS >= 100) %>%  filter(grepl("^15 68", RECEIVER_FARMTYPE) | RECEIVER_N_PIGS >= 100)
  
  
  # # calculations for rapport ( Exclude small herds)
  # test_ids2 = unique(c(move.data$FARMID_SENDER,move.data$FARMID_RECEIVER))
  # # Types
  # info_all %>% filter(Farmid %in% setdiff(test_ids1, test_ids2)) %>% group_by(Farm_type) %>% count()
  
  
  # Keep only 10 largest abattoirs
  slaughter_ID_low = move.data %>% filter(RECEIVER_FARMTYPE == "15 68 Svineslagteri") %>% 
    group_by(FARMID_RECEIVER) %>% summarise(Size = sum(N_MOVED_PIGS)) %>% arrange(desc(Size)) %>% slice(11:n())%>% pull(FARMID_RECEIVER)
  # Exclude smaller abattoirs from data 
  move.data = move.data %>% filter(!(FARMID_RECEIVER %in% slaughter_ID_low)) %>% arrange(DATE_MOVEMENT)
  
  # # calculations for rapport (excluding small slaughterhouses)
  # test_ids3 = unique(c(move.data$FARMID_SENDER,move.data$FARMID_RECEIVER))
  # info_all_farms %>% filter(Farmid %in% setdiff(test_ids2, test_ids3)) %>% group_by(Farm_type) %>% count()
  
  
  # Make an easy info of all the different herds left in the data
  info_all_farms_years = distinct(rbind(move.data %>% distinct("Farmid" = FARMID_SENDER, "Farm_type" = SENDER_FARMTYPE, "N_pigs" = SENDER_N_PIGS, Year),
                                        move.data %>% distinct("Farmid" = FARMID_RECEIVER, "Farm_type" = RECEIVER_FARMTYPE, "N_pigs" = RECEIVER_N_PIGS, Year)))
  
  # Keep only one row for each herd (If they change over the years keep first registered)
  info_all_farms = info_all_farms_years %>% arrange(Farmid, Year) %>% distinct(Farmid, .keep_all = T)
  
  # Find dates on all fridays, saturdays and sundays in 2020 - 2022
  weekend_dates = c(unique(raw.data$DATE_MOVEMENT)[weekdays(unique(raw.data$DATE_MOVEMENT)) == "Friday"],
                    unique(raw.data$DATE_MOVEMENT)[weekdays(unique(raw.data$DATE_MOVEMENT)) == "Saturday"],
                    unique(raw.data$DATE_MOVEMENT)[weekdays(unique(raw.data$DATE_MOVEMENT)) == "Sunday"])
  
  # Calculate the total daily amount of pigs sent for slaughter for the 10 largest slaughterhouses
  daily_slaughter <- move.data %>% filter(RECEIVER_FARMTYPE == "15 68 Svineslagteri") %>% group_by(DATE_MOVEMENT) %>% summarise(daily_slaughter = sum(N_MOVED_PIGS))
  
  ### IDS ####
  # SendIDs for the remaining herds
  sendIDs = unique(move.data$FARMID_SENDER)
  # FarmIDs all herds that er registred in 2020 (Will be used to initialize the model)
  farmIDs = info_all_farms %>% filter(Farmid %in% unique(c(move.data$FARMID_SENDER,move.data$FARMID_RECEIVER))) %>% filter(Year == min(raw.data$Year)) %>% pull(Farmid)
  # Find IDs for the 10 slaughterhouses as they should not start the transmission
  slaught_ID = move.data %>% filter(RECEIVER_FARMTYPE == "15 68 Svineslagteri") %>% distinct(FARMID_RECEIVER) %>% pull()
  # All ids that can be index herds
  herdID = setdiff(farmIDs, slaught_ID)
  
} else {
  # Load IDs from data
  IDs            = read_data(file.path(dirname(getwd()), "Data/General/IDs.rds"))
  All_IDs        = IDs[["All_IDs"]]       # IDs for all herds/slaughterhouses in raw data (before cleaning)
  sendIDs        = IDs[["sendIDs"]]       # IDs for herds that sends any pigs during 2020-2022  
  farmIDs        = IDs[["farmIDs"]]       # IDs for herds registered in 2020
  slaught_ID     = IDs[["slaught_ID"]]    # IDs for the 10 largest slaughterhouses 2020-2022
  herdID         = IDs[["herdID"]]        # IDs for all index herds 2020
  week_IDs       = IDs[["week_IDs"]]      # IDs for index herds sending for slaughter outside weekends
  # Load the herd info data
  info_all_farms = read_data(file.path(dirname(getwd()), "Data/General/info_all_farms.rds"))
  
  # Weekends from raw.data
  weekend_dates  = read_data(file.path(dirname(getwd()), "Data/General/weekend_dates.rds"))
  
  # Total daily amount of pigs sent for slaughter for the 10 largest slaughterhouses
  daily_slaughter  = read_data(file.path(dirname(getwd()), "Data/General/daily_slaughter.rds"))
}


# IDs = list(All_IDs = All_IDs,
#            sendIDs = sendIDs,
#            farmIDs = farmIDs,
#            slaught_ID = slaught_ID,
#            herdID = herdID,
#            week_IDs = week_IDs)
# saveRDS(IDs, file.path("../Data/General", "IDs.rds"))



