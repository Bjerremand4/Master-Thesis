# -------------------------------------------------------------------------
#                       DATA CLEANING 
# -------------------------------------------------------------------------

# Load packages 
#source(file.path("Load_packages.R"))

#load all years of data
raw.data_20 <- readRDS("../Data/Raw/movement_farm_2020.RDS")
raw.data_21 <- readRDS("../Data/Raw/movement_farm_2021.RDS")
raw.data_22 <- readRDS("../Data/Raw/movement_farm_2022.RDS")
raw.data <- rbind(raw.data_20, raw.data_21, raw.data_22)
raw.data <- raw.data %>% mutate(Year = year(DATE_MOVEMENT))

# Load full dataset  
#raw.data <- readRDS("../Data/Raw/movement_farm_2022.RDS")

# Make an easy info of all the different herds
info_all_farms_years = distinct(rbind(raw.data %>% distinct("Farmid" = FARMID_SENDER, "Farm_type" = SENDER_FARMTYPE, "N_pigs" = SENDER_N_PIGS, Year),
                                      raw.data %>% distinct("Farmid" = FARMID_RECEIVER, "Farm_type" = RECEIVER_FARMTYPE, "N_pigs" = RECEIVER_N_PIGS, Year)))

# behold kun en række for hver farm
# info_all_farms = na.omit(info_all_farms) %>% arrange(Farmid) %>% distinct(Farmid, .keep_all = T)
info_all_farms = info_all_farms_years %>% arrange(Farmid, Year) %>% distinct(Farmid, .keep_all = T)

# All unique IDs 
All_IDs = unique(c(raw.data$FARMID_SENDER, raw.data$FARMID_RECEIVER))

#Remove herds that have not reported their capacity (capacity of 0 if not slaughter is counted as missing) 
#move.data = raw.data %>% filter((SENDER_N_PIGS > 0 & RECEIVER_N_PIGS > 0) | grepl("^15 68", RECEIVER_FARMTYPE)) %>% arrange(DATE_MOVEMENT)
zero_ids = info_all_farms %>% filter(Farm_type != "15 68 Svineslagteri") %>% filter(N_pigs < 1 | is.na(N_pigs)) %>% pull(Farmid)
move.data = raw.data %>% filter(!(FARMID_SENDER %in% zero_ids) & !(FARMID_RECEIVER %in% zero_ids))

# # calculations for rapport
# #unique ids included
# test_ids1 = unique(c(move.data$FARMID_SENDER,move.data$FARMID_RECEIVER))
# # Types
# info_all_farms %>% filter(Farmid %in% setdiff(All_IDs, test_ids1)) %>% group_by(Farm_type) %>% count()



# Remove small farms (capacity < 100)
move.data = move.data %>% filter(SENDER_N_PIGS >= 100) %>%  filter(grepl("^15 68", RECEIVER_FARMTYPE) | RECEIVER_N_PIGS >= 100)
#small_ids = info_all_farms %>% filter(Farm_type != "15 68 Svineslagteri") %>% filter(N_pigs >= 1 & N_pigs <= 100) %>% pull(Farmid)
#move.data = move.data %>% filter(!(FARMID_SENDER %in% small_ids) & !(FARMID_RECEIVER %in% small_ids))

# # calculations for rapport
# test_ids2 = unique(c(move.data$FARMID_SENDER,move.data$FARMID_RECEIVER))
# # Types
# info_all_farms %>% filter(Farmid %in% setdiff(test_ids1, test_ids2)) %>% group_by(Farm_type) %>% count()
# #info_all_farms %>% filter (Farmid %in% small_ids) %>% group_by(Farm_type) %>% count()


# Keep only 10 largest abattoirs
slaughter_ID_low = move.data %>% filter(RECEIVER_FARMTYPE == "15 68 Svineslagteri") %>% 
  group_by(FARMID_RECEIVER) %>% summarise(Size = sum(N_MOVED_PIGS)) %>% arrange(desc(Size)) %>% slice(11:n())%>% pull(FARMID_RECEIVER)
# Exclude smaller abattoirs from data 
move.data = move.data %>% filter(!(FARMID_RECEIVER %in% slaughter_ID_low)) %>% arrange(DATE_MOVEMENT)

# # calculations for rapport
# test_ids3 = unique(c(move.data$FARMID_SENDER,move.data$FARMID_RECEIVER))
# info_all_farms %>% filter(Farmid %in% setdiff(test_ids2, test_ids3)) %>% group_by(Farm_type) %>% count()


# Find all remaining farmIDs 
farmIDs = info_all_farms %>% filter(Farmid %in% unique(c(move.data$FARMID_SENDER,move.data$FARMID_RECEIVER))) %>% filter(Year == 2020) %>% pull(Farmid)
sendIDs = unique(move.data$FARMID_SENDER)


# behold kun en række for hver farm
# info_all_farms = na.omit(info_all_farms) %>% arrange(Farmid) %>% distinct(Farmid, .keep_all = T)
info_all_farms = info_all_farms %>% arrange(Farmid) %>% distinct(Farmid, .keep_all = T)

# Find IDs for the 10 slaughterhouses as they should not start the transmission
slaught_ID = move.data %>% filter(RECEIVER_FARMTYPE == "15 68 Svineslagteri") %>% distinct(FARMID_RECEIVER) %>% pull()
herdID = setdiff(farmIDs, slaught_ID)


# Find IDs that appear twice
# f = info_all_farms_years %>% group_by(Farmid) %>% count() %>% filter(n > 1) %>% pull(Farmid)
# info_dublicates = info_all_farms_years %>% filter(Farmid %in% f) %>% arrange(Farmid)


# Define the two betas used in the model:
#bacterial beta
bac_beta = 0.05
#virus beta
vir_beta = 0.2






# Calculatins for HArd appearance figure ----------------------------------

