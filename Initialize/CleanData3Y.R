# load all years of data
raw.data_20 <- readRDS("../Data/movement_farm_2020.RDS")
raw.data_21 <- readRDS("../Data/movement_farm_2021.RDS")
raw.data_22 <- readRDS("../Data/movement_farm_2022.RDS")
raw.data <- rbind(raw.data_20, raw.data_21, raw.data_22)

#Remove herds that have not reported their capacity (capacity of 0 if not slaughter is counted as missing) 
move.data = raw.data %>% filter((SENDER_N_PIGS > 0 & RECEIVER_N_PIGS > 0) | grepl("^15 68", RECEIVER_FARMTYPE)) %>% arrange(DATE_MOVEMENT)


# Remove small farms (capacity < 100)
move.data = move.data %>% filter(SENDER_N_PIGS >= 100) %>%  filter(grepl("^15 68", RECEIVER_FARMTYPE) | RECEIVER_N_PIGS >= 100)



# Keep only 10 largest abattoirs
slaughter_ID_low = move.data %>% filter(RECEIVER_FARMTYPE == "15 68 Svineslagteri") %>% 
  group_by(FARMID_RECEIVER) %>% summarise(Size = sum(N_MOVED_PIGS)) %>% arrange(desc(Size)) %>% slice(11:n())%>% pull(FARMID_RECEIVER)
# Exclude smaller abattoirs from data 
move.data = move.data %>% filter(!(FARMID_RECEIVER %in% slaughter_ID_low))


# Find all remaining farmIDs
All_IDs = unique(c(raw.data$FARMID_SENDER, raw.data$FARMID_RECEIVER))
farmIDs = unique(c(move.data$FARMID_SENDER, move.data$FARMID_RECEIVER))
sendIDs = unique(move.data$FARMID_SENDER)



# Make an easy info of all the different herds
info_all_farms = distinct(rbind(move.data %>% distinct("Farmid" = FARMID_SENDER, "Farm_type" = SENDER_FARMTYPE, "N_pigs" = SENDER_N_PIGS),
                                move.data %>% distinct("Farmid" = FARMID_RECEIVER, "Farm_type" = RECEIVER_FARMTYPE, "N_pigs" = RECEIVER_N_PIGS)))

# Find which years each herd has been active
herds_years = raw.data %>%
  group_by("Farmid" = FARMID_SENDER) %>%
  summarise(moves_2020 = any(year(DATE_MOVEMENT) == 2020, na.rm = TRUE),
            moves_2021 = any(year(DATE_MOVEMENT) == 2021, na.rm = TRUE),
            moves_2022 = any(year(DATE_MOVEMENT) == 2022, na.rm = TRUE))

# Find farmIDs that have not sended anything eg. slaughterhauses
id_receive_only = setdiff(All_IDs, herds_years %>% pull(Farmid))

herds_years_receive = raw.data %>% filter(FARMID_RECEIVER %in% id_receive_only) %>% 
  group_by("Farmid" = FARMID_RECEIVER) %>%
  summarise(moves_2020 = any(year(DATE_MOVEMENT) == 2020, na.rm = TRUE),
            moves_2021 = any(year(DATE_MOVEMENT) == 2021, na.rm = TRUE),
            moves_2022 = any(year(DATE_MOVEMENT) == 2022, na.rm = TRUE))

# Combine the active years
herds_years = rbind(herds_years, herds_years_receive)

# Isert active years in farm info
info_all_farms = left_join(info_all_farms, herds_years, by = "Farmid")
info_all_farms = cbind(info_all_farms, "active_years" = rowSums(info_all_farms %>% select(moves_2020, moves_2021, moves_2022)))


# Grupper dataene efter Farmid og tæl antallet af unikke Farm_type og N_pigs inden for hver gruppe
summary_info <- info_all_farms %>%
  group_by(Farmid) %>%
  summarise(
    unique_Farm_type = n_distinct(Farm_type),
    unique_N_pigs = n_distinct(N_pigs)
  )

# Find antallet af Farmid'er med samme id, men forskellig Farm_type og/eller N_pigs
num_different_type <- sum(summary_info$unique_Farm_type > 1)
num_different_capacity <- sum(summary_info$unique_N_pigs > 1)
num_both_different <- sum(summary_info$unique_Farm_type > 1 & summary_info$unique_N_pigs > 1)


# Find IDs that appear twice
f = info_all_farms %>% group_by(Farmid) %>% count() %>% filter(n > 1) %>% pull(Farmid)
info_dublicates = info_all_farms %>% filter(Farmid %in% f)

dub = info_dublicates %>% group_by(Farmid) %>% count()
dub %>% group_by(n) %>% count()

# How many are active all three years
info_all_farms %>% group_by(active_years) %>% count()

p = info_all_farms %>% filter(is.na(active_years)) %>% pull(Farmid)


