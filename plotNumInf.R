# -------------------------------------------------------------------------
#                            Plots - transmission
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------
source(file.path("../Initialize/Load_packages.R"))


# Load and clean data  -----------------------------------------------------
source(file.path("../Initialize/LoadNClean_Data.R"))


# Contacts ----------------------------------------------------------------
# Import data on contact patterns
#contact.data <- readRDS("../Data/dataAnalysis/contact_farms.rds")

contacts = tibble(init_farm = numeric(),
                  farm_type = character(),
                  contacts = numeric())

# unique contacts
for (list in 1:length(contact.data)){
  # Extract data 
  data = contact.data[[list]]
  # Extract initial infected farm
  init_f = data$FarmID[1] 
  # Count spread
  contacts = contacts %>% add_row(init_farm = init_f,
                                  farm_type = info_all_farms %>% filter(Farmid == init_f) %>% pull(Farm_type),
                                  contacts = length(unique(data$FarmID)))
}





# Transmission -------------------------------------------------------------
# Import data from Transmission model
#transmision.data <- readRDS("../Data/full_trans_beta005.rds")[[1]]
#transmision1.data <- readRDS("../Data/trans_beta_bacteria.rds")
#transmision2.data <- readRDS("../Data/trans_beta_bacteria2.rds")
transmision.data <- readRDS("../Data/trans_bac_reduceMove.rds")

# Initialize 2 tibbles to store transmisssion and contact 
propagation = tibble(init_farm = numeric(),
                     farm_type = character(),
                     spread = numeric(),
                     proportion = numeric())


# Transmission full 
for (list in 1:length(transmision.data)){
  # Extract data 
  data = transmision.data[[list]][[2]]
  # Extract initial infected farm
  init_f = data$farmID[1] 
  # Count spread
  propagation = propagation %>% add_row(init_farm = init_f,
                                        farm_type = info_all_farms %>% filter(Farmid == init_f) %>% pull(Farm_type),
                                        spread = nrow(data),
                                        proportion = nrow(data)/length(transmision.data))
  # propagation = propagation %>% add_row(init_farm = FarmIDs[list],
  #                                       farm_type = info_all_farms %>% filter(Farmid == FarmIDs[list]) %>% pull(Farm_type),
  #                                       spread = nrow(data))
}


# Plot proportions smittede med full herds 
propagation = propagation %>% arrange(proportion) %>% mutate(index = row_number())
propagation = propagation %>% arrange(spread) %>% mutate(index_spread = row_number())

p2_full = ggplot(propagation, aes(x = index, y = proportion))+ 
  geom_line()

# Same plot at spread 
p2_full_spread = ggplot(propagation, aes(x = index_spread, y = spread)) + 
  geom_line()

### Density plots
p_full = ggplot(propagation, aes(x=proportion)) + 
  geom_density() 


# Plots trans + contacts (propagation without detection) --------------------------------------------------
p_trans2 = ggplot(propagation, aes(x=farm_type, y=spread, fill=farm_type)) + 
  geom_boxplot() +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Sygdomsudbredelse med beta = 0.05",
       x = "Initial infected farm",
       y = "Number of infected Farms")

P_trans = ggplot(Propagation, aes(x=farm_type, y=spread, fill=farm_type)) + 
  geom_boxplot() +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Sygdomsudbredelse med beta = 0.05",
       x = "Initial infected farm",
       y = "Number of infected Farms")


p_contact = ggplot(contacts, aes(x=farm_type, y=contacts, fill=farm_type)) +
  geom_boxplot() +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Sygdomsudbredelse med beta = 1",
       x = "Initial infected farm",
       y = "Number of infected Farms")

grid.arrange(P_trans,p_trans1, p_trans2, nrow = 3)




# Plots Beta -------------------------------------------------------------------
p005 = ggplot(propagation, aes(x=farm_type, y=spread, fill=farm_type)) + 
  geom_half_violin(side = "r", alpha = 0.7) +
  geom_half_boxplot(side = "l", alpha = 0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Sygdomsudbredelse med beta = 0.05",
       x = "Initial infected farm",
       y = "Number of infected Farms")
  
p007 = ggplot(propagation_beta0.7, aes(x=farm_type, y=spread, fill=farm_type)) + 
  geom_half_violin(side = "r", alpha = 0.7) +
  geom_half_boxplot(side = "l", alpha = 0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Sygdomsudbredelse med beta = 0.07",
       x = "Initial infected farm",
       y = "Number of infected Farms")

p01 = ggplot(propagation_beta0.1, aes(x=farm_type, y=spread, fill=farm_type)) + 
  geom_half_violin(side = "r", alpha = 0.7) +
  geom_half_boxplot(side = "l", alpha = 0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Sygdomsudbredelse med beta = 0.1",
       x = "Initial infected farm",
       y = "Number of infected Farms")
  
# Display plots
p005 + p007 + p01
(p05 + ylim(0,75)) + (p07+ ylim(0,75)) + (p1+ ylim(-1,50))




transmision.data1 <- readRDS("../Data/trans_beta_bacteria.rds")
transmision.data2 <- readRDS("../Data/full_trans_beta005.rds")[[1]]

Farm1 = transmision.data1[["2336659471"]][[2]]
Farm2 = transmision.data2[["2336659471"]][[2]]

Farm1 %>% mutate(year_date = year(date_of_infection)) %>% group_by(year_date) %>% count()
Farm2 %>% mutate(year_date = year(date_of_infection)) %>% group_by(year_date) %>% count()
