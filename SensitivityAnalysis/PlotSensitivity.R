# -------------------------------------------------------------------------
#                       VARIATION PLOTS 
# -------------------------------------------------------------------------

# Load packages -----------------------------------------------------------
source(file.path("../Initialize/Load_packages.R"))


# Load and clean data  -----------------------------------------------------
source(file.path("../Initialize/LoadNClean_Data.R"))

seed.data <- readRDS("../Data/Sensitivity/Results_10Seeds_data22_part1.rds")
seeds<- readRDS("../Data/Sensitivity/seeds.rds")
spread10 = readRDS("../Data/Sensitivity/spread10_22.rds")

# Allocate space to store propagation for each seed
 spread10 = tibble(seed = numeric(),
                   init_farm = numeric(),
                 farm_type = character(),
                   spread = numeric())
 
 
 for (seed in seeds){
   # Extract data 
   data = seed.data[[as.character(seed)]]
   
   for (list in 1:5053){
     # Extract data 
     trans = data[[list]][[2]]
     # Extract initial infected farm
     init_f = trans$farmID[1] 
     # Count spread
     spread10 = spread10 %>% add_row(seed = seed,
                                     init_farm = init_f,
                                     farm_type = info_all_farms %>% filter(Farmid == init_f) %>% pull(Farm_type),
                                     spread = nrow(trans))
     # propagation = propagation %>% add_row(init_farm = FarmIDs[list],
     #                                       farm_type = info_all_farms %>% filter(Farmid == FarmIDs[list]) %>% pull(Farm_type),
     #                                       spread = nrow(data))
   }
 }  

 # Save
 saveRDS(spread10, file.path("../Data/Sensitivity", "spread10_22.rds") )


# Boxplot of spread in the 10 seads
ggplot(spread10, aes(x=as.character(seed), y=spread, fill=as.character(seed))) + 
  geom_half_violin(side = "r", alpha = 0.7) +
  geom_half_boxplot(side = "l", alpha = 0.7) +
  theme(legend.position="none")  +
  labs(title = "unique infected farms during 2022",
       x = "seed",
       y = "Number of infected Farms")

# Spread to at least 1 other farm
spread10 %>% filter(spread > 1) %>% 
ggplot(aes(x=as.character(seed), y=spread, fill=as.character(seed))) + 
  geom_half_violin(side = "r", alpha = 0.7) +
  geom_half_boxplot(side = "l", alpha = 0.7) +
  theme(legend.position="none")  +
  labs(title = "unique infected farms during 2022",
       x = "seed",
       y = "Number of infected Farms")



# ggplot(spread10, aes(x = as.character(init_farm), y = spread, col =as.character(seed))) + 
#   geom_line(aes(group = seed), alpha = 0.7)


# Info of the variation across seeds
spread10 %>% group_by(seed) %>% summarise(max = max(spread),
                                          min = min(spread),
                                          mean = mean(spread),
                                          median = median(spread),
                                          variance = var(spread),
                                          sd = sd(spread))  

# find max, min and diff for each farm across the 10 seeds
spread.data = spread10 %>% 
  group_by(init_farm) %>% 
  mutate(max_spread = max(spread),
         min_spread = min(spread),
         dif_spread = max(spread) - min(spread)) %>% 
  distinct(init_farm, .keep_all = T) %>% 
  arrange(dif_spread)
spread.data$idx = c(1:5053)

# Exclude all the zeroes, where the result is the same across all 10 seeds
spread.zero.data = spread.data %>% filter(dif_spread > 0) %>% arrange(dif_spread) %>% ungroup()


# Plot difference in spread across seeds as line plot
ggplot(spread.zero.data, aes(group = seed, x = as.factor(idx))) +
  geom_line(aes(y = dif_spread, col = "Max difference")) + 
  geom_line(aes(y = spread.zero.data$max_spread, col = "Max number of farms infected")) +
  geom_line(aes(y = spread.zero.data$min_spread, col = "Min number of farms infected"),alpha = 0.8)



# calculate number and percent of differences between the 10 seeds
pz = spread.data %>% group_by(dif_spread) %>%  summarise(count = n(), percent = n()/nrow(spread.data)*100)
pnz = spread.zero.data %>% group_by(dif_spread) %>%  summarise(count = n(), percent = n()/nrow(spread.data)*100) 

# Plot with and without zeroes
plot_zero = ggplot(pz, aes(x = dif_spread, y = percent)) + 
  geom_bar(stat = "identity") +
  labs(title = "Including zeroes",
       x = "Max difference in spread across seeds",
       y = "% of Farms")

plot_no_zero = ggplot(pnz, aes(x = dif_spread, y = percent)) + 
  geom_bar(stat = "identity") +
  labs(title = "Excluding zeroes",
       x = "Max difference in spread across seeds ",
       y = "% of Farms")

# Display both plots
grid.arrange(plot_zero, plot_no_zero, ncol = 2)




# Correlation  ------------------------------------------------------------
# Create correlation matrix
correlation_matrix = matrix(nrow = 10, ncol = 10)
for (row in 1:10){
  for (col in 1:10){
    correlation_matrix[row,col] = cor((spread10 %>% filter(seed == seeds[row]) %>% pull(spread)),(spread10 %>% filter(seed == seeds[col]) %>% pull(spread)))
  }
}



# Anova -------------------------------------------------------------------
# Fit ANOVA model
model <- aov(spread ~ seed, data = spread10)

# Summarize ANOVA results
summary(model)






# Find only farmIds that are not the same result across all 10 seeds --------
# id_var = c()
# 
# for (id in 1:length(farmIDs)){
#   s = spread10 %>% filter(init_farm == farmIDs[id])
#   if (length(unique(s$spread)) > 1){
#     Id = farmIDs[id]
#     id_var = c(id_var, Id)
#   }
# }
# 
# # Extract data on those farmIDs
# spread_var = spread10 %>% filter(init_farm %in% id_var)
# spread_var %>%  group_by(init_farm) %>% mutate()
# spread_var_unique = spread_var %>% distinct(init_farm, spread, .keep_all = T) %>% arrange(spread) %>%  group_by(init_farm) %>% mutate(idx = row_number(), idx_ID = NA) 
# ids = unique(spread_var_unique$init_farm)
# for (i in 1:length(ids)){
#   spread_var_unique$idx_ID[which(spread_var_unique$init_farm == ids[i])] = i
# }
# 
# 
# ggplot(spread_var_unique, aes(x = as.character(idx_ID), y = spread, col = as.character(idx))) + 
#   geom_point() + 
#   theme(legend.position="none") +
#   labs(title = "",
#        x = "",
#        y = "Spread")

