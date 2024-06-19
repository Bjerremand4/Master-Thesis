# -------------------------------------------------------------------------
#                             VARIATION PLOTS 
# -------------------------------------------------------------------------

# Load packages -----------------------------------------------------------
source(file.path("../Initialize/Load_packages.R"))


# Load and clean data  -----------------------------------------------------
source(file.path("../Initialize/LoadNClean_Data.R"))

# -------------------------------------------------------------------------
#                                  SEEDS 
# -------------------------------------------------------------------------

# Load and prepare data ---------------------------------------------------
seed.data <- readRDS("../Data/Sensitivity/transmission_10seeds_full3years.rds")
seeds<- readRDS("../Data/Sensitivity/seeds10_full.rds")
spread10 = readRDS("../Data/Sensitivity/spread10_full.rds")

# # Allocate space to store propagation for each seed
# spread10 = tibble(seed = numeric(),
#                  init_farm = numeric(),
#                farm_type = character(),
#                  spread = numeric())
#  
#  
#  for (seed in seeds){
#    # Extract data 
#    data = seed.data[[as.character(seed)]]
#    
#    for (list in 1:length(data)){
#      # Extract data 
#      trans = data[[list]][[2]]
#      # Extract initial infected farm
#      init_f = trans$farmID[1] 
#      # Count spread
#      spread10 = spread10 %>% add_row(seed = seed,
#                                      init_farm = init_f,
#                                      farm_type = info_all_farms %>% filter(Farmid == init_f) %>% pull(Farm_type),
#                                      spread = nrow(trans))
#      print(list)
#    }
#  }  
# 
#  # Save
#  saveRDS(spread10, file.path("../Data/Sensitivity", "spread10_full.rds") )

paletteer_c("ggthemes::Classic Blue", 30)[seq(1, 30, by = 1)]

colors = c( "#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#05295F", "#08305B")
 
 
# Boxplot of spread in the 10 seads
sens_seed = ggplot(spread10, aes(x=as.character(seed), y=spread, fill=as.character(seed))) + 
  geom_half_violin(side = "r", alpha = 0.7) +
  geom_half_boxplot(side = "l", alpha = 0.7) +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position="none") +
  scale_fill_manual(values = colors) +
  #scale_fill_brewer(palette = "Blues") +
  labs(title = "",
       x = "seed",
       y = "Infected Farms [#]") 
  

# Spread to at least 1 other farm
spread10 %>% filter(spread > 1) %>% 
ggplot(aes(x=as.character(seed), y=spread, fill=as.character(seed))) + 
  geom_half_violin(side = "r", alpha = 0.8) +
  geom_half_boxplot(side = "l", alpha = 0.8) +
  scale_y_log10() +
  scale_fill_manual(values = colors) +
  theme(legend.position="none") +
  labs(title = "",
       x = "Seed",
       y = "Infected Farms [#]")



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
spread.data$idx = c(1:nrow(spread.data))

# Exclude all the zeroes, where the result is the same across all 10 seeds
spread.zero.data = spread.data %>% filter(dif_spread > 0) %>% arrange(dif_spread) %>% ungroup()


# Plot difference in spread across seeds as line plot
ggplot(spread.zero.data, aes(group = seed, x = as.factor(idx))) +
  geom_line(aes(y = dif_spread, col = "Max difference")) + 
  geom_line(aes(y = max_spread, col = "Max number of farms infected")) +
  geom_line(aes(y = min_spread, col = "Min number of farms infected"),alpha = 0.8)



# calculate number and percent of differences between the 10 seeds
pz = spread.data %>% group_by(dif_spread) %>%  summarise(count = n(), percent = n()/nrow(spread.data)*100)
pnz = spread.zero.data %>% group_by(dif_spread) %>%  summarise(count = n(), percent = n()/nrow(spread.data)*100) 

# Plot with and without zeroes
plot_zero = ggplot(pz, aes(x = dif_spread, y = percent)) + 
  geom_bar(stat = "identity") +
  #scale_y_log10() +
  scale_y_sqrt() +
  labs(title = "",
       x = "Max Difference in Propagation",
       y = "% Herds") + 
  theme_minimal()

plot_no_zero = ggplot(pnz, aes(x = dif_spread, y = percent)) + 
  geom_bar(stat = "identity") +
  #scale_y_log10() +
  labs(title = "",
       x = "Max difference in spread across 10 seeds ",
       y = "% of Herds")

# Display both plots
grid.arrange(plot_zero, plot_no_zero, ncol = 2)
Sensitivity_difference = plot_grid(plot_zero, plot_no_zero, 
                            labels = c("Including zero difference", "Excluding zero difference"),
                            #rows = 2,
                            label_size = 16)




# -------------------------------------------------------------------------
#                                  DATES 
# -------------------------------------------------------------------------

# Load and prepare data ---------------------------------------------------
date.data <- readRDS("../Data/Sensitivity/transmission_5_Start_Dates.rds")
Dates = c("01.Jan.2020", "01.Feb.2020", "15.Jan.2020", "02.Mar.2020", "31.Mar.2020")
date5 = readRDS("../Data/Sensitivity/date5_full.rds")

# # Allocate space to store propagation for each seed
# date5 = tibble(date = character(),
#                init_farm = numeric(),
#                farm_type = character(),
#                spread = numeric())
# 
# 
# for (date in Dates){
#   # Extract data 
#   data = date.data[[as.character(date)]]
#   
#   for (list in 1:length(data)){
#     # Extract data 
#     trans = data[[list]][[2]]
#     # Extract initial infected farm
#     init_f = trans$farmID[1] 
#     # Count spread
#     date5 = date5 %>% add_row(date = date,
#                               init_farm = init_f,
#                               farm_type = info_all_farms %>% filter(Farmid == init_f) %>% pull(Farm_type),
#                               spread = nrow(trans))
#     print(paste0(date, "  -  ", list, "/", length(data)))
#   }
# }  
# 
# # Save
# saveRDS(date5, file.path("../Data/Sensitivity", "date5_full.rds") )

#colors = c( "#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#05295F", "#08305B")
colors = c("#6BAED6", "#4292C6", "#2171B5", "#08519C", "#05295F")

date5$date <- factor(date5$date, levels = c("01.Jan.2020", "15.Jan.2020", "01.Feb.2020", "02.Mar.2020", "31.Mar.2020"))


# Boxplot of spread in the 10 seads
sens_date = ggplot(date5, aes(x = date, y=spread, fill=date)) + 
  geom_half_violin(side = "r", alpha = 0.7) +
  geom_half_boxplot(side = "l", alpha = 0.7) +
  scale_y_log10() +
  theme_minimal()+
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Blues") + 
  scale_x_discrete(limits = c("01.Jan.2020", "15.Jan.2020", "01.Feb.2020", "02.Mar.2020", "31.Mar.2020"), 
                   labels = c("01.Jan", "15.Jan", "01.Feb", "02.Mar", "31.Mar")) +
  labs(title = "",
       x = "Date",
       y = "Infected Farms [#]")

# Plot
ggplot(date5, aes(x = as.character(init_farm), y = spread, col = date)) +
  geom_line(aes(group = date), alpha = 0.7) +
  facet_wrap(~ date) +
  scale_color_manual(values = colors) +
  labs(title = "Spread across different introduction date",
       x = "Index herds",
       y = "Number of infected Farms") +
  theme(
    axis.text.x = element_blank(),  # Hide x-axis text
    axis.ticks.x = element_blank(),  # Hide x-axis ticks
    legend.position = "none"  # Optional: hide legend if not needed
  )


# Info of the variation date# Info of the variation across seeds
date5 %>% group_by(date) %>% summarise(max = max(spread),
                                          min = min(spread),
                                          mean = mean(spread),
                                          median = median(spread),
                                          variance = var(spread),
                                          sd = sd(spread))  

# find max, min and diff for each farm across the 10 seeds
spread.data.date = date5 %>% 
  group_by(init_farm) %>% 
  mutate(max_spread = max(spread),
         min_spread = min(spread),
         dif_spread = max(spread) - min(spread)) %>% 
  distinct(init_farm, .keep_all = T) %>% 
  arrange(dif_spread)
spread.data.date$idx = c(1:nrow(spread.data.date))

# Exclude all the zeroes, where the result is the same across all 10 seeds
spread.zero.data.date = spread.data.date %>% filter(dif_spread > 0) %>% arrange(dif_spread) %>% ungroup()


# calculate number and percent of differences between the 10 seeds
pz.date = spread.data.date %>% group_by(dif_spread) %>%  summarise(count = n(), percent = n()/nrow(spread.data.date)*100)
pnz.date = spread.zero.data.date %>% group_by(dif_spread) %>%  summarise(count = n(), percent = n()/nrow(spread.data.date)*100) 

# Plot with and without zeroes
plot_zero.date = ggplot(pz.date, aes(x = dif_spread, y = percent)) + 
  geom_bar(stat = "identity") +
  scale_y_sqrt() +
  labs(title = "",
       x = "Max Difference in Propagation",
       y = "% Herds") +
  theme_minimal()

plot_no_zero.date = ggplot(pnz.date, aes(x = dif_spread, y = percent)) + 
  geom_bar(stat = "identity") +
  #scale_y_log10() +
  labs(title = "",
       x = "Max difference in spread across 10 seeds ",
       y = "% of Herds")

pnz.date %>% filter(dif_spread < 200) %>% 
ggplot(aes(x = dif_spread, y = percent)) + 
  geom_bar(stat = "identity") +
  #scale_y_log10() +
  labs(title = "",
       x = "Max difference in spread across 10 seeds ",
       y = "% of Herds")

# Display both plots
grid.arrange(plot_zero.date, plot_no_zero.date, ncol = 2)
Sensitivity_difference_date = plot_grid(plot_zero.date, plot_no_zero.date, 
                                   labels = c("Including zero difference", "Excluding zero difference"),
                                   #rows = 2,
                                   label_size = 16)


sens_bar = plot_grid(sens_seed, sens_date,
                     labels = "AUTO",
                     rel_widths = c(1.7,1))

sens_dif = plot_grid(plot_zero,plot_zero.date,
                     labels = c("A)  Seeds", "B) Dates"))


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

