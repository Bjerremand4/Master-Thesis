# -------------------------------------------------------------------------
#                           Visualizations  
# -------------------------------------------------------------------------
# Load packages -----------------------------------------------------------
source(file.path("Initialize/Load_packagesNfunctions.R"))


# Load and clean data  -----------------------------------------------------
source(file.path("Initialize/LoadNClean_Data.R"))


# Results module2a --------------------------------------------------------
# Load test results
Farms_result_100 = readRDS("../Data/2a_TestSlaughter/TestSlaughter100%_beta0.05_sample5_sens0.85.rds")
Farms_result_50 = readRDS("../Data/2a_TestSlaughter/TestSlaughter50%_beta0.05_sample5_sens0.85.rds")
Farms_result_33 = readRDS("../Data/2a_TestSlaughter/TestSlaughter33%_beta0.05_sample5_sens0.85.rds")

# Set non detect to 1500 days 
Farms_result_100$discover_days[Farms_result_100$discover_days > 1064] = 2000
Farms_result_50$discover_days[Farms_result_50$discover_days > 1064] = 2000
Farms_result_33$discover_days[Farms_result_33$discover_days > 1064] = 2000

# # Set non detect to 250 propagation
# Farms_result_100$Propagation[Farms_result_100$discover_days > 1064] = 250
# Farms_result_50$Propagation[Farms_result_50$discover_days > 1064] = 250
# Farms_result_33$Propagation[Farms_result_33$discover_days > 1064] = 250

# Comnine data to one tibble
Farms_result_all = rbind(Farms_result_100%>% mutate(m_freq = "100%"),
                         Farms_result_50 %>% mutate(m_freq  = "50%"),
                         Farms_result_33 %>% mutate(m_freq  = "33%"))

# factor the m_freq
Farms_result_all$m_freq <- factor(Farms_result_all$m_freq, levels=c("100%", "50%", "33%"))

# Filter data
Farms_result_all_prop = Farms_result_all %>% filter(discover_days < 1064)

# Create the plot
p_prop_all = ggplot(Farms_result_all_prop, aes(x = m_freq, y = Propagation, fill = m_freq)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(side = "l") +
  geom_point(data = Farms_result_all %>% filter(discover_days > 1064),
             aes(x = m_freq, y = Propagation, color = "Not Detected"), # Use a placeholder for the color
             size = 0.9, shape = 21, fill = "goldenrod") +
  theme_minimal() + 
  scale_color_manual(name = "", values = c("Not Detected" = "goldenrod")) + # Create the custom legend
  scale_fill_brewer(palette = "Blues") + 
  labs(#title = "",
       x = "Movement Frequency",
       y = "Infected Farms [#]") +
  theme(legend.position = "top") +
  guides(fill = "none")# Position the legend to the right


# Farms_result_all_prop = Farms_result_all %>% filter(discover_days < 1064)
# 
# p_prop_all = ggplot(Farms_result_all_prop, aes(x = m_freq, y=Propagation, fill = m_freq)) + 
#   geom_half_violin(side = "r", alpha = 1) +
#   geom_half_boxplot(side = "l") +
#   geom_point(data = Farms_result_all %>% filter(discover_days > 1064),
#              aes(x = m_freq, y = Propagation), 
#              color = "goldenrod", size = 0.9, shape = 21, fill = "goldenrod") +
#   theme_minimal() + 
#   theme(legend.position="none") +
#   scale_fill_brewer(palette = "Blues") + 
#   labs(title = "",
#        x = "Movement Frequency",
#        y = "Infected Farms [#]")



# Days after intro 
p_days_all = ggplot(Farms_result_all, aes(x = m_freq, y = discover_days, fill = m_freq)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(data = Farms_result_all %>% filter(discover_days < 1064),
                    aes(x = m_freq, y=discover_days, 
                        fill = m_freq), side = "l") +
  theme_minimal() +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "",
       x = "Movement Frequency",
       y = "Days to detect [#]") +
  scale_y_continuous(labels = function(x) ifelse(x == 2000, "not detected", x))

# Display them side by side
plot_grid(p_prop_all, p_days_all, labels="AUTO", rel_widths = c(1, 1.1))
Prop_allFreq_plot = plot_grid(p_prop_all, p_days_all, labels="AUTO")
# save plot
ggsave("../Results/Propagation_allFreq1.tiff", plot = Prop_allFreq_plot, units="in", width=20, height=8, dpi=300,compression = 'lzw')





# Reduce moves by farmtype
p_prop_100 = ggplot(Farms_result_100, aes(x=Farmtype, y= Propagation, fill=Farmtype)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(data = Farms_result_all %>% filter(Propagation < 250),
                    aes(x = Farmtype, y = Propagation, 
                        fill = Farmtype), side = "l")  +
  #scale_y_log10() +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Blues") +
  labs(title = "Propagation at detection - 50% of movements normal slaughter",
       x = "Farmtype",
       y = "Infected Farms [#]") +
  scale_x_discrete(labels = c("15 11","15 13","15 17","15 41","15 46")) +
  scale_y_continuous(labels = function(x) ifelse(x == 250, "not detected", x))

# Reduce moves but not to slaughterhouses 
p_prop_50 = ggplot(Farms_result_50, aes(x=Farmtype, y=Propagation, fill=Farmtype)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(data = Farms_result_all %>% filter(Propagation < 250),
                    aes(x = Farmtype, y = Propagation, 
                        fill = Farmtype), side = "l")  +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Blues") +
  labs(title = "Propagation at detection - 50% of movements normal slaughter",
       x = "Farmtype",
       y = "Infected Farms [#]") +
  scale_x_discrete(labels = c("15 11","15 13","15 17","15 41","15 46")) +
  scale_y_continuous(labels = function(x) ifelse(x == 250, "not detected", x))

# Reduce all moves 
p_prop_33 = ggplot(Farms_result_33, aes(x=Farmtype, y=Propagation, fill=Farmtype)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(data = Farms_result_all %>% filter(Propagation < 250),
                    aes(x = Farmtype, y = Propagation, 
                        fill = Farmtype), side = "l")  +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Blues") +
  labs(title = "Propagation at detection - 33% of all movements",
       x = "Farmtype",
       y = "Infected Farms [#]") +
  scale_x_discrete(labels = c("15 11","15 13","15 17","15 41","15 46")) +
  scale_y_continuous(labels = function(x) ifelse(x == 250, "not detected", x))

grid.arrange(p_prop_100, p_prop_50, p_prop_33, ncol = 3)
#Show both plots
Prop_allType_plot = arrangeGrob(p_prop_100, p_prop_50, p_prop_33, ncol = 3)
# save
#ggsave("../Results/Propagation_2a_1.tiff", plot = Prop_allType_plot, units="in", width=20, height=5, dpi=300, compression = 'lzw')



# Plot the number of days since agent introduction at detection for all movements and 50% of movements
p_days_100 = ggplot(Farms_result_100, aes(x=Farmtype, y=discover_days, fill=Farmtype)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(data = Farms_result_all %>% filter(discover_days < 1500),
                    aes(x = Farmtype, y = discover_days, 
                        fill = Farmtype), side = "l")  +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Blues") +
  labs(title = "Days at detection, all movements",
       x = "Farmtype",
       y = "Time sime intro of pathogen [days]")+
  scale_x_discrete(labels = c("15 11","15 13","15 17","15 41","15 46")) +
  scale_y_continuous(labels = function(x) ifelse(x == 2000, "not detected", x))

# Reduce moves not slaughter
p_days_50 = ggplot(Farms_result_50, aes(x=Farmtype, y=discover_days, fill=Farmtype)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(data = Farms_result_all %>% filter(discover_days < 1500),
                    aes(x = Farmtype, y = discover_days, 
                        fill = Farmtype), side = "l")  +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Blues") +
  labs(title = "Days at detection, 50% of movements normal slaughter",
       x = "Farmtype",
       y = "Time sime intro of pathogen [days]")+
  scale_x_discrete(labels = c("15 11","15 13","15 17","15 41","15 46")) +
  scale_y_continuous(labels = function(x) ifelse(x == 2000, "not detected", x))

# Reduce all moves including slaughter
p_days_33 = ggplot(Farms_result_33, aes(x=Farmtype, y=discover_days, fill=Farmtype)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(data = Farms_result_all %>% filter(discover_days < 1500),
                    aes(x = Farmtype, y = discover_days, 
                        fill = Farmtype), side = "l")  +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Blues") +
  labs(title = "Days at detection, 50% of movements",
       x = "Farmtype",
       y = "Time sime intro of pathogen [days]")+
  scale_x_discrete(labels = c("15 11","15 13","15 17","15 41","15 46")) +
  scale_y_continuous(labels = function(x) ifelse(x == 2000, "not detected", x))

# Display both plots
grid.arrange(p_days_100, p_days_50, p_days_33)
Days_all_plot = arrangeGrob(p_days_100, p_days_50, p_days_33, ncol = 3)
#ggsave("../Results/Days_diffReduction.tiff", plot = Days_all_plot, units="in", width=20, height=5, dpi=300, compression = 'lzw')

#Display all in one plot 
DayProp_all_plot = arrangeGrob(p_prop_100, p_prop_50, p_prop_33,
                               p_days_100, p_days_50, p_days_33,
                               ncol = 3, nrow = 2)
ggsave("../Results/Dayprop_diffReduction1.tiff", plot = DayProp_all_plot, units="in", width=20, height=10, dpi=300, compression = 'lzw')

# Display all four plots
grid.arrange(p_prop_full, p_prop_reduce, 
             p_days_full, p_days_reduce, 
             ncol = 2, nrow = 2)

# # Display all four plots with same y-scale
# grid.arrange(p_prop_full, (p_prop_reduce+ ylim(0,165)), 
#              p_days_full, p_days_reduce, 
#              ncol = 2, nrow = 2)


# p2 = ggplot(Farms_result, aes(y=discover_days))+ 
#   geom_half_violin(side = "r", alpha = 0.7) +
#   geom_half_boxplot(side = "l", alpha = 0.7) +
#   labs(title = "Days before disover",
#        x = "",
#        y = "Days since pathogen introduction")
# 
# p_tran = ggplot(propagation, aes(y=spread))+ 
#   geom_half_violin(side = "r", alpha = 0.7) +
#   geom_half_boxplot(side = "l", alpha = 0.7) +
#   labs(title = "Sygdomsudbredelse med beta = 0.05",
#        x = "",
#        y = "Number of infected Farms")




# Results module2b --------------------------------------------------------
Farms_result_5sample_vir = readRDS("../Data/2a_TestSlaughter/TestSlaughter100%_beta0.2_sample5_sens0.85.rds")
Farms_result_50sample_vir = readRDS("../Data/2a_TestSlaughter/TestSlaughter100%_beta0.2_sample50_sens0.7.rds")
Visual_results_vir = readRDS("../Data/2b_VisualInspection/Detect_beta0.2move_100%.rds") 

# Identify undetected herds
Farms_result_5sample_vir$discover_days[Farms_result_5sample_vir$discover_days > 1064] = 1500
Farms_result_50sample_vir$discover_days[Farms_result_50sample_vir$discover_days > 1064] = 1500
Visual_results_vir$days_since_intro[Visual_results_vir$days_since_intro > 1064] = 1500

# Collect the three in one and give them an extra col specifying the testType
result_testType_all = rbind(Farms_result_5sample_vir %>% select(-Farmtype) %>% mutate(testType = "5_Samples"),
                         Farms_result_50sample_vir %>% select(-Farmtype) %>% mutate(testType  = "50_Samples"),
                         Visual_results_vir %>%select(FarmID = init_farm, 
                                                      Detection_date = detection_date,
                                                      Propagation = propagation,
                                                      discover_days = days_since_intro) %>%  mutate(testType  = "Observation"))
#Farms_result_vir50 = readRDS("../Data/2a_TestSlaughter/Slaughter_TestResult_virus_50samples.rds")


# Plot the number of infected herds at detection for all movements and 50% of movements
p_prop_testType = ggplot(result_testType_all, aes(x = testType, y=Propagation, fill = testType)) + 
  geom_half_violin(side = "r", alpha = 1, scale = 3) +
  geom_half_boxplot(side = "l", alpha = 1) +
  geom_point(data = result_testType_all %>% filter(discover_days > 1064),
             aes(x = testType, y = Propagation, color = "Not Detected"), # Use a placeholder for the color
             size = 0.9, shape = 21, fill = "goldenrod") + 
  theme_minimal() + 
  scale_color_manual(name = "", values = c("Not Detected" = "goldenrod")) +
  #scale_y_log10() +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "",
       x = "Test type",
       y = "Infected Farms [#]") +
  theme(legend.position = "top") + 
  guides(fill = "none")

p_days_testType = ggplot(result_testType_all, aes(x = testType, y = discover_days, fill = testType)) + 
  geom_half_violin(side = "r", alpha = 1, scale = 3) +
  geom_half_boxplot(data = result_testType_all %>% filter(discover_days < 1064),
                    aes(x = testType, y=discover_days,
                        fill = testType), side = "l") +
  #scale_y_log10() +
  theme_minimal() +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "",
       x = "Test Type",
       y = "Detection time [Days]") +
  scale_y_continuous(labels = function(x) ifelse(x == 1500, "Not Detected", x))


result_testType = plot_grid(p_prop_testType, p_days_testType, labels="AUTO", rel_widths = c(1, 1.1))
#Show both plots
result_testType_All = arrangeGrob(p_prop_testType, p_days_testType, ncol = 2)
# save
ggsave("../Results/result_testType_all.tiff", plot = result_testType, units="in", width=20, height=10, dpi=300, compression = 'lzw')


# For sjov bacteria
Visual_results_bac = readRDS("../Data/2b_VisualInspection/Detect_beta0.05move_100%.rds")

ggplot(Visual_results_bac, aes(y = days_since_intro)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(side = "l", alpha = 1) +
  #scale_y_log10() +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "YlGnBu") + 
  labs(title = "Time for detection - Test at slaughter",
       x = "Test type",
       y = "Days since pathogen intro [#]")






# Data  -------------------------------------------------------------------
# Simple counts -----------------------------------------------------------
# Count how many of each farmtype and how many pigs are within
a = info_all_farms %>%  group_by(Farm_type) %>% summarise(n_farms = n(),
                                                          total_n_pigs = sum(N_pigs, na.rm = T),
                                                          min_pigs = min(N_pigs, na.rm = T),
                                                          max_pigs = max(N_pigs, na.rm = T),
                                                          median_pigs = median(N_pigs, na.rm = T))

# Appearance Of Herds across years-----------------------------------------------------
total_2020 = info_all_farms_years %>% filter(Year == 2020) %>% distinct(Farmid) %>% pull(Farmid)
total_2021 = info_all_farms_years %>% filter(Year == 2021) %>% distinct(Farmid) %>% pull(Farmid)
total_2022 = info_all_farms_years %>% filter(Year == 2022) %>% distinct(Farmid) %>% pull(Farmid)
total_all  = info_all_farms_years %>% distinct(Farmid) %>% pull(Farmid)

only_2020 = setdiff(total_2020, union(total_2021, total_2022))
only_2021 = setdiff(total_2021, union(total_2020, total_2022))
only_2022 = setdiff(total_2022, union(total_2020, total_2021))
only_all  = intersect(intersect(total_2020, total_2021), total_2022)

Intersect_20_21 = intersect(total_2020, total_2021)
Intersect_20_22 = intersect(total_2020, total_2022)
Intersect_21_22 = intersect(total_2021, total_2022)

union_20_21 = union(total_2020, total_2021)
union_20_22 = union(total_2020, total_2022)
union_21_22 = union(total_2021, total_2022)

# Distribution of herd types  - raw data and final data -------------------
Herds_raw_data = info_all %>% group_by(Farm_type) %>% count()
Herds_used = info_all_farms %>% group_by(Farm_type) %>% count()

# Movements from the different types
raw.data %>% group_by(SENDER_FARMTYPE) %>% count()
move.data %>% group_by(SENDER_FARMTYPE) %>% count()



# Number of movements - distribution --------------------------------------
# Count amount of sendings and receivings as well as how many pigs are sent and received by each farm type
N_move = move.data %>%  count(RECEIVER_FARMTYPE, name = "receivings") %>% rename(Farm_type = RECEIVER_FARMTYPE) %>% 
  left_join(move.data %>%  count(SENDER_FARMTYPE, name = "sendings"), by = c("Farm_type" = "SENDER_FARMTYPE")) %>% 
  left_join(move.data %>% filter(RECEIVER_FARMTYPE != "15 68 Svineslagteri") %>%  count(SENDER_FARMTYPE, name = "sendings_ns"), by = c("Farm_type" = "SENDER_FARMTYPE")) %>%
  left_join(info_all_farms %>% group_by(Farm_type) %>% summarise(Total_N_pigs = sum(N_pigs, na.rm = TRUE)) %>% ungroup(), by = "Farm_type") %>% 
  left_join(move.data %>% group_by(SENDER_FARMTYPE) %>% summarise(mean_send_size = mean(N_MOVED_PIGS)), by = c("Farm_type" = "SENDER_FARMTYPE")) %>% 
  left_join(move.data %>% group_by(RECEIVER_FARMTYPE) %>% summarise(mean_receive_size = mean(N_MOVED_PIGS)),by = c("Farm_type" = "RECEIVER_FARMTYPE")) %>% 
  left_join(move.data %>% group_by(SENDER_FARMTYPE) %>% summarise(Total_pigs_sent = sum(N_MOVED_PIGS[!is.na(FARMID_SENDER)])), by = c("Farm_type" = "SENDER_FARMTYPE")) %>% 
  left_join(move.data %>% group_by(RECEIVER_FARMTYPE) %>% summarise(Total_pigs_received = sum(N_MOVED_PIGS[!is.na(FARMID_RECEIVER)])), by = c("Farm_type" = "RECEIVER_FARMTYPE")) %>% 
  mutate(SlaughterSendings = sendings - coalesce(sendings_ns, 0)) %>% mutate_all( ~replace_na(.,0)) %>%
  select(Farm_type, Total_N_pigs, sendings, mean_send_size, receivings, mean_receive_size, SlaughterSendings, Total_pigs_sent, Total_pigs_received)

# Ready the data for plotting
n_move = N_move %>% select(Farm_type, "Sent" = sendings, "Received" = receivings) %>%
  pivot_longer( cols = !Farm_type, names_to = "movement", values_to = "moves") %>% 
  mutate(type = c("Production","Production","Organic","Organic","Free-Range","Free-Range","Breed + multi","Breed + multi","Piglets","Piglets","Slaughter","Slaughter")) %>% 
  mutate(moves = replace(moves, moves == 0, NA))

# Plot bar chart 
ggplot(n_move, aes(Farm_type, x = type, y = moves, fill = movement)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_y_log10() +
  geom_text(aes(label=moves), vjust= -0.3, color="black",
            position = position_dodge(0.9), size=3.5) +
  labs(title = "Distribution of movement across herd types",
       x = "Herd type",
       y = "Movements [#]") +
  scale_fill_manual(name = "Type of move",
                    values =  c("#6BAED6", "#3182BD")) +
  theme_minimal()
# save
#ggsave("../Results/Data/MovementDistribution.tiff", units="in", width=9, height=5, dpi=300, compression = 'lzw')

# Sendings over time - weekly and monthly ---------------------------------
# Group by every week starting on Saturday and sum the number of pigs moved
pigs_sent_per_week <- move.data %>%
  group_by(week = lubridate::floor_date(DATE_MOVEMENT, "week", week_start = 3)) %>%
  summarise(Total_pigs_sent = sum(N_MOVED_PIGS[!is.na(FARMID_SENDER)]))

# Plot
weekly_sending = ggplot(pigs_sent_per_week, aes(x = week, y = Total_pigs_sent)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Number of Pigs Sent Each Week",
       x = "Time [Week]",
       y = "Pigs Sent [#] ")
# Save plot
#ggsave("../Results/Data/WeeklySendings.tiff", units="cm", width=18, height=8, dpi=300, compression = 'lzw')

# Group by every month and sum the number of pigs moved
pigs_sent_per_month <- move.data %>%
  group_by(month = lubridate::floor_date(DATE_MOVEMENT, "month")) %>%
  summarise(Total_pigs_sent = sum(N_MOVED_PIGS[!is.na(FARMID_SENDER)]))
# Plot
monthly_sendings = ggplot(pigs_sent_per_month, aes(x = month, y = Total_pigs_sent)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Number of Pigs Sent Each Month",
       x = "Time [Month]",
       y = "Pigs Sent [#]") 
# Save plot
#ggsave("../Results/Figures_Rapport/Data/MonthlySendings.tiff", units="cm", width=18, height=8, dpi=300, compression = 'lzw')

# plot an save monthly and weekly together
month_week_Sendings = arrangeGrob(weekly_sending, monthly_sendings,
                                  ncol = 1, nrow = 2)
ggsave("../Results/Figures_Rapport/Data/month_week_Sendings.tiff", plot = month_week_Sendings, units="cm", width=18, height=16, dpi=300, compression = 'lzw')


# Distribution of sending ------------------------------------------------
# Including only positive counts
dist1 <- move.data %>%
  group_by(SENDER_FARMTYPE, RECEIVER_FARMTYPE) %>% 
  summarise(Npigs = sum(N_MOVED_PIGS)) 

# Plotting using facet_wrap the distribution of sent pigs from each framtype in every plot
ggplot(data=dist1, aes(x=RECEIVER_FARMTYPE, y=Npigs, fill=RECEIVER_FARMTYPE)) +
  geom_bar(stat="identity") +
  scale_y_log10() +
  geom_text(aes(label=Npigs), vjust= -0.3, color="black",
            position = position_dodge(0.9), size=3) +
  facet_wrap(~SENDER_FARMTYPE, ncol = 3, 
             labeller = labeller(SENDER_FARMTYPE = 
                                   c("15 11 Svin, produktionsbesætning" = "15 11 Production", 
                                     "15 13 Økologisk svinebesætning" = "15 13 Organic", 
                                     "15 17 Frilandssvinebesætning" = "15 17 Free-range", 
                                     "15 41 Svin, avls- og opformeringsbesætn." = "15 41 Breeding & Multiplication", 
                                     "15 46 Smågriseopdrætsbesætning" = "15 46 Piglets", 
                                     "15 68 Svineslagteri" = "15 68 Slaughterhouse"))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank()) +
  scale_fill_manual(name = "Receiving Herd type",
                    values = c("#FFEDA0", "#C7E9B4" ,"#7FCDBB", "#41B6C4", "#2C7FB8", "#253494"),
                    labels = c("15 11 Production", "15 13 Organic", "15 17 Free-range", "15 41 Breeding & Multi", "15 46 Piglets", "15 68 Slaughter")) +
  labs(y = "Pigs sent [#]",
       x = "")

ggplot(data=dist1, aes(x=RECEIVER_FARMTYPE, y=Npigs, fill=RECEIVER_FARMTYPE)) +
  geom_bar(stat="identity") +
  scale_y_log10() +
  geom_text(aes(label=Npigs), vjust= -0.3, color="black",
            position = position_dodge(0.9), size = 3.5) +
  facet_wrap(~SENDER_FARMTYPE, ncol = 3, 
             labeller = labeller(SENDER_FARMTYPE = 
                                   c("15 11 Svin, produktionsbesætning" = "15 11 Production", 
                                     "15 13 Økologisk svinebesætning" = "15 13 Organic", 
                                     "15 17 Frilandssvinebesætning" = "15 17 Free-Range", 
                                     "15 41 Svin, avls- og opformeringsbesætn." = "15 41 Breeding & Multiplication", 
                                     "15 46 Smågriseopdrætsbesætning" = "15 46 Piglets", 
                                     "15 68 Svineslagteri" = "15 68 Slaughterhouse"))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        strip.text = element_text(face = "bold",size = 11)) +  # Make facet titles bold
  scale_fill_manual(name = "Receiving Type",
                    values = c("#FFEDA0", "#C7E9B4" ,"#7FCDBB", "#41B6C4", "#2C7FB8", "#253494"),
                    labels = c("15 11 Production", "15 13 Organic", "15 17 Free-Range", "15 41 Breeding & Multi", "15 46 Piglets", "15 68 Slaughter")) +
  labs(y = "Pigs sent [#]",
       x = "")



# Save
ggsave("../Results/DistributionSendings.tiff", units="in", width=12.5, height=8, dpi=300, compression = 'lzw')


# Weekly and monthly sending and receiving across types  --------------------------------------------

# Group by every week starting on Saturday and sum the number of pigs moved for each farm type
pigs_sent_per_week <- move.data %>%
  group_by(week = lubridate::floor_date(DATE_MOVEMENT, "week", week_start = 3), SENDER_FARMTYPE) %>%
  summarise(Total_pigs_sent = sum(N_MOVED_PIGS[!is.na(FARMID_SENDER)]))

# Compute total number of pigs sent per week across all farm types
total_pigs_sent_per_week <- pigs_sent_per_week %>%
  group_by(week) %>%
  summarise(Total_pigs_sent = sum(Total_pigs_sent))

# Add a row for total to the pigs_sent_per_week dataset
total_row <- total_pigs_sent_per_week %>%
  mutate(SENDER_FARMTYPE = "Total")
pigs_sent_per_week <- bind_rows(pigs_sent_per_week, total_row)


# Plot
Sent_plot <- ggplot(pigs_sent_per_week, aes(x = week, y = Total_pigs_sent, color = SENDER_FARMTYPE, group = SENDER_FARMTYPE)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "",
       x = "Time [Week]",
       y = "Pigs Sent [#]",
       color = "Farm Type") +
  scale_color_manual(name = "Herd type",
                     values = c("#FFEDA0", "#C7E9B4" ,"#7FCDBB", "#41B6C4", "#2C7FB8","#081D58"),
                     labels = c("15 11 Production", 
                                "15 13 Organic", 
                                "15 17 Free-range", 
                                "15 41 Breeding & Multi", 
                                "15 46 Piglets",
                                "Total")) 
  
  

### Receivings per week
pigs_receive_per_week <- move.data %>%
  group_by(week = lubridate::floor_date(DATE_MOVEMENT, "week", week_start = 3), RECEIVER_FARMTYPE) %>%
  summarise(Total_pigs_receive = sum(N_MOVED_PIGS[!is.na(FARMID_RECEIVER)]))

# Compute total number of pigs receive per week across all farm types
total_pigs_receive_per_week <- pigs_receive_per_week %>%
  group_by(week) %>%
  summarise(Total_pigs_receive = sum(Total_pigs_receive))

# Add a row for total to the pigs_sent_per_week dataset
total_row <- total_pigs_receive_per_week %>%
  mutate(RECEIVER_FARMTYPE = "Total")
pigs_receive_per_week <- bind_rows(pigs_receive_per_week, total_row)

# PLot 
receive_plot <- ggplot(pigs_receive_per_week, aes(x = week, y = Total_pigs_receive, color = RECEIVER_FARMTYPE, group = RECEIVER_FARMTYPE)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  labs(title = "",
       x = "Time [Week]",
       y = "Pigs received [#]",
       color = "Farm Type") +
  scale_color_manual(name = "Herd type",
                     values = c("#FFEDA0", "#C7E9B4" ,"#7FCDBB", "#41B6C4", "#2C7FB8","#253494","#081D58"),
                     labels = c("15 11 Production", 
                                "15 13 Organic", 
                                "15 17 Free-range", 
                                "15 41 Breeding & Multi", 
                                "15 46 Piglets", 
                                "15 68 Slaughter",
                                "Total")) +
  theme_minimal()

plot_grid(Sent_plot, receive_plot, 
          labels = c("Weekly Sendings", "Weekly Receivings"),
          nrow = 2)

#Monthly sending and receivings
# compute number of pigs sent monthly across the farm types 
pigs_sent_per_month <- move.data %>%
  group_by(month = lubridate::floor_date(DATE_MOVEMENT, "month"), SENDER_FARMTYPE) %>%
  summarise(Total_pigs_sent = sum(N_MOVED_PIGS[!is.na(FARMID_SENDER)]))
# compute number of pigs sent monthly
total_pigs_sent_per_month <- pigs_sent_per_month %>%
  group_by(month) %>%
  summarise(Total_pigs_sent = sum(Total_pigs_sent))
total_row_m <- total_pigs_sent_per_month %>%
  mutate(SENDER_FARMTYPE = "Total")
# Combine data 
pigs_sent_per_month <- bind_rows(pigs_sent_per_month, total_row_m)

# Plot
Sent_plot_month <- ggplot(pigs_sent_per_month, aes(x = month, y = Total_pigs_sent, color = SENDER_FARMTYPE, group = SENDER_FARMTYPE)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  labs(title = "",
       x = "Time [Month]",
       y = "Pigs Sent [#]",
       color = "Farm Type") +
  scale_color_manual(name = "Herd type",
                     values = c("#FFEDA0", "#C7E9B4" ,"#7FCDBB", "#41B6C4", "#2C7FB8","#081D58"),
                     labels = c("15 11 Production", 
                                "15 13 Organic", 
                                "15 17 Free-range", 
                                "15 41 Breeding & Multi", 
                                "15 46 Piglets",
                                "Total")) +
  theme_minimal()


# compute number of pigs sent monthly across the farm types 
pigs_receive_per_month <- move.data %>%
  group_by(month = lubridate::floor_date(DATE_MOVEMENT, "month"), RECEIVER_FARMTYPE) %>%
  summarise(Total_pigs_receive = sum(N_MOVED_PIGS[!is.na(FARMID_RECEIVER)]))
# compute number of pigs sent monthly
total_pigs_receive_per_month <- pigs_receive_per_month %>%
  group_by(month) %>%
  summarise(Total_pigs_receive = sum(Total_pigs_receive))
total_row_mr <- total_pigs_receive_per_month %>%
  mutate(RECEIVER_FARMTYPE = "Total")
# Combine data 
pigs_receive_per_month <- bind_rows(pigs_receive_per_month, total_row_mr)


Receive_plot_month <- ggplot(pigs_receive_per_month, aes(x = month, y = Total_pigs_receive, color = RECEIVER_FARMTYPE, group = RECEIVER_FARMTYPE)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  labs(title = "",
       x = "Time [Month]",
       y = "Pigs Received [#]",
       color = "Farm Type") +
  scale_color_manual(name = "Herd type",
                     values = c("#FFEDA0", "#C7E9B4" ,"#7FCDBB", "#41B6C4", "#2C7FB8","#253494","#081D58"),
                     labels = c("15 11 Production", 
                                "15 13 Organic", 
                                "15 17 Free-range", 
                                "15 41 Breeding & Multi", 
                                "15 46 Piglets",
                                " 11 68 Slaughter",
                                "Total")) +
  theme_minimal()

# Arrange the plots using grid.arrange
grid.arrange(Sent_plot, receive_plot, ncol = 1, nrow = 2)              # Weekly   
grid.arrange(Sent_plot_month, Receive_plot_month, ncol = 1, nrow = 2)  # Monthly

plot_grid(Sent_plot_month, Receive_plot_month, 
          labels = c("Monthly Sendings", "Monthly Receivings"),
          nrow = 2)

plot_grid(Sent_plot, receive_plot, Sent_plot_month, Receive_plot_month, 
          labels = c("Weekly Sendings", "Weekly Receivings", "Monthly Sendings", "Monthly Receivings"),
          ncol = 2,
          nrow = 2)
# Overshooting -----------------------------------------------------------
# Farms sending more than they have (3638 stk.)
big_send = move.data %>% 
  filter(N_MOVED_PIGS > SENDER_N_PIGS) %>% mutate(overshoot = N_MOVED_PIGS - SENDER_N_PIGS,
                                                  overshoot_percent = ifelse(round(overshoot/SENDER_N_PIGS*100) == 1, 1.1, round(overshoot/SENDER_N_PIGS*100))) %>% 
  select(-SENDER_FARMTYPE, -RECEIVER_FARMTYPE)
# Plot
hist.data.send = hist(big_send$overshoot_percent, breaks = 20)
# Plot med logaritmisk y-akse
overshoot_send = ggplot(data.frame(x = hist.data.send$breaks[-1], y = ifelse(hist.data.send$counts == 1, 1.1 ,hist.data.send$counts)), aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = "#1F78B4") +
  scale_y_log10() +
  labs(title = "Movements larger than Sender capacity",
       x = "[% pigs send more than capacity]",
       y = "Number of movements")
# Save
ggsave("../Results/overshootSendings.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')


# Gårde der modtager mere end de har plads til (1406 stk)
big_receive = move.data %>% filter(RECEIVER_FARMTYPE != "15 68 Svineslagteri") %>% 
  filter(N_MOVED_PIGS > RECEIVER_N_PIGS) %>% mutate(overshoot = N_MOVED_PIGS - RECEIVER_N_PIGS,
                                                    overshoot_percent = round(overshoot/RECEIVER_N_PIGS*100)) %>% 
  select(-SENDER_FARMTYPE, -RECEIVER_FARMTYPE)
# Plot
hist.data.receive = hist(big_receive$overshoot_percent, breaks = 20)
# Plot med logaritmisk y-akse
overshoot_receive = ggplot(data.frame(x = hist.data.receive$breaks[-1], y = ifelse(hist.data.receive$counts == 1, 1.1 ,hist.data.receive$counts)), 
                           aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = "#1F78B4") +
  scale_y_log10() +
  labs(title = "Movements larger than Receiver capacity",
       x = "[% pigs received more than capacity]",
       y = "Number of movements ")
# Save
ggsave("../Results/overshootReceivings.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')


grid.arrange(overshoot_send, overshoot_receive, ncol = 2)
overshootPlots = arrangeGrob(overshoot_send, overshoot_receive, ncol = 2)
# Save plots 
ggsave("../Results/overshootBoth.tiff", plot = overshootPlots, units="in", width=10, height=4, dpi=300, compression = 'lzw')


# DIvide by year Send
hist.data.send20 = hist(big_send %>% filter(Year == 2020) %>% pull(overshoot_percent), breaks = 20) # breaks = 20 for dodge
hist.data.send21 = hist(big_send %>% filter(Year == 2021) %>% pull(overshoot_percent), breaks = 10) # breaks = 10 for dodge
hist.data.send22 = hist(big_send %>% filter(Year == 2022) %>% pull(overshoot_percent), breaks = 20) # breaks = 20 for dodge

# Put data together
data.p.send = tibble(x = c(hist.data.send20$breaks[-1], hist.data.send20$breaks[-1], hist.data.send20$breaks[-1]),
                     y = c(ifelse(hist.data.send20$counts == 1, 1.1 ,hist.data.send20$counts),
                           c(ifelse(hist.data.send21$counts == 1, 1.1 ,hist.data.send21$counts), rep(0,length(hist.data.send20$breaks[-1])-length(hist.data.send21$breaks[-1]))),
                           c(ifelse(hist.data.send22$counts == 1, 1.1 ,hist.data.send22$counts), rep(0,length(hist.data.send20$breaks[-1])-length(hist.data.send22$breaks[-1])))),
                     year = c(rep("2020", length(hist.data.send20$counts)),
                              rep("2021", length(hist.data.send20$counts)),
                              rep("2022", length(hist.data.send20$counts))))
data.p.send = data.p.send %>% mutate(y = replace(y, y == 0, NA))
data.p.send = data.p.send %>% arrange(desc(y))

# Set colors
colors = rev(RColorBrewer::brewer.pal(6, "Blues")[4:6])
#plot
# dodge years
overshoot_send_year = ggplot(data.p.send , aes(x = x, y = y, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +
  labs(title = "",#"Movements larger than Sender capacity",
       x = "Pigs send more than capacity [%]",
       y = "Movements [#] ") +
  scale_fill_manual(name = "Year",
                    values =  colors) +
  theme_minimal()
# Save plot
#ggsave("../Results/overshootReceivings_yearly_dodge.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#nudge years
overshoot_send_year1 = ggplot(data.p.send , aes(x = x, y = y, fill = year, order = )) +
  geom_bar(stat = "identity", position = "nudge") +
  scale_y_log10() +
  labs(title = "Movements larger than Sender capacity",
       x = "[% pigs send more than capacity]",
       y = "Number of movements ") +
  scale_fill_manual(name = "Year",
                    values =  colors)
# Save plot
#ggsave("../Results/overshootReceivings_yearly_nudge.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#grid.arrange(overshoot_send_year1, overshoot_send_year, ncol = 2)


# DIvide by year Receive
hist.data.receive20 = hist(big_receive %>% filter(Year == 2020) %>% pull(overshoot_percent), breaks = 20) # breaks = 20 for dodge
hist.data.receive21 = hist(big_receive %>% filter(Year == 2021) %>% pull(overshoot_percent), breaks = 5)  # breaks =  5 for dodge
hist.data.receive22 = hist(big_receive %>% filter(Year == 2022) %>% pull(overshoot_percent), breaks = 10) # breaks = 10 for dodge

# Put data together
data.p.receive = tibble(x = c(hist.data.receive20$breaks[-1], hist.data.receive20$breaks[-1], hist.data.receive20$breaks[-1]),
                        y = c(ifelse(hist.data.receive20$counts == 1, 1.1 , hist.data.receive20$counts),
                              c(ifelse(hist.data.receive21$counts == 1, 1.1 ,hist.data.receive21$counts), rep(0,length(hist.data.receive20$counts)-length(hist.data.receive21$counts))),
                              c(ifelse(hist.data.receive22$counts == 1, 1.1 ,hist.data.receive22$counts), rep(0,length(hist.data.receive20$counts)-length(hist.data.receive22$counts)))),
                        year = c(rep("2020", length(hist.data.receive20$counts)),
                                 rep("2021", length(hist.data.receive20$counts)),
                                 rep("2022", length(hist.data.receive20$counts))))
data.p.receive = data.p.receive %>% mutate(y = replace(y, y == 0, NA))
data.p.receive = data.p.receive %>% arrange(desc(y))

# Set colors
colors = rev(RColorBrewer::brewer.pal(6, "Blues")[4:6])
#plot
overshoot_receive_year = ggplot(data.p.receive , aes(x = x, y = y, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +
  labs(title = "",
       x = "Pigs received more than capacity [%]",
       y = "Movements [#] ") +
  scale_fill_manual(name = "Year",
                    values =  colors) +
  theme_minimal()
# Save plot
#ggsave("../Results/overshootReceivings_yearly_dodge.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')


overshoot_receive_year1 = ggplot(data.p.receive , aes(x = x, y = y, fill = year )) +
  geom_bar(stat = "identity", position = "nudge") +
  scale_y_log10() +
  labs(title = "Movements larger than Receiver capacity",
       x = "[% pigs received more than capacity]",
       y = "Number of movements ") +
  scale_fill_manual(name = "Year",
                    values =  colors)
# Save plot
#ggsave("../Results/overshootReceivings_yearly_nudge.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')



grid.arrange(overshoot_send, overshoot_receive,
             overshoot_send_year,overshoot_receive_year,
             overshoot_send_year1, overshoot_receive_year1,
             ncol = 2, nrow = 3)

overshootTwoPlots = plot_grid(overshoot_send_year,
                              overshoot_receive_year,
                              labels = c("(A) Sendings", "(B) Receivings"),
                              rows = 2)


overshootAllPlots = arrangeGrob(overshoot_send, overshoot_receive,
                                overshoot_send_year,overshoot_receive_year,
                                overshoot_send_year1, overshoot_receive_year1,
                                ncol = 2, nrow = 3)

# Save plot
ggsave("../Results/overshoottwoPlots.tiff", plot = overshootTwoPlots , units="in", width=20, height=8, dpi=300, compression = 'lzw')
ggsave("../Results/overshootallPlots.tiff", plot = overshootAllPlots , units="in", width=20, height=12, dpi=300, compression = 'lzw')









# Flow in/out of farm types  ---------------------------------------------------
#make empty tibble to store results
flow_population = tibble()

# Start loop to track flow in each farm type ()
for (type in 1:6){
  # set farm type
  Farm_Type = c(unique(info_all_farms$Farm_type))[type]
  # Select data involving this type
  farm_data= subset(move.data, SENDER_FARMTYPE == Farm_Type | RECEIVER_FARMTYPE == Farm_Type)
  # Sort by date
  farm_data <- farm_data[order(farm_data$DATE_MOVEMENT, decreasing = F),] 
  
  # Group by farm ID and farm type and calculate the total number of pigs on each farm
  start_Pop = distinct(rbind(farm_data %>% distinct("Farmid" = FARMID_SENDER, "Farm_type" = SENDER_FARMTYPE, "N_pigs" = SENDER_N_PIGS),
                             farm_data %>% distinct("Farmid" = FARMID_RECEIVER, "Farm_type" = RECEIVER_FARMTYPE, "N_pigs" = RECEIVER_N_PIGS) %>% 
                               filter(Farm_type == Farm_Type))) %>% summarise(total_n_pigs = sum(N_pigs, na.rm = T))
  # dates
  all_dates = unique(farm_data$DATE_MOVEMENT) 
  first_date = all_dates[1] - days(1)
  
  # Create tibble to store population size over time
  flow_pop = tibble(date = first_date, TypeFarm = Farm_Type, population = start_Pop$total_n_pigs, n_in = 0, n_out = 0, net_flow = 0)
  
  for (day in 1:length(all_dates)){
    moves = farm_data %>% filter(DATE_MOVEMENT == all_dates[day])
    flow_out = moves %>%  filter(SENDER_FARMTYPE == Farm_Type) %>% summarise(n_out = sum(N_MOVED_PIGS, na.rm = T))
    flow_in = moves %>% filter(RECEIVER_FARMTYPE == Farm_Type) %>% summarise(n_in = sum(N_MOVED_PIGS, na.rm = T))
    flow_pop = flow_pop %>% add_row(date = all_dates[day],
                                    TypeFarm = Farm_Type,
                                    population = flow_pop$population[day] - flow_out$n_out + flow_in$n_in,
                                    n_in = flow_in$n_in,
                                    n_out = flow_out$n_out,
                                    net_flow = n_in - n_out)
  }
  
  flow_population = rbind(flow_population, flow_pop) 
} 


# Netto flow
ggplot(data = flow_population, aes(x = date, y = net_flow, color = TypeFarm)) +
  #geom_line() +
  geom_point() + 
  facet_wrap(~TypeFarm, scales="free_y",
             labeller = labeller(TypeFarm = 
                                   c("15 11 Svin, produktionsbesætning" = "15 11 Production", 
                                     "15 13 Økologisk svinebesætning" = "15 13 Organic", 
                                     "15 17 Frilandssvinebesætning" = "15 17 Free-range", 
                                     "15 41 Svin, avls- og opformeringsbesætn." = "15 41 Breeding & Multiplication", 
                                     "15 46 Smågriseopdrætsbesætning" = "15 46 Piglets", 
                                     "15 68 Svineslagteri" = "15 68 Slaughterhouse")))+
  labs(title = "Netto flow of pigs in and out of each farmtype over time",
       x = "Time",
       y = "Pigs [#]") +
  scale_color_manual(name = "Herd type",
                     values = c("#FFDAA0", "#C7E9B4" ,"#7FCDBB", "#41B6C4", "#2C7FB8","#081D58"),
                     labels = c("15 11 Production", 
                                "15 13 Organic", 
                                "15 17 Free-range", 
                                "15 41 Breeding & Multi", 
                                "15 46 Piglets",
                                "Total"))

# In flow
ggplot(data = flow_population, aes(x = date, y = n_in, color = TypeFarm)) +
  #geom_line() +
  geom_point() + 
  facet_wrap(~TypeFarm, scales="free_y") +
  labs(title = "Number of Pigs recieved by each farmtype over time",
       x = "Date",
       y = "Number Pigs")

# Out flow
ggplot(data = flow_population, aes(x = date, y = n_out, color = TypeFarm)) +
  #geom_line() +
  geom_point() + 
  facet_wrap(~TypeFarm, scales="free_y") +
  labs(title = "Number of Pigs sent by each farmtype over time",
       x = "Date",
       y = "Number Pigs")


# Population size for each farm over time
ggplot(data = flow_population, aes(x = date, y = population, color = TypeFarm)) +
  geom_line() +
  geom_point() + 
  facet_wrap(~TypeFarm, scales="free_y") +
  labs(title = "Population size for each farm over time",
       x = "Date",
       y = "Number Pigs")

# Mean of netto flow for each farm
mean_netflow <- flow_population %>% group_by(TypeFarm) %>% summarise(mean_flow = mean(net_flow),
                                                                     median_flow = median(net_flow),
                                                                     sd_flow = sd(net_flow),
                                                                     var_flow = var(net_flow))
farmtypes = c(unique(mean_netflow$TypeFarm))
coLors = c("#FFDAA0", "#C7E9A4" ,"#7FCDBB", "#41B6C4", "#2C7FB8","#081D58")
type_names = c("15 11 Production", "15 13 Organic",  "15 17 Free-range",  "15 41 Breeding & Multi","15 46 Piglets", "15 68 Slaughter")

type = 6
p6 = flow_population %>% 
  filter(TypeFarm == farmtypes[type]) %>% 
  ggplot(aes(x = date, y = net_flow)) +
  #geom_line(color = coLors[type]) +
  geom_point(color = coLors[type]) +
  geom_hline(yintercept = mean_netflow$median_flow[type], alpha = 0.9) +
  geom_hline(yintercept = mean_netflow$median_flow[type] + 1*mean_netflow$sd_flow[type], linetype="dashed", color = "black", alpha = 0.75) +
  geom_hline(yintercept = mean_netflow$median_flow[type] - 1*mean_netflow$sd_flow[type], linetype="dashed", color = "black", alpha = 0.75) +
  labs(title = paste(type_names[type]),
       y = "Netto flow",
       x = "Time")

grid.arrange(p1, p6, p4, p5, p2, p3, ncol = 2, nrow = 3)



# FLow of population over time --------------------------------------------





# Unique monthly contacts -------------------------------------------------
# Plot the unique contacts pr month pr farm 
tt = readRDS("../Data/DataAnalysis/Tot_unique_contacts.rds")
tt$farmId = as.character(tt$farmId)
tt = tt %>% mutate(Date = as.Date(paste0("01-", Month), format = "%d-%b-%y")) %>% arrange(Date)

# library(virdis)
# ggplot(data = tt, aes(x = Date, y = Unique_contacts, group = farmId, color = farmId)) + 
#   geom_line()  +
#   #scale_y_log10() + 
#   scale_color_viridis(option = "H",discrete = T) +
#   theme(legend.position = "none") +
#   labs(title = "Monthly unique contacts with other farm",
#        x = "Time",
#        y = "Number of unique contacts")


# Create the plot using the custom color palette
ggplot(data = tt, aes(x = Date, y = Unique_contacts, group = farmId, color = farmId)) + 
  geom_line() +
  scale_color_manual(values = paletteer_c("ggthemes::Blue-Teal", length(unique(tt$farmId)))) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Monthly Unique Contacts",#"Monthly unique contacts with other farm",
       x = "Time",
       y = "Unique contacts [#]")  

# Save
ggsave("../Results/UniqueMonthlyContacts.tiff", units="in", width=9, height=5, dpi=300, compression = 'lzw')




# Find farmId on the farms that in some months has over 25 unique contacts
tt_id = tt %>% filter(Unique_contacts > 25) %>% pull(farmId) %>% unique()
tt[(tt$farmId %in% tt_id),]  %>% 
  ggplot( aes(x = Date, y = Unique_contacts, group = farmId)) + 
  geom_line(aes(color = farmId)) +
  labs(title = "Monthly unique contacts -  including only farms with over 12 unique contact in any given month",
       x = "Month",
       y = "Number of unique contacts") + 
  scale_color_manual(values = paletteer_c("ggthemes::Blue-Teal", length(tt_id)))+
  theme_minimal()
#scale_y_continuous(breaks = round(seq(0, max(tt$Unique_contacts)+10, by = 10),1))  



# how to make table R -----------------------------------------------------
# How to make tables look nice in r
options(knitr.table.format = "html")
# Load the data
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyUndirecterWeighted.csv", header=T, row.names="Cities.", sep=",") %>% as.matrix
colnames(data) <- gsub("\\.", " ", colnames(data))

# show data
tmp <- data %>% as.data.frame() %>% select(1,3,6) %>% .[c(1,3,6),]
tmp[is.na(tmp)] <- "-"
tmp %>% kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

