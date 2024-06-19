# -------------------------------------------------------------------------
#                           Visualizations  
# -------------------------------------------------------------------------

b = 0.05
n_test = 5
test_sensitivity = 0.85
move_freq = 33

# Load and prep data  -----------------------------------------------------
Farms_result = readRDS(file.path(dirname(getwd()),paste0("Data/2a_TestSlaughter/TestSlaughter",move_freq, "%_beta", b,"_sample", n_test, "_sens", test_sensitivity, ".rds")))
Detection_result = readRDS(file.path(dirname(getwd()),paste0("Data/2b_VisualInspection/Detect_beta", b, "move_",move_freq,"%.rds")))

# Setting non detected to max + max*0.5
max_days = round(max(Farms_result$discover_days) + (0.5*max(Farms_result$discover_days)), -2)
Farms_result$discover_days[Farms_result$discover_days > (as.numeric(difftime(as.Date("2023-01-01"), as.Date("2020-02-01")))-1)] = max_days
Detection_result$days_since_intro[Detection_result$days_since_intro >= (as.numeric(difftime(as.Date("2023-01-01"), as.Date("2020-02-01")))-1)] = max_days

# Adding movement_frequemcy and detect/non-detect
Farms_result$m_freq = paste0(move_freq,"%")
Farms_result = Farms_result %>% mutate(Status = ifelse(discover_days == max_days, "Not detected", "Detected"))
Detection_result$m_freq = "100%"

# Create the General text output statng the used parameters
# Create the formatted text
formatted_text <- paste0(
  "move_freq        = ", move_freq, "%\n",
  "b                = ", b, "\n",
  "n_test           = ", n_test, "\n",
  "test_sensitivity = ", test_sensitivity, "%"
)

# Create a text grob with bold header and monospaced parameters
text_grob <- grobTree(
  textGrob(
    label = "Parameters used:",
    x = unit(0.05, "npc"),
    y = unit(0.95, "npc"),
    just = c("left", "top"),
    gp = gpar(fontface = "bold", fontsize = 12)
  ),
  textGrob(
    label = formatted_text,
    x = unit(0.05, "npc"),
    y = unit(0.9, "npc"),
    just = c("left", "top"),
    gp = gpar(fontfamily = "mono", fontsize = 12)
  )
)

# Draw the grob using ggdraw
text_output <- ggdraw() + draw_grob(text_grob) + 
  theme(panel.background = element_rect(fill = "white", color = NA)) 

# # Thext with parameters
# text_output <- ggdraw() + 
#   draw_label(
#     paste0(
#       "Parameters used:\n",
#       "move_freq = ", move_freq, "%\n",
#       "b = ", b, "\n",
#       "n_test = ", n_test, "\n",
#       "test_sensitivity = ", test_sensitivity
#     ),
#     size = 12,
#     hjust = 0, # Left-align the text
#     x = 0.05,  # Slightly adjust the x position to avoid clipping
#     vjust = 1, # Top-align the text
#     y = 0.95
#   )

colors = c("#A6CEE3", "#1F78B4" ,"#B2DF8A" ,"#33A02C" ,"#FB9A99" ,"#E31A1C")


# Plotting for Module 2a Test at Slaughter ------------------------------------
p_prop_freq = ggplot(Farms_result, aes(x = m_freq, y=Propagation)) + 
  geom_half_violin(side = "r", alpha = 1,fill = colors[2]) +
  geom_half_boxplot(side = "l", alpha = 1, fill = colors[2]) +
  geom_point(data = Farms_result %>% filter(discover_days > 1064),
             aes(x = m_freq, y = Propagation, color = "Not Detected"), # Use a placeholder for the color
             size = 0.9, shape = 21, fill = "goldenrod3") +
theme(panel.background = element_rect(fill = "white", color = NA)) +
  scale_color_manual(name = "", values = c("Not Detected" = "goldenrod3")) +
  #scale_y_log10() +
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "",
       x = "",
       y = "Infected Farms [#]") +
  theme(legend.position = "top", 
        axis.text.x = element_blank()) + 
  guides(fill = "none")
    
    

# # Plotting propagation
# p_prop_freq1 = ggplot(Farms_result, aes(x = Status, y=Propagation, fill = Status)) + 
#   geom_half_violin(side = "r", alpha = 1, scale = 3) +
#   geom_half_boxplot(side = "l", alpha = 1) +
#   theme_minimal() +
#   theme(legend.position="none") +
#   scale_fill_brewer(palette = "Paired") + 
#   labs(title = "Number of infected farms at detection",
#        x = "",
#        y = "Infected Farms [#]") 
# 


# Plotting days
p_day_freq = ggplot(Farms_result, aes(x = m_freq, y=discover_days, fill = m_freq)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(data = Farms_result %>% filter(discover_days < max_days),
                    aes(x = m_freq, y = discover_days, 
                        fill = m_freq), side = "l") +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Paired") + 
  labs(title = "",
       x = "",
       y = "Detection time [Days]") +
  scale_y_continuous(breaks = seq(0, max_days, length.out = 6),
                     labels = function(x) ifelse(x == max_days, "Not Detected", as.character(x))) +
  scale_x_discrete(labels = c("")) 


# Plots distributed by type
p_prop_types = ggplot(Farms_result, aes(x = Farmtype, y=Propagation, fill = Farmtype)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(side = "l", alpha = 1) +
  geom_point(data = Farms_result %>% filter(discover_days > 1064),
             aes(x = Farmtype, y = Propagation, color = "Not Detected"), # Use a placeholder for the color
             size = 0.9, shape = 21, fill = "goldenrod3") +
  theme(panel.background = element_rect(fill = "white", color = NA)) + 
  scale_color_manual(name = "", values = c("Not Detected" = "goldenrod3")) +
  #scale_y_log10() +
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "",
       x = "Farm Type",
       y = "Infected Farms [#]") +
  scale_x_discrete(labels = c("Production","Organic","Free-Range","Breeding+Multiplication","Piglets")) +
  theme(legend.position = "top") + 
  guides(fill = "none")
  

p_day_types = ggplot(Farms_result, aes(x = Farmtype, y=discover_days, fill = Farmtype)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(data = Farms_result %>% filter(discover_days < max(Farms_result$discover_days)),
                    aes(x = Farmtype, y = discover_days, 
                        fill = Farmtype), side = "l") +
  theme(panel.background = element_rect(fill = "white", color = NA))  +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "",
       x = "Farm Type",
       y = "Detection time [Days]") +
  scale_x_discrete(labels = c("Production","Organic","Free-Range","Breeding+Multiplication","Piglets")) +
  scale_y_continuous(breaks = seq(0, max_days, length.out = 6),
                     labels = function(x) ifelse(x == max_days, "Not Detected", as.character(x))) 

# Combine the plot and text output side by side
TestSlaughter_General = plot_grid(p_prop_freq, p_day_freq, text_output, labels = c("Popagation at Detection", "Time of Detection",""),
                                  ncol = 3)
TestSlaughter_By_FarmType <- plot_grid(p_prop_types, text_output, p_day_types,
                                       labels = c("Popagation at Detection", "", "Time of Detection"),
                                       ncol = 2, rel_widths = c(2,1))

ggsave("../Results/OutputModel/TestSlaughter_Result.png", plot = TestSlaughter_General, units="in", width=20, height=8, dpi=300)
ggsave("../Results/OutputModel/TestSlaughter_By_FarmType.png", plot = TestSlaughter_By_FarmType, units="in", width=20, height=8, dpi=300)

print(TestSlaughter_General)


# Plotting for Module 2b Visual Inspection only if agent caurse symptoms--------------------------------
if (pathogen == T){
  
  combined_result <- bind_rows(
    Farms_result %>% select(discover_days, Propagation) %>% mutate(Test_type = "TestSlaughter"),
    Detection_result %>% select(days_since_intro, propagation) %>% rename(discover_days = days_since_intro, Propagation = propagation) %>% mutate(Test_type = "VisualInspection")
  )
  # Plot
  p_prop_freq_b = ggplot(Detection_result, aes(x = m_freq, y=propagation)) + 
    geom_half_violin(side = "r", alpha = 1,fill = colors[2]) +
    geom_half_boxplot(side = "l", alpha = 1, fill = colors[2]) +
    geom_point(data = Detection_result %>% filter(days_since_intro >= 1064),
               aes(x = m_freq, y = propagation, color = "Not Detected"), # Use a placeholder for the color
               size = 0.9, shape = 21, fill = "goldenrod3") +
    theme(panel.background = element_rect(fill = "white", color = NA))  + 
    scale_color_manual(name = "", values = c("Not Detected" = "goldenrod3")) +
    #scale_y_log10() +
    scale_fill_brewer(palette = "Blues") + 
    labs(title = "",
         x = "",
         y = "Infected Farms [#]") +
    theme(legend.position = "top", 
          axis.text.x = element_blank()) + 
    guides(fill = "none")
  
  # Plotting days
  p_day_freq_b = ggplot(Detection_result, aes(x = m_freq, y=days_since_intro, fill = m_freq)) + 
    geom_half_violin(side = "r") +
    geom_half_boxplot(side = "l") +
    theme(panel.background = element_rect(fill = "white", color = NA))  +
    theme(legend.position="none") +
    scale_fill_brewer(palette = "Paired") + 
    labs(title = "",
         x = "",
         y = "Detection time [Days]") +
    scale_y_continuous(breaks = seq(0, ifelse(max(Detection_result$days_since_intro) == max_days, max_days, ceiling(max(Detection_result$days_since_intro)/10)*10), length.out = 6),
                       labels = function(x) ifelse(x == max_days, "Not Detected", as.character(x))) +
    scale_x_discrete(labels = c("")) 
  
  
  # Plotting the two test methods together: 
  # Plot
  p_prop_both = ggplot(combined_result, aes(x = Test_type, y=Propagation, fill = Test_type)) + 
    geom_half_violin(side = "r", alpha = 1) +
    geom_half_boxplot(side = "l", alpha = 1) +
    geom_point(data = combined_result %>% filter(discover_days >= 1064),
               aes(x = Test_type, y = Propagation, color = "Not Detected"), # Use a placeholder for the color
               size = 0.9, shape = 21, fill = "goldenrod3") +
    theme(panel.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray90"))  + 
    scale_color_manual(name = "", values = c("Not Detected" = "goldenrod3")) +
    #scale_y_log10() +
    scale_fill_brewer(palette = "Paired") + 
    labs(title = "",
         x = "Test Type",
         y = "Infected Farms [#]") +
    theme(legend.position = "top") + 
    guides(fill = "none")
  
  # Plotting days
  p_day_both = ggplot(combined_result, aes(x = Test_type, y=discover_days, fill = Test_type)) + 
    geom_half_violin(side = "r", scale = 3) +
    geom_half_boxplot(data = combined_result %>% filter(discover_days < max_days),
                      aes(x = Test_type, y = discover_days, 
                          fill = Test_type), side = "l") +
    theme(panel.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray90"))  +
    theme(legend.position="none") +
    scale_fill_brewer(palette = "Paired") + 
    labs(title = "",
         x = "Test Type",
         y = "Detection time [Days]") +
    scale_y_continuous(breaks = seq(0, max_days, length.out = 6),
                       labels = function(x) ifelse(x == max_days, "Not Detected", as.character(x))) 
  
  

  
  
                     
  # Display plots                   
  VisualInspection_Result =  plot_grid(p_prop_freq_b, p_day_freq_b, text_output, 
            labels = c("Propagation at Detection","Time of Detection", ""),
            ncol = 3, rel_widths = c(2,2,1))
  
  All_test_Result = plot_grid(p_prop_both, p_day_both, text_output, 
            labels = c("Propagations at Detection", "Time of Detection",""),
            ncol = 3,rel_widths = c(2,2,1) )

  
  ggsave("../Results/OutputModel/VisualInspection_Result.png", plot = VisualInspection_Result, units="in", width=15, height=10, dpi=300)
  ggsave("../Results/OutputModel/BothTests_Results.png", plot = All_test_Result, units="in", width=15, height=10, dpi=300)
  
  print(All_test_Result)
}

# Transmission 
transmission.data = readRDS(file.path(dirname(getwd()), paste0("Data/1_Transmission/trans_move",move_freq,"%_beta", b, ".rds")))

# Find length of each transmission chain
trans_lengths <- tibble(
  ID = herdID, 
  MaxTrans = sapply(transmission.data, function(x) nrow(x[[2]]))
)
plot_herd = trans_lengths$ID[which(trans_lengths$MaxTrans == max(trans_lengths$MaxTrans))]
plot_type = info_all_farms %>% filter(Farmid == plot_herd) %>% pull(Farm_type)


trans_chain = transmission.data[[as.character(plot_herd)]][[2]]
cumulative_infections <- trans_chain %>%
  group_by(date_of_infection) %>%
  summarise(daily_infections = n()) %>%
  mutate(cumulative_infections = cumsum(daily_infections))


trans_chain_plot = ggplot(cumulative_infections, aes(x = date_of_infection, y = cumulative_infections)) +
  geom_line() +
  # Optionally, add labels and customize the plot
  labs(x = "Date", y = "Total Number of Infected Herds", title = paste0("The largest infection chain, herdID: ", plot_herd, ", " , plot_type)) +
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray90")) 

ggsave("../Results/OutputModel/Largest_transmission_chain.png", plot = trans_chain_plot, width=20, height=8, dpi=300)
