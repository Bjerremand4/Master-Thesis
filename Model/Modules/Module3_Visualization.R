# -------------------------------------------------------------------------
#                           Visualizations  
# -------------------------------------------------------------------------


# Plotting for Module 2a Test at Slaughter ------------------------------------
# Setting non detected 
max_days = round(max(Farms_result$discover_days) + (0.45*max(Farms_result$discover_days) ), -2)
max_prop = round(max(Farms_result$Propagation) + (0.4*max(Farms_result$Propagation) ), -1)

Farms_result$discover_days[Farms_result$discover_days > (as.numeric(difftime(as.Date("2023-01-01"), as.Date("2020-02-01")))-1)] = max_days
Farms_result$Propagation[Farms_result$discover_days > (as.numeric(difftime(as.Date("2023-01-01"), as.Date("2020-02-01")))-1)] = max_prop
# Define movement frequency
Farms_result$m_freq = paste0(move_freq,"%")

# Plotting propagation
p_prop_freq = ggplot(Farms_result, aes(x = m_freq, y=Propagation, fill = m_freq)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(data = Farms_result %>% filter(Propagation < max_prop),
                    aes(x = m_freq, y = Propagation, 
                        fill = m_freq), side = "l") +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "Number of infected farms at detection",
       x = "",
       y = "Infected Farms [#]") +  
  scale_x_discrete(labels = c("")) +
  scale_y_continuous(labels = function(x) ifelse(x == max_prop, "not detected", x))

# Plotting days
p_day_freq = ggplot(Farms_result, aes(x = m_freq, y=discover_days, fill = m_freq)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(data = Farms_result %>% filter(discover_days < max_days),
                    aes(x = m_freq, y = discover_days, 
                        fill = m_freq), side = "l") +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "Number of days before detection",
       x = "",
       y = "Detection time [Days]") +
  scale_y_continuous(labels = function(x) ifelse(x == max_days, "not detected", as.character(x))) +
  scale_x_discrete(labels = c("")) 


# Plots distributed by type
p_prop_types = ggplot(Farms_result, aes(x = Farmtype, y=Propagation, fill = Farmtype)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(data = Farms_result %>% filter(Propagation < max(Farms_result$Propagation)),
                    aes(x = Farmtype, y = Propagation, 
                        fill = Farmtype), side = "l") +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "Number of infected farms at detection",
       x = "Farm Type",
       y = "Infected Farms [#]") +
  scale_x_discrete(labels = c("15 11","15 13","15 17","15 41","15 46")) +
  scale_y_continuous(labels = function(x) ifelse(x == max(Farms_result$Propagation), "not detected", x))

p_day_types = ggplot(Farms_result, aes(x = Farmtype, y=discover_days, fill = Farmtype)) + 
  geom_half_violin(side = "r", alpha = 1) +
  geom_half_boxplot(data = Farms_result %>% filter(discover_days < max(Farms_result$discover_days)),
                    aes(x = Farmtype, y = discover_days, 
                        fill = Farmtype), side = "l") +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "Number of days before detection",
       x = "Farm Type",
       y = "Detection time [Days]") +
  scale_x_discrete(labels = c("15 11","15 13","15 17","15 41","15 46")) +
  scale_y_continuous(labels = function(x) ifelse(x == max(Farms_result$discover_days), "not detected", x))

# Thext with parameters
text_output <- ggdraw() + 
  draw_label(
    paste0(
      "Parameters used:\n",
      "move_freq = ", move_freq, "%\n",
      "b = ", b, "\n",
      "n_test = ", n_test, "\n",
      "test_sensitivity = ", test_sensitivity
    ),
    size = 12
  )

# Combine the plot and text output side by side
TestSlaughter_General = plot_grid(p_prop_freq, p_day_freq, text_output, ncol = 3, rel_widths = c(3,3, 1))
TestSlaughter_By_FarmType = plot_grid(p_prop_types, p_day_types, text_output, ncol = 3, rel_widths = c(3,3, 1))

# TestSlaughter_General = grid.arrange(p_prop_freq, p_day_freq, ncol = 2)
# TestSlaughter_By_FarmType = grid.arrange(p_prop_types, p_day_types, ncol = 2)

ggsave("../Results/OutputModel/TestSlaughter_General.tiff", plot = TestSlaughter_General, units="in", width=20, height=8, dpi=300,compression = 'lzw')
ggsave("../Results/OutputModel/TestSlaughter_By_FarmType.tiff", plot = TestSlaughter_By_FarmType, units="in", width=20, height=8, dpi=300,compression = 'lzw')



# Plotting for Module 2b Visual Inspection --------------------------------
if (pathogen == T){
  Detection_result$m_freq = "100%"
  
  p_prop_freq_b = ggplot(Detection_result, aes(x = m_freq, y=propagation, fill = m_freq)) + 
    geom_half_violin(side = "r", alpha = 1) +
    geom_half_boxplot(side = "l", alpha = 1) +
    theme(legend.position="none") +
    scale_fill_brewer(palette = "Paired") + 
    labs(title = "",
         x = "",
         y = "Infected Farms [#]") +  
    scale_x_discrete(labels = c("")) 
  
  # Plotting days
  p_day_freq_b = ggplot(Detection_result, aes(x = m_freq, y=days_since_intro, fill = m_freq)) + 
    geom_half_violin(side = "r") +
    geom_half_boxplot(side = "l") +
    theme(legend.position="none") +
    scale_fill_brewer(palette = "Paired") + 
    labs(title = "",
         x = "",
         y = "Detection time [Days]") +
    scale_x_discrete(labels = c(""))
  
  p_prop_freq_a = ggplot(Farms_result, aes(x = m_freq, y=Propagation, fill = m_freq)) + 
    geom_half_violin(side = "r", alpha = 1) +
    geom_half_boxplot(data = Farms_result %>% filter(Propagation < max_prop),
                      aes(x = m_freq, y = Propagation, 
                          fill = m_freq), side = "l") +
    theme(legend.position="none") +
    scale_fill_brewer(palette = "Set3") + 
    labs(title = "",
         x = "",
         y = "Infected Farms [#]") +  
    scale_x_discrete(labels = c("")) +
    scale_y_continuous(labels = function(x) ifelse(x == max_prop, "not detected", x))
  
  # Plotting days
  p_day_freq_a = ggplot(Farms_result, aes(x = m_freq, y=discover_days, fill = m_freq)) + 
    geom_half_violin(side = "r", alpha = 1) +
    geom_half_boxplot(data = Farms_result %>% filter(discover_days < max_days),
                      aes(x = m_freq, y = discover_days, 
                          fill = m_freq), side = "l") +
    theme(legend.position="none") +
    scale_fill_brewer(palette = "Set3") + 
    labs(title = "",
         x = "",
         y = "Detection time [Days]") +
    scale_y_continuous(labels = function(x) ifelse(x == max_days, "not detected", as.character(x))) +
    scale_x_discrete(labels = c(""))
  
  
  VisualInspection_General =  plot_grid(p_prop_freq_b, p_day_freq_b, 
            labels = c("Visual Inspection"),
            label_size = 16)
  
  Visual_and_test = plot_grid(grid.arrange(p_prop_freq_b, p_day_freq_b, ncol = 2), 
            grid.arrange(p_prop_freq_a, p_day_freq_a, ncol = 2), 
            labels = c("Visual Inspection", "Test at Slaughter"),
            #rows = 2,
            label_size = 16)
  
  ggsave("../Results/OutputModel/VisualInspection_General.tiff", plot = VisualInspection_General, units="in", width=15, height=6, dpi=300,compression = 'lzw')
  ggsave("../Results/OutputModel/BothTests.tiff", plot = Visual_and_test, units="in", width=15, height=12, dpi=300,compression = 'lzw')
  
}



