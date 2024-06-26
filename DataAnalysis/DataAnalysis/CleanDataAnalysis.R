# Data Analysis 

# Initialize -----------------------------------------------------------
source(file.path("../Model/Initialize/Load_packages.R"))
source(file.path("../Model/Initialize/Define_Functions.R"))
rawData = FALSE # Change if you have access to row data
source(file.path("../Model/Initialize/LoadNClean_Data.R"))

# Simple counts -----------------------------------------------------------
nrow(move.data)               #number of movements including slaughter 
nrow(move.data %>% filter(RECEIVER_FARMTYPE != "15 68 Svineslagteri"))         #number of movements excluding slaughter 


# Count how many of each farmtype and how many pigs are within
a = info_all_farms %>%  group_by(Farm_type) %>% summarise(n_farms = n(),
                                                 total_n_pigs = sum(N_pigs, na.rm = T),
                                                 min_pigs = min(N_pigs, na.rm = T),
                                                 max_pigs = max(N_pigs, na.rm = T),
                                                 median_pigs = median(N_pigs, na.rm = T))


# Plot total number of pigs sent and received by each farmtype  ----------
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
  mutate(type = c("15 11","15 11","15 13","15 13","15 17","15 17","15 41","15 41","15 46","15 46","15 68","15 68")) %>% 
  mutate(moves = replace(moves, moves == 0, NA))

# Plot bar chart 
ggplot(n_move, aes(Farm_type, x = type, y = moves, fill = movement)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_y_log10() +
  geom_text(aes(label=moves), vjust= -0.3, color="black",
            position = position_dodge(0.9), size=3.5) +
  labs(title = "Distribution of movement across herd types",
       x = "Herd type [ID]",
       y = "Number of movements") +
  scale_fill_manual(name = "Type of move",
                    values =  c("#6BAED6", "#3182BD"))
# save
ggsave("../Results/Data/MovementDistribution.tiff", units="in", width=9, height=5, dpi=300, compression = 'lzw')


# Plot weekly number of sent and received pigs  ------------------------------------------------------
movement.data$DATE_MOVEMENT <- as.Date(movement.data$DATE_MOVEMENT) 

# Group by every week starting on Saturday and sum the number of pigs moved
pigs_sent_per_week <- movement.data %>%
  group_by(week = lubridate::floor_date(DATE_MOVEMENT, "week", week_start = 6)) %>%
  summarise(Total_pigs_sent = sum(N_MOVED_PIGS[!is.na(FARMID_SENDER)]))

# Exclude the last data point
pigs_sent_per_week <- pigs_sent_per_week[-nrow(pigs_sent_per_week), ]

# Plot
ggplot(pigs_sent_per_week, aes(x = week, y = Total_pigs_sent)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Pigs Sent Each Week",
       x = "Week",
       y = "Total Pigs Sent") +
  theme_minimal()


# Group by every month and sum the number of pigs moved
pigs_sent_per_month <- movement.data %>%
  group_by(month = lubridate::floor_date(DATE_MOVEMENT, "month")) %>%
  summarise(Total_pigs_sent = sum(N_MOVED_PIGS[!is.na(FARMID_SENDER)]))
# Plot
ggplot(pigs_sent_per_month, aes(x = month, y = Total_pigs_sent)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Pigs Sent Each Month",
       x = "Month",
       y = "Total Pigs Sent") +
  theme_minimal()


# ---------------------- Weekly sending and receiving --------------------------------------------

# Group by every week starting on Saturday and sum the number of pigs moved for each farm type
pigs_sent_per_week <- movement.data %>%
  group_by(week = lubridate::floor_date(DATE_MOVEMENT, "week", week_start = 6), SENDER_FARMTYPE) %>%
  summarise(Total_pigs_sent = sum(N_MOVED_PIGS[!is.na(FARMID_SENDER)]))

# Compute total number of pigs sent per week across all farm types
total_pigs_sent_per_week <- pigs_sent_per_week %>%
  group_by(week) %>%
  summarise(Total_pigs_sent = sum(Total_pigs_sent))

# Add a row for total to the pigs_sent_per_week dataset
total_row <- total_pigs_sent_per_week %>%
  mutate(SENDER_FARMTYPE = "Total")
pigs_sent_per_week <- bind_rows(pigs_sent_per_week, total_row)

# Exclude the last data point
#pigs_sent_per_week <- pigs_sent_per_week[-nrow(pigs_sent_per_week), ]

# Plot
Sent_plot <- ggplot(pigs_sent_per_week, aes(x = week, y = Total_pigs_sent, color = SENDER_FARMTYPE, group = SENDER_FARMTYPE)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Pigs Sent Each Week by each Farm Type",
       x = "Date",
       y = "# Pigs Sent",
       color = "Farm Type") +
  theme_minimal()


## Zooming in on økologisk, friland, avl og smågriseopræt bessætninger
pigs_send_zoom1 <- pigs_sent_per_week %>% filter(SENDER_FARMTYPE != "15 11 Svin, produktionsbesætning" & SENDER_FARMTYPE != "Total") 

pigs_send_zoom2 <- pigs_sent_per_week %>% filter(SENDER_FARMTYPE != "15 11 Svin, produktionsbesætning") %>% 
  mutate("Total_pigs_sent" = ifelse(SENDER_FARMTYPE == "Total", Total_pigs_sent/50, Total_pigs_sent))

send_zoom_plot <- ggplot(pigs_send_zoom1, aes(x = week, y = Total_pigs_sent, color = SENDER_FARMTYPE, group = SENDER_FARMTYPE)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Pigs Sent Each Week by selected Farm Types",
       x = "Date",
       y = "# Pigs Sent",
       color = "Farm Type") +
  theme_minimal()


### Receivings per week
pigs_receive_per_week <- movement.data %>%
  group_by(week = lubridate::floor_date(DATE_MOVEMENT, "week", week_start = 6), RECEIVER_FARMTYPE) %>%
  summarise(Total_pigs_receive = sum(N_MOVED_PIGS[!is.na(FARMID_RECEIVER)]))

# Compute total number of pigs receive per week across all farm types
total_pigs_receive_per_week <- pigs_receive_per_week %>%
  group_by(week) %>%
  summarise(Total_pigs_receive = sum(Total_pigs_receive))

# Add a row for total to the pigs_sent_per_week dataset
total_row <- total_pigs_receive_per_week %>%
  mutate(RECEIVER_FARMTYPE = "Total")
pigs_receive_per_week <- bind_rows(pigs_receive_per_week, total_row)

receive_plot <- ggplot(pigs_receive_per_week, aes(x = week, y = Total_pigs_receive, color = RECEIVER_FARMTYPE, group = RECEIVER_FARMTYPE)) +
  geom_line() +
  geom_point() +
  labs(title = "Pigs received Each Week by each Farm Type",
       x = "Date",
       y = "# Pigs received",
       color = "Farm Type") +
  theme_minimal()

## Zooming in on økologisk, friland, avl og smågriseopræt bessætninger
pigs_receive_zoom1 <- pigs_receive_per_week %>% filter(RECEIVER_FARMTYPE != "15 11 Svin, produktionsbesætning" & 
                                                         RECEIVER_FARMTYPE != "Total" &
                                                         RECEIVER_FARMTYPE != "15 68 Svineslagteri") 

receive_zoom_plot <- ggplot(pigs_receive_zoom1, aes(x = week, y = Total_pigs_receive, color = RECEIVER_FARMTYPE, group = RECEIVER_FARMTYPE)) +
  geom_line() +
  geom_point() +
  labs(title = "Pigs Received Each Week by selection of farm types",
       x = "Date",
       y = "# Pigs Received",
       color = "Farm Type") +
  theme_minimal()


grid.arrange(Sent_plot, receive_plot, ncol = 1, nrow = 2)              # All farmtypes   
grid.arrange(send_zoom_plot, receive_zoom_plot, ncol = 1, nrow = 2).   # Selected farmtypes; økologisk, friland, avl og smågriseopræt




# ----------------------- Monthly sending and receivings ------------------------------------------
movement.data = move.data
# compute number of pigs sent monthly across the farm types 
pigs_sent_per_month <- movement.data %>%
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
  labs(title = "Number of Pigs Sent Each Week by each Farm Type",
       x = "Date",
       y = "# Pigs Sent",
       color = "Farm Type") +
  theme_minimal()

## Zooming in on økologisk, friland, avl og smågriseopræt bessætninger
pigs_send_zoom1_m <- pigs_sent_per_month %>% filter(SENDER_FARMTYPE != "15 11 Svin, produktionsbesætning" & SENDER_FARMTYPE != "Total") 

pigs_send_zoom2_m <- pigs_sent_per_month %>% filter(SENDER_FARMTYPE != "15 11 Svin, produktionsbesætning") %>% 
  mutate("Total_pigs_sent" = ifelse(SENDER_FARMTYPE == "Total", Total_pigs_sent/50, Total_pigs_sent))

send_zoom_plot_m <- ggplot(pigs_send_zoom1_m, aes(x = month, y = Total_pigs_sent, color = SENDER_FARMTYPE, group = SENDER_FARMTYPE)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Pigs Sent Each Month",
       x = "Date",
       y = "# Pigs Sent",
       color = "Farm Type") +
  theme_minimal()




# Distribution of sendings ------------------------------------------------
# Including zero counts
dist0 <- movement.data %>%
  complete(SENDER_FARMTYPE, RECEIVER_FARMTYPE, fill = list(Npigs = 0)) %>%
  group_by(SENDER_FARMTYPE, RECEIVER_FARMTYPE) %>% 
  summarise(Npigs = sum(N_MOVED_PIGS)) %>% mutate_all( ~replace_na(.,0))

# Including only positive counts
dist1 <- movement.data %>%
  group_by(SENDER_FARMTYPE, RECEIVER_FARMTYPE) %>% 
  summarise(Npigs = sum(N_MOVED_PIGS)) 

#Positive counts but exclude sent pigs from production as they overpower
dist2 <- subset(dist1, SENDER_FARMTYPE != "15 11 Svin, produktionsbesætning")

# Plot distribution of sendings from each farmtype in one plot
ggplot(data=dist1, aes(x=SENDER_FARMTYPE, y=Npigs, fill=RECEIVER_FARMTYPE)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=Npigs), vjust= -0.3, color="black",
            position = position_dodge(0.9), size=3)

# Plotting using facet_wrap the distribution of sent pigs from each framtype in every plot
ggplot(data=dist1, aes(x=RECEIVER_FARMTYPE, y=Npigs, fill=RECEIVER_FARMTYPE)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Npigs), vjust= -0.3, color="black",
            position = position_dodge(0.9), size=3) +
  facet_wrap(~SENDER_FARMTYPE, scales="free_y", ncol = 2) +
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank())

###### Flow in and out of farms #####
# Flow in single farm -----------------------------------------------------
Farm = farmIDs[sample(1:length(farmIDs), 1)] #Pick random farm  #2336572565 el 2336575352, 2336629691, 2336654323, 2336629266
## Initialize
# Subset data to only include movement involving this farm
farm.data = subset(movement.data, FARMID_SENDER == Farm | FARMID_RECEIVER == Farm)
# Sort data by date, increasing from start date
farm.data <- farm.data[order(farm.data$DATE_MOVEMENT, decreasing = F),] 
farmType = info_all_farms %>% filter(Farmid == Farm) %>% pull(Farm_type)
# Find a start date (one day before first sent/receive)
first.date = farm.data$DATE_MOVEMENT[1] - days(1)
# Find start population of the farm
start.pop = info_all_farms %>% filter(Farmid == Farm) %>% pull(N_pigs)
# Create df to store the in and out flow on the farm
flow = tibble(Date = first.date, Population = start.pop)

#Update flow substract for sendings and add for receivings
for (move in 1:nrow(farm.data)){
  flow = flow %>% add_row(Date = farm.data$DATE_MOVEMENT[move], 
                          Population = ifelse(farm.data$FARMID_SENDER[move] == Farm, 
                                              flow$Population[move] - farm.data$N_MOVED_PIGS[move],
                                              flow$Population[move] + farm.data$N_MOVED_PIGS[move]))
}

p28 = ggplot(data = flow, aes(x = Date, y = Population)) +
  geom_line() +
  geom_point() +
  labs(#title = paste("Flow within farm", Farm, ", ", farmType),
       x = "Date",
       y = "Total pig population") +
  theme_minimal()

# How many pigs exactly in and out
paste( "Gård ", Farm, ",", farmType, ":")
paste("Grise sendt: ", movement.data %>% filter(FARMID_SENDER == Farm) %>% summarise(sum(N_MOVED_PIGS)))
paste("Grise modtaget: ",movement.data %>% filter(FARMID_RECEIVER == Farm) %>% summarise(sum(N_MOVED_PIGS)))

grid.arrange(p1,p7,p3,p4,p22,p6,p2,p8,p9,p10,p11,p21,p13,p14,p15,p16,p26,p18,p19,p5,p20,p12,p23,p24,p25,p17,p27,p28, ncol = 4)

install.packages("gridExtra")
install.packages("ggplot2")  # Assuming you're using ggplot2 for the plots
# The grid package is part of base R, so you don't need to install it, but you do need to load it.
library(gridExtra)
library(grid)  # This is necessary for textGrob
library(ggplot2)
y_axis_grob <- textGrob("Total population [#]", gp = gpar(fontsize = 15), rot = 90)
x_axis_grob <- textGrob("Time", gp = gpar(fontsize = 15))

# Combine the plots in a grid
combined_plot <- grid.arrange(
  arrangeGrob(
    grobs = list(p1, p7, p3, p4, p22,
                 p6, p2, p8, p9, p10,
                 p11, p21, p13, p14, p15,
                 p16, p26, p18, p19, p5,
                 p20, p12, p23, p24, p25,
                 p17, p27, p28),
    ncol = 4
  ),
  left = y_axis_grob,
  bottom = x_axis_grob
)

# Display the combined plot
print(combined_plot)




# Flow in farm types  ---------------------------------------------------
#make empty tibble to store results
flow_population = tibble()

# Start loop to track flow in each farm type ()
for (type in 1:6){
  # set farm type
  Farm_Type = c(unique(all_farms$Farm_type))[type]
  # Select data involving this type
  farm_data= subset(movement.data, SENDER_FARMTYPE == Farm_Type | RECEIVER_FARMTYPE == Farm_Type)
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
  geom_line() +
  geom_point() + 
  facet_wrap(~TypeFarm, scales="free_y")+
  labs(title = "Netto flow of pig in and out of each farmtype over time",
       x = "Date",
       y = "Number Pigs")

# In flow
ggplot(data = flow_population, aes(x = date, y = n_in, color = TypeFarm)) +
  geom_line() +
  geom_point() + 
  facet_wrap(~TypeFarm, scales="free_y") +
  labs(title = "Number of Pigs recieved by each farmtype over time",
       x = "Date",
       y = "Number Pigs")

# Out flow
ggplot(data = flow_population, aes(x = date, y = n_out, color = TypeFarm)) +
  geom_line() +
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
                                                     sd_flow = sd(net_flow),
                                                     var_flow = var(net_flow))
farmtypes = c(unique(mean_netflow$TypeFarm))
coLors = c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3")

type = 5
p5 = flow_population %>% 
    filter(TypeFarm == farmtypes[type]) %>% 
    ggplot(aes(x = date, y = net_flow)) +
    geom_line(color = coLors[type]) +
    geom_point(color = coLors[type]) +
    geom_hline(yintercept = mean_netflow$mean_flow[type]) +
    geom_hline(yintercept = mean_netflow$mean_flow[type] + mean_netflow$sd_flow[type], linetype="dashed", color = "black") +
    geom_hline(yintercept = mean_netflow$mean_flow[type] - mean_netflow$sd_flow[type], linetype="dashed", color = "black") +
    labs(title = paste(farmtypes[type]))

grid.arrange(p1, p2, p3, p4, p5, ncol = 3, nrow = 2)

  
