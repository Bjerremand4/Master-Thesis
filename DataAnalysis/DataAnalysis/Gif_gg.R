# Load packagers
library(magick)
library(ggplot2)
library(dplyr)
library(tidyr)
library(statnet)
library(scales)
library(RColorBrewer)

# Load data 
# Full data
movement.data <- readRDS("../Data/Raw/movement_farm_2022.RDS")

# removing all slaughter entries and farms with less than 100 pigs
move.data <- subset(movement.data, RECEIVER_FARMTYPE != "15 68 Svineslagteri") %>% 
  filter(SENDER_N_PIGS > 100) %>%  filter(RECEIVER_N_PIGS > 100)


# Sort by date
move.data = move.data %>% arrange(DATE_MOVEMENT)

contact_farms_outputlist <- readRDS("../Data/dataAnalysis/contact_farms.rds")


# Extract data from a specific farm
farm_nr = 24
start_farm_ID = contact_farms_outputlist[[farm_nr]]$FarmID[1]
start_farm_type = move.data$SENDER_FARMTYPE[which(move.data$FARMID_SENDER == start_farm_ID)[1]]
start_date = move.data$DATE_MOVEMENT[1]

# select data from farm 2336641601 and clean it
net_data = contact_farms_outputlist[[farm_nr]][-1,] %>% 
  mutate(days_since_start = as.integer(contact_farms_outputlist[[farm_nr]][-1,]$date - start_date)) %>% 
  select("From" = contactFarm, "To" = FarmID, "Days_from_Start" = days_since_start)

# Create a combined id for from to 
net_data = net_data %>%rowwise() %>% mutate(samletID = paste(min(From,To), max(From,To), sep = ""))

# Keep only unique combined IDs and distinct connections
net_data = net_data %>% distinct(samletID, .keep_all = T) %>% distinct(To, .keep_all = T)

# All the unique farms included in this pattern
Nodes = unique(c(net_data$From, net_data$To))

# Cut data 
net.data = net_data 

# Network model -----------------------------------------------------------
nodes = as.character(unique(c(net.data$From, net.data$To)))
# extract the last 3 characters from each farmID 
farms_short <- substr(nodes, nchar(nodes) - 2, nchar(nodes))
# Extract days
days_start = net.data$Days_from_Start


# Create an empty matrix with specified column and row names
column_names <- nodes
row_names <- nodes
m <- matrix(NA, nrow = length(row_names), ncol = length(column_names),
            dimnames = list(row_names, column_names))

# fill in with the contacts
for (i in 1:length(nodes)){
  c = net.data$To[net.data$From == nodes[i]]
  row_i = as.integer(nodes %in% c)
  m[i,] = row_i
}






# Create network
library(GGally)
network1 = network(x = m, directed = TRUE)
ggnet2(network1)

# vertex names
network.vertex.names(network1) <- c(days_start)

# Add farm types to network
node_farmtypes = rep(NA,length(nodes))
for (i in 1:length(nodes)){
  node_farmtypes[i] = ifelse(length(which(move.data$FARMID_SENDER == nodes[i])) > 0,
                             move.data$SENDER_FARMTYPE[which(move.data$FARMID_SENDER == nodes[i])[1]],
                             move.data$RECEIVER_FARMTYPE[which(move.data$FARMID_RECEIVER == nodes[i])[1]])
}

days_after_start = c(0,net.data$Days_from_Start[match(nodes[-1], net.data$To)])

# Reset values for the chosen origin farm
node_farmtypes[1] = "Origin farm"
days_after_start[1] = max(net.data$Days_from_Start)

# Add days, farmtype and color to network
network1 %v% "days" = net.data$Days_from_Start
network1 %v% "FarmType" = node_farmtypes


# Set colors
node_color <- rep("",length(nodes))
# color pakcage
colors = brewer.pal(n = 8, name = "YlGnBu")
farmtypes = c("15 11 Svin, produktionsbesætning",
              "15 46 Smågriseopdrætsbesætning",
              "15 41 Svin, avls- og opformeringsbesætn",
              "15 17 Frilandssvinebesætning",
              "15 13 Økologisk svinebesætning")

for(i in 1:length(nodes)){
  if(get.node.attr(network1,"FarmType")[i] == farmtypes[1]){
    node_color[i] <- colors[7]
  }else if(get.node.attr(network1,"FarmType")[i] == farmtypes[2]){
    node_color[i] <- colors[6]
  }else if(get.node.attr(network1,"FarmType")[i] == farmtypes[3]){
    node_color[i] <- colors[5]
  }else if(get.node.attr(network1,"FarmType")[i] == farmtypes[4]){
    node_color[i] <- colors[4]
  }else{
    node_color[i] <- colors[3]
  }
}

node_color[1] = "forestgreen"

# Plot
ggnet2(network1, color = "FarmType", palette = "Set2",
       size = "days", 
       label = T,
       arrow.size = 12, arrow.gap = 0.025,
       legend.position = "right", legend.size = 7) 

ggnet2(network1, color = "FarmType", palette = "Set2", 
       label = T,
       legend.position = "right", legend.size = 7) 



# Fixed placement 
x = gplot.layout.fruchtermanreingold(network1, NULL)
network1 %v% "x" = full_layout[, 1]
network1 %v% "y" = full_layout[, 2]

# Definde activation of nodes
network1 %v% "t1" = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
network1 %v% "t2" = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
network1 %v% "t3" = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
network1 %v% "t4" = c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
network1 %v% "t5" = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
network1 %v% "t6" = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
network1 %v% "t7" = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
network1 %v% "t8" = c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
network1 %v% "t9" = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
network1 %v% "t10" = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
network1 %v% "t11" = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

# Replane 0 with NA
network1 %v% "t1" = ifelse(network1 %v% "t1", 1, NA)
network1 %v% "t2" = ifelse(network1 %v% "t2", 1, NA)
network1 %v% "t3" = ifelse(network1 %v% "t3", 1, NA)
network1 %v% "t4" = ifelse(network1 %v% "t4", 1, NA)
network1 %v% "t5" = ifelse(network1 %v% "t5", 1, NA)
network1 %v% "t6" = ifelse(network1 %v% "t6", 1, NA)
network1 %v% "t7" = ifelse(network1 %v% "t7", 1, NA)
network1 %v% "t8" = ifelse(network1 %v% "t8", 1, NA)
network1 %v% "t9" = ifelse(network1 %v% "t9", 1, NA)
network1 %v% "t10" = ifelse(network1 %v% "t10", 1, NA)
network1 %v% "t11" = ifelse(network1 %v% "t11", 1, NA)


t2 = ggnet2(network1, mode = c("x", "y"),
       color = node_color,
       size = 8, 
       label = T,
       arrow.size = 12, arrow.gap = 0.025,
       legend.position = "bottom", legend.size = 7,
       na.rm = "t2")

t3 = ggnet2(network1, mode = c("x", "y"),
       color = node_color,
       size = 8, 
       label = T,
       arrow.size = 12, arrow.gap = 0.025,
       legend.position = "bottom", legend.size = 7,
       na.rm = "t3") 

t4 = ggnet2(network1, mode = c("x", "y"),
       color = node_color,
       size = 8, 
       label = T,
       arrow.size = 12, arrow.gap = 0.025,
       legend.position = "bottom", legend.size = 7,
       na.rm = "t4") 

t5 = ggnet2(network1, mode = c("x", "y"),
       color = node_color,
       size = 8, 
       label = T,
       arrow.size = 12, arrow.gap = 0.025,
       legend.position = "bottom", legend.size = 7,
       na.rm = "t5") 

t6 = ggnet2(network1, mode = c("x", "y"),
       color = node_color,
       size = 8, 
       label = T,
       arrow.size = 12, arrow.gap = 0.025,
       legend.position = "bottom", legend.size = 7,
       na.rm = "t6") 

t7 = ggnet2(network1, mode = c("x", "y"),
       color = node_color,
       size = 8, 
       label = T,
       arrow.size = 12, arrow.gap = 0.025,
       legend.position = "bottom", legend.size = 7,
       na.rm = "t7") 

t8 = ggnet2(network1, mode = c("x", "y"),
       color = node_color,
       size = 8, 
       label = T,
       arrow.size = 12, arrow.gap = 0.025,
       legend.position = "bottom", legend.size = 7,
       na.rm = "t8") 

t9 = ggnet2(network1, mode = c("x", "y"),
       color = node_color,
       size = 8, 
       label = T,
       arrow.size = 12, arrow.gap = 0.025,
       legend.position = "bottom", legend.size = 7,
       na.rm = "t9") 

t10 = ggnet2(network1, mode = c("x", "y"),
       color = node_color,
       size = 8, 
       label = T,
       arrow.size = 12, arrow.gap = 0.025,
       legend.position = "bottom", legend.size = 7,
       na.rm = "t10") 

t11 = ggnet2(network1, mode = c("x", "y"),
       color = node_color,
       size = 8, 
       label = T,
       na.rm = "t11") 


day = net.data$Days_from_Start
x_lim <- c(min(network1 %v% "x"), max(network1 %v% "x"))
y_lim <- c(min(network1 %v% "y"), max(network1 %v% "y"))

# show each temporal network
t2 + ggtitle(paste("Day: ", day[2])) 
t3 + ggtitle(paste("Day: ", day[2])) 
t4 + ggtitle(paste("Day: ", day[3])) 
t5 + ggtitle(paste("Day: ", day[4])) 
t7 + ggtitle(paste("Day: ", day[6]))  
t8 + ggtitle(paste("Day: ", day[7]))  
t9 + ggtitle(paste("Day: ", day[8]))  
t10 + ggtitle(paste("Day: ", day[9])) 
t11 + ggtitle(paste("Day: ", day[10])) + xlim(x_lim) + ylim(y_lim)




