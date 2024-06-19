
# Initialize -----------------------------------------------------------
source(file.path("../Model/Initialize/Load_packages.R"))
source(file.path("../Model/Initialize/Define_Functions.R"))
rawData = FALSE # Change if you have access to row data
source(file.path("../Model/Initialize/LoadNClean_Data.R"))

# -------------------------------------------------------------------------
#                           PAIR CONTACTS
# -------------------------------------------------------------------------

# Capacity of farms ---------------------------------------------------
# Count how many of each farmtype and how many pigs are within
info_all_farms %>%  group_by(Farm_type) %>% summarise(n_farms = n(),
                                                 kapacity = sum(N_pigs, na.rm = T),
                                                 min_kap = min(N_pigs, na.rm = T),
                                                 max_kap = max(N_pigs, na.rm = T),
                                                 mean_kap = mean(N_pigs, na.rm = T))

all_farms = info_all_farms %>% mutate(kapacitet = case_when( N_pigs < 500     ~ "<500",
                                       N_pigs >= 500  & N_pigs < 1000    ~ "500-1000",
                                       N_pigs >= 1000 & N_pigs < 1500    ~ "1000-1500",
                                       N_pigs >= 1500 & N_pigs < 2000    ~ "1500-2000",
                                       N_pigs >= 2000 & N_pigs < 2500    ~ "2000-2500",
                                       N_pigs >= 2500 & N_pigs < 3000    ~ "2500-3000",
                                       N_pigs >= 3000 & N_pigs < 3500    ~ "3000-3500",
                                       N_pigs >= 3500 & N_pigs < 4000    ~ "3500-4000",
                                       N_pigs >= 4000 & N_pigs < 4500    ~ "4000-4500",
                                       N_pigs >= 4500 & N_pigs < 5000    ~ "4500-5000",
                                       N_pigs >= 5000 & N_pigs < 5500    ~ "5000-5500",
                                       N_pigs >= 5500 & N_pigs < 6000    ~ "5500-6000",
                                       N_pigs >= 6000 & N_pigs < 6500    ~ "6000-6500",
                                       N_pigs >= 6500 & N_pigs < 7000    ~ "6500-7000",
                                       N_pigs >= 7000 & N_pigs < 8000    ~ "7000-8000",
                                       N_pigs >= 8000 & N_pigs < 9000    ~ "8000-9000",
                                       N_pigs >= 9000 & N_pigs < 10000   ~ "7000-8000",
                                       N_pigs >= 7000 & N_pigs < 8000    ~ "7000-8000",
                                       N_pigs >= 9000                    ~ ">9000"))
  
# Kapacitet på gårde
all_farms %>% group_by(kapacitet, Farm_type) %>% summarise(N = n()) %>% 
  ggplot(aes(x = kapacitet, y = N)) +
  geom_bar(stat = "identity", position = position_dodge())

# kapacitet på gårde fordelt på farmtype
all_farms %>% group_by(kapacitet, Farm_type) %>% summarise(N = n()) %>% 
  ggplot(aes(x = kapacitet, y = N, fill = Farm_type)) +
  geom_bar(stat = "identity", position = position_dodge())



# Tracking pairs ----------------------------------------------------------
farm.pairs = tibble(pairID = character(),
                    senderID = numeric(),
                    receiverID = numeric(),
                    typeMove = character(),
                    date = Date())

start = Sys.time()

for ( move in 1:nrow(move.data)){
  sender = move.data$FARMID_SENDER[move]
  receiver = move.data$FARMID_RECEIVER[move]
  farm.pairs = farm.pairs %>% add_row(pairID = paste(min(sender,receiver), max(sender,receiver), sep = ""),
                                      senderID = sender,
                                      receiverID = receiver,
                                      typeMove = ifelse(move.data$SENDER_FARMTYPE[move] == move.data$RECEIVER_FARMTYPE[move],
                                                        "Within farm types",
                                                        "Between farm types"),
                                      date = move.data$DATE_MOVEMENT[move])
}

stop = Sys.time()
stop-start

# save data 
saveRDS(farm.pairs, file.path("../Data", "farm_pairs_full.rds"))
farm.pairs = readRDS("../Data/farm_pairs.rds")
#farm.pairs = farm_pairs_full
# count unique pairs
length(unique(farm.pairs$pairID))

# Divide into categories
farm.pairs = farm.pairs %>% group_by(pairID, typeMove) %>% summarise(pairMoves = n()) 
# farm.pairs = farm.pairs %>% mutate(moveFreq = case_when(pairMoves == 1                       ~ "1",
#                                                         pairMoves == 2                       ~ "2",
#                                                         pairMoves == 3                       ~ "3",
#                                                         pairMoves == 4                       ~ "4",
#                                                         pairMoves == 5                       ~ "5",
#                                                         pairMoves > 5   & pairMoves <= 10    ~ "6-10",
#                                                         pairMoves > 10  & pairMoves <= 20    ~ "11-20",
#                                                         pairMoves > 20  & pairMoves <= 30    ~ "21-30",
#                                                         pairMoves > 30  & pairMoves <= 40    ~ "31-40",
#                                                         pairMoves > 40  & pairMoves <= 50    ~ "41-50",
#                                                         pairMoves > 50  & pairMoves <= 60    ~ "51-60",
#                                                         pairMoves > 60                       ~ "> 60"))
farm.pairs = farm.pairs %>% mutate(moveFreq = case_when(pairMoves == 1                         ~ "1",
                                                        pairMoves > 1   & pairMoves <= 5       ~ "2-5",
                                                        pairMoves > 5   & pairMoves <= 20      ~ "6-20",
                                                        pairMoves > 20  & pairMoves <= 40      ~ "21-40",
                                                        pairMoves > 40  & pairMoves <= 60      ~ "41-60",
                                                        pairMoves > 60  & pairMoves <= 80      ~ "61-80",
                                                        pairMoves > 80  & pairMoves <= 100     ~ "81-100",
                                                        pairMoves > 100  & pairMoves <= 120    ~ "101-120",
                                                        pairMoves > 120  & pairMoves <= 140    ~ "121-140",
                                                        pairMoves > 140  & pairMoves <= 160    ~ "141-160",
                                                        pairMoves > 160  & pairMoves <= 180    ~ "161-180",
                                                        pairMoves > 180  & pairMoves <= 200    ~ "181-200",
                                                        pairMoves > 200                        ~ "> 200"))

#farm.pairs$moveFreq <- factor(farm.pairs$moveFreq, levels = c("1","2","3","4","5","6-10","11-20","21-30","31-40","41-50","51-60","> 60"))
farm.pairs$moveFreq <- factor(farm.pairs$moveFreq, levels = c("1","2-5","6-20","21-40","41-60","61-80","81-100","101-120","121-140","141-160","161-180","181-200","> 200"))

# Make the plot of movements within each pairs. 
farm.pairs %>% group_by(moveFreq, typeMove) %>% summarise(N = n()) %>% 
  ggplot(aes(x = moveFreq, y = N, fill = typeMove)) +
  geom_bar(stat = "identity") +
  labs(title = "",
       x = "Movements [#]",
       y = "Pairs [#]") +
  scale_fill_brewer(name = "Pair installation",
                    labels = c("Different types", "Same type"),
                    palette = "Paired") +
  theme_minimal()
  



# -------------------------------------------------------------------------
#               Track contacts initiated from each farm
# -------------------------------------------------------------------------
# Initialize an empty list to store interactions for each farm
contact_farms_outputlist = list()
unique_contact = tibble(start_farm = double(), N_contacts = integer(), N_unique_contacts = integer())

# timing loop
start = Sys.time()

# Start a loop that runs through all farms in sendIDs (only farms that sends to others)
for (id in 1:length(sendIDs)){
  # update farmID
  farm = sendIDs[id]
  
  period = days(50)
  start_date = move.data$DATE_MOVEMENT[1]
  end_date = start_date + period
  
  # Subset/reset data to only contain days within the set period
  period.data <- move.data %>%
    filter(DATE_MOVEMENT >= start_date & DATE_MOVEMENT <= end_date)
  
  # Initializing index
  index <- 1
  
  # Allocate space to collect all farms that have somehow been in contact with the chosen farm 
  contact_farms <- tibble(FarmID = farm, date = start_date, contactFarm = NA)
  
  while (index != 0){
    # Check if there's only one row left in period.data
    if (nrow(period.data) == 1) {
      break  # Break the while loop if there's only one row left
    }
    
    # Find next move from a relevant farm
    index <- ifelse(is.na(which(period.data$FARMID_SENDER %in% contact_farms$FarmID)[1]), 0, 
                    which(period.data$FARMID_SENDER %in% contact_farms$FarmID)[1])
    
    # Check if index is 0 before adding to contact_farms
    if (index != 0) {
      # Update contact tibble
      contact_farms <- contact_farms %>% add_row(FarmID = period.data$FARMID_RECEIVER[index],
                                                 date = period.data$DATE_MOVEMENT[index],
                                                 contactFarm = period.data$FARMID_SENDER[index])
      
      # Update data, such that all prior indexes are excluded 
      period.data <- period.data %>% slice((index+1):n())
      
    
    }
  }
  unique_contact = unique_contact %>% add_row(start_farm = farm,
                                              N_contacts = length(contact_farms$FarmID), 
                                              N_unique_contacts = length(unique(contact_farms$FarmID)))
  contact_farms_outputlist[[id]] = contact_farms
  print(id)
}

slut = Sys.time()

# Tid for loopet har kørt
slut - start

# Save data on moving patterns of all farms: 
#saveRDS(contact_farms_outputlist, file.path("../Data", "contact_farms.rds"))
#saveRDS(unique_contact, file.path("../Data", "unique_contact.rds"))
unique_contact = readRDS("../Data/dataAnalysis/unique_contact.rds")

# Plotting
hist(unique_contact$N_unique_contacts)
unique_contact %>% group_by(N_unique_contacts) %>% summarise(N=n()) %>% 
  ggplot(aes(x = N_unique_contacts, y = N)) +
  geom_bar(stat = "identity") + 
  labs(title = "Number of contacts reached within 50 days",
       y = " # farms",
       x = "Number of unique contacts")

hist(unique_contact$N_unique_contacts)
unique_contact %>% group_by(N_contacts) %>% summarise(N=n()) %>% filter(N_contacts > 1) %>% 
  ggplot(aes(x = N_contacts, y = N)) +
  geom_bar(stat = "identity") + 
  labs(title = "Number of contacts reached within 50 days",
       y = "farmsIDs on start farm",
       x = "Number of total contacts")


period.data %>% group_by(N_MOVED_PIGS, SENDER_FARMTYPE) %>% summarise(N = n())

# Maks antal unikke kontaker på det givne antal dage
max(unique_contact$N_unique_contacts)




# Ikke brugt kode herfra
# Network for en specific farm -----------------------------------------------------
contact_farms_outputlist <- readRDS("../Data/contact_farms.rds")
farm_nr = 24
start_farm = contact_farms_outputlist[[farm_nr]]$FarmID[1]
start_farm_type = move.data$SENDER_FARMTYPE[which(move.data$FARMID_SENDER == start_farm)[1]]
start_date = move.data$DATE_MOVEMENT[1]
# select data from farm 2336641601 and clean it
dend.data = contact_farms_outputlist[[farm_nr]][-1,] %>% 
  mutate(days_since_start = as.integer(contact_farms_outputlist[[farm_nr]][-1,]$date - start_date)) %>% 
  select("From" = contactFarm, "To" = FarmID, "Days_from_Start" = days_since_start)

# Create a combined id for from to 
dend.data = dend.data %>%rowwise() %>% mutate(samletID = paste(min(From,To), max(From,To), sep = ""))

# Keep only unique combined IDs and distinct connections
dend.data = dend.data %>% distinct(samletID, .keep_all = T) %>% distinct(To, .keep_all = T)

# All the unique farms included in this pattern
nodes = unique(c(dend.data$From, dend.data$To))

# Network model 
nodes = as.character(nodes)
# extract the last 3 characters from each farmID 
farms_short <- substr(nodes, nchar(nodes) - 2, nchar(nodes))

column_names <- nodes
row_names <- nodes

# Create an empty matrix with specified column and row names
m <- matrix(NA, nrow = length(row_names), ncol = length(column_names),
            dimnames = list(row_names, column_names))

# fill in with the contacts
for (i in 1:length(nodes)){
  c = dend.data$To[dend.data$From == nodes[i]]
  row_i = as.integer(nodes %in% c)
  m[i,] = row_i
  
}

#create network
library(statnet)
net <- as.network(x = m, # the network object
                  directed = TRUE, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  matrix.type = "adjacency") # the type of input

# Add farm names/IDs to the network
network.vertex.names(net) <- c(farms_short)

# Add farm types to network
node_farmtypes = rep(NA,length(nodes))
for (i in 1:length(nodes)){
  node_farmtypes[i] = ifelse(length(which(move.data$FARMID_SENDER == nodes[i])) > 0,
                             move.data$SENDER_FARMTYPE[which(move.data$FARMID_SENDER == nodes[i])[1]],
                             move.data$RECEIVER_FARMTYPE[which(move.data$FARMID_RECEIVER == nodes[i])[1]])
}

network::set.vertex.attribute(net, "FarmType",node_farmtypes)

# Add the number of days after start date the given farm was in contact
days_after_start = c(0,dend.data$Days_from_Start[match(nodes[-1], dend.data$To)])
# Rescale the size (days) to be between 0.1 and 1 
size = rescale(days_after_start, to = c(0.1,1))
# Set firt farm node size to the mean size so it is easy and clear to find the starting point
size[1] = mean(size)
# Include the days after start (not scaled in the network)
network::set.vertex.attribute(net,"Days",days_after_start)


# Add color by farmtype: 
node_color <- rep("",length(nodes))
# color pakcage
colors = brewer.pal(n = 8, name = "YlGnBu")
farmtypes = c("15 11 Svin, produktionsbesætning",
              "15 46 Smågriseopdrætsbesætning",
              "15 41 Svin, avls- og opformeringsbesætn",
              "15 17 Frilandssvinebesætning",
              "15 13 Økologisk svinebesætning")

for(i in 1:length(nodes)){
  if(get.node.attr(net,"FarmType")[i] == farmtypes[1]){
    node_color[i] <- colors[7]
  }else if(get.node.attr(net,"FarmType")[i] == farmtypes[2]){
    node_color[i] <- colors[6]
  }else if(get.node.attr(net,"FarmType")[i] == farmtypes[3]){
    node_color[i] <- colors[5]
  }else if(get.node.attr(net,"FarmType")[i] == farmtypes[4]){
    node_color[i] <- colors[4]
  }else{
    node_color[i] <- colors[3]
  }
}

# Add special colour to the start farm 
node_color[1] = "forestgreen"

# Check how the network looks
summary.network(net, # the network we want to look at
                print.adj = FALSE # if TRUE then this will print out the whole adjacency matrix.
)


# # Saves the plot in the current directory
# pdf("Network_Plot_farm24.pdf", # name of pdf (need to include .pdf)
#     width = 10, # width of resulting pdf in inches
#     height = 10 # height of resulting pdf in inches
# ) 
plot.network(net, # our network object
             vertex.cex = size*5, # size nodes by their age
             vertex.col = node_color, # color nodes by gender
             displaylabels = T, # show the node names
             label.pos = 5, # display the names directly over nodes
             main = paste("Moving pattern for farm", start_farm, ",", start_farm_type))



legend("bottomright",                             # Position of the legend
       legend = c("Origin farm", c(farmtypes)),  # Labels for the legend
       fill = c("forestgreen", colors[7], colors[6], colors[5],colors[4],colors[3]),      # Colors for the legend
       title = "Legend",                         # Title for the legend
       cex = 0.6,                         # Adjust size of text
       box.lwd = 0.5,                     # Adjust the width of the legend box
       box.lty = "solid",                 # Adjust the line type of the legend box
       box.col = "black") 








# Dendogram try for farm 24  ----------------------------------------------------------------
# libraries
library(ggplot2)
library(ggraph)
library(igraph)
library(tidyverse)
library(dendextend)
library(colormap)
library(kableExtra)

start_date = move.data$DATE_MOVEMENT[1]
# select data from farm 24 and clean it for the dendogram
dend.data = contact_farms_outputlist[[farm_nr]][-1,] %>% 
            mutate(days_since_start = as.integer(contact_farms_outputlist[[farm_nr]][-1,]$date - start_date)) %>% 
            select("From" = contactFarm, "To" = FarmID, "Days_from_Start" = days_since_start)
# Create a combined id for from to 
dend.data = dend.data %>%rowwise() %>% mutate(samletID = paste(min(From,To), max(From,To), sep = ""))
# Keep only unique combined IDs
dend.data = dend.data %>% distinct(samletID, .keep_all = T) %>% distinct(To, .keep_all = T)


nodes = unique(c(dend.data$From, dend.data$To))

edge.list = dend.data %>% select(From, To)
# Now we can plot that
dend.graph <- graph_from_data_frame( edge.list )
ggraph(dend.graph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point(color="#69b3a2", size=3)  +
  theme_void() +
  coord_flip() +
  scale_y_reverse() 

# Plot Dendogram
dend.data = dend.data %>% select(To, Days_from_Start) %>% add_row("To" = start_farm, "Days_from_Start" = 0)
# Clusterisation using 3 variables
dend.data %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() -> dend

# Plot
par(mar=c(7,3,1,1))  # Increase bottom margin to have the complete label
plot(dend)





