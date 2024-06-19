# -------------------------------------------------------------------------
#                 Creating a GIF for moving patterns 
# -------------------------------------------------------------------------

# Initialize -----------------------------------------------------------
source(file.path("../Model/Initialize/Load_packages.R"))
source(file.path("../Model/Initialize/Define_Functions.R"))
rawData = FALSE # Change if you have access to row data
source(file.path("../Model/Initialize/LoadNClean_Data.R"))

contact_farms_outputlist <- readRDS("../Data/dataAnalysis/Full_contact_farms.rds")

# Make english farm_type names
info_all_farms <- info_all_farms %>%
  mutate(Farm_type_english = case_when(
    str_starts(Farm_type, "15 11") ~ "Production",
    str_starts(Farm_type, "15 13") ~ "Organic",
    str_starts(Farm_type, "15 17") ~ "Free-range",
    str_starts(Farm_type, "15 41") ~ "Breeding and multiplication",
    str_starts(Farm_type, "15 46") ~ "Piglets",
    TRUE ~ "Slaughter"
  ))
# Initialize
# choose an origin farm to look at and extract info-----------------------------------------------------
start_farm_ID = 2336588621 #2336599638  #2336659471 #2336660964  # 2336656811 #2336651430
start_farm_type = info_all_farms %>% filter(Farmid == start_farm_ID) %>% pull(Farm_type_english)
start_date = as.Date("2020-02-01") #move.data$DATE_MOVEMENT[1]
end_date = as.Date("2020-12-31")

# select data from farm 2336641601 and clean it
net_data = contact_farms_outputlist[[as.character(start_farm_ID)]] %>% 
  mutate(days_since_start = as.integer(contact_farms_outputlist[[as.character(start_farm_ID)]]$date - start_date)) %>% 
  select( "From" = contactFarm, "To" = farmID, "Days_from_Start" = days_since_start, "contactDate" = date)

net_data = net_data[-1,]
net_data = net_data %>% distinct(To, .keep_all = T)


net_data = net_data %>% filter(contactDate < (end_date+1))

# All the unique farms included in this pattern
Nodes = c(start_farm_ID, unique(net_data$To))


# Fixed placement  --------------------------------------------------------
# Full network matrix
m1 <- matrix(NA, nrow = length(Nodes), ncol = length(Nodes))
# fill in with the contacts
for (i in 1:length(Nodes)){
  c = net_data$To[net_data$From == Nodes[i]]
  row_i = as.integer(Nodes %in% c)
  m1[i,] = row_i
}
# setting the network and get coordinates
full_n = as.network(x = m1, directed = T)
coordinates = gplot.layout.fruchtermanreingold(full_n, NULL)
# Find limits of x and y axis and round them 
# x_lim <- c(ifelse(min(coordinates[,1]) < 0, floor(min(coordinates[,1]))-2, ceiling(min(coordinates[,1]))-2),
#            ifelse(max(coordinates[,1]) < 0, floor(max(coordinates[,1])), ceiling(max(coordinates[,1]))))
# y_lim<- c(ifelse(min(coordinates[,2]) < 0, floor(min(coordinates[,2])-2), ceiling(min(coordinates[,2]))-2),
#           ifelse(max(coordinates[,2])< 0, floor(max(coordinates[,2]))+1, ceiling(max(coordinates[,2])))+1)
x_lim <- range(coordinates[,1]) + c(-1, 1)
y_lim <- range(coordinates[,2]) + c(-1, 1)

 




# Network model -----------------------------------------------------------
nodes = as.character(Nodes)
# extract the last 3 characters from each farmID
farms_short <- substr(nodes, nchar(nodes) - 2, nchar(nodes))


# Create an empty matrix with specified column and row names
column_names <- nodes
row_names <- nodes
m <- matrix(NA, nrow = length(row_names), ncol = length(column_names),
            dimnames = list(row_names, column_names))

# fill in with the contacts
for (i in 1:length(nodes)){
  c = net_data$To[net_data$From == nodes[i]]
  row_i = as.integer(nodes %in% c)
  m[i,] = row_i
}

#create network
net <- as.network(x = m, # the network object
                  directed = TRUE, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  matrix.type = "adjacency") # the type of input

# Add coordinates to network
xy = coordinates[1:length(nodes),]




# Add farm types to network
node_farmtypes = rep(NA,length(nodes))
for (i in 1:length(nodes)){
  # node_farmtypes[i] = ifelse(length(which(move.data$FARMID_SENDER == nodes[i])) > 0,
  #                            move.data$SENDER_FARMTYPE[which(move.data$FARMID_SENDER == nodes[i])[1]],
  #                            move.data$RECEIVER_FARMTYPE[which(move.data$FARMID_RECEIVER == nodes[i])[1]])
  node_farmtypes[i] = info_all_farms %>% filter(Farmid == nodes[i]) %>% pull(Farm_type)
}

network::set.vertex.attribute(net, "FarmType",node_farmtypes)

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
node_color[1] = "yellow"

# Add the number of days after start date the given farm was in contact
days_after_start = c(0,net_data$Days_from_Start[match(nodes[-1], net_data$To)])
# Add farm names/IDs to the network
network.vertex.names(net) <- c(0,net_data$Days_from_Start)

### Color by days since start
# 
# # Add the number of days after start date the given farm was in contact
# days_after_start = c(0,net_data$Days_from_Start[match(nodes[-1], net_data$To)])
# 
# # Rescale the size (days) to be between 0.1 and 1
# size = rescale(days_after_start, to = c(0.1,1))
# # Set firt farm node size to the mean size so it is easy and clear to find the starting point
# size[1] = mean(size)
# # Include the days after start (not scaled in the network)
# network::set.vertex.attribute(net,"Days",days_after_start)
# 
# # Add farm names/IDs to the network
# network.vertex.names(net) <- c(0,net_data$Days_from_Start)
# 
# # Add color by farmtype:
# node_color <- rep("",length(nodes))
# 
# # Interpolate between the two palettes to create a custom palette with 47 colors
# custom_palette <- paletteer_c("ggthemes::Blue-Teal", max(net_data$Days_from_Start)+1)
# 
# 
# 
# # Add colors to the nodes
# node_color = custom_palette[days_after_start+1]
# node_color[1] = "yellow"

# Check how the network looks
summary.network(net, # the network we want to look at
                print.adj = FALSE # if TRUE then this will print out the whole adjacency matrix.
)



# Saves the plot in the current directory
#fp <- paste0(dir_out, "/", node, ".png")
#png(filename = fp)
plot.network(net, # our network object
             coord = xy,
             xlim = x_lim,
             ylim = y_lim,
             vertex.cex = rep(2.5,length(nodes)), 
             vertex.col = node_color, #
             displaylabels = F, # show the node names
             label.pos = 5, # display the names directly over nodes
             #main = paste0("Contact pattern herd ", start_farm_ID, ", ", start_farm_type),
             xlab =  paste0(format(end_date,"%e. %b. %Y"), ",  ", difftime(end_date, start_date, units = "days"), " days since start"))

# legend("bottomleft",                            
#        legend = c("Origin farm", "15 11", "15 46", "15 41", "15 17", "15 13"),
#        fill = c("yellow", colors[7], colors[6], colors[5],colors[4],colors[3]),   
#        title = "Herd Type",                       
#        cex = 0.8,                         # Adjust size of text
#        box.lwd = 0.1,                     # Adjust the width of the legend box
#        box.lty = "solid",                 # Adjust the line type of the legend box
#        box.col = "black",
#        horiz = TRUE)

dev.off()
