# -------------------------------------------------------------------------
#                 Creating a GIF for moving patterns 
# -------------------------------------------------------------------------

# Load packages -----------------------------------------------------------
source(file.path("../Initialize/Load_packages.R"))


# Load and clean data  -----------------------------------------------------
source(file.path("../Initialize/LoadNClean_Data.R"))

contact_farms_outputlist <- readRDS("../Data/dataAnalysis/contact_farms.rds")

# Initialize
# choose an origin farm to look at and extract info-----------------------------------------------------
farm_nr = 100
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
Nodes = c(start_farm_ID, unique(net_data$To))

# Fixed placement 
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
x_lim <- c(ifelse(min(coordinates[,1]) < 0, floor(min(coordinates[,1]))-2, ceiling(min(coordinates[,1]))-2),
           ifelse(max(coordinates[,1]) < 0, floor(max(coordinates[,1])), ceiling(max(coordinates[,1]))))
y_lim<- c(ifelse(min(coordinates[,2]) < 0, floor(min(coordinates[,2])-2), ceiling(min(coordinates[,2]))-2),
          ifelse(max(coordinates[,2])< 0, floor(max(coordinates[,2]))+1, ceiling(max(coordinates[,2])))+1)

## create a directory to which the images will be written
dir_out <- file.path(getwd(), paste0("Gif_pics_Farm", farm_nr))
dir.create(dir_out, recursive = TRUE)


# Start loop to make pictures for gif -------------------------------------

for (node in 1:(length(Nodes)-1)){
  # Cut data 
   net.data = net_data %>% filter(Days_from_Start <= net_data$Days_from_Start[node])
   
   # Network model -----------------------------------------------------------
   nodes = as.character(Nodes[1:(node+1)])
   # extract the last 3 characters from each farmID 
   farms_short <- substr(nodes, nchar(nodes) - 2, nchar(nodes))
   
   
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
   
   #create network
   net <- as.network(x = m, # the network object
                     directed = TRUE, # specify whether the network is directed
                     loops = FALSE, # do we allow self ties (should not allow them)
                     matrix.type = "adjacency") # the type of input
   
   # Add coordinates to network
   xy = coordinates[1:length(nodes),]
    
   
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
   days_after_start = c(0,net.data$Days_from_Start[match(nodes[-1], net.data$To)])
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
   

   
   # Saves the plot in the current directory
   fp <- paste0(dir_out, "/", node, ".png")
   png(filename = fp)
   plot.network(net, # our network object
                coord = xy,
                xlim = x_lim,
                ylim = y_lim,
                vertex.cex = rep(4.5,length(size)), # size nodes by their age
                vertex.col = node_color, #
                displaylabels = T, # show the node names
                label.pos = 5, # display the names directly over nodes
                main = paste("Moving pattern for farm", start_farm_ID, ",", start_farm_type),
                xlab =  paste0(format(start_date + net_data$Days_from_Start[node],"%e. %b"), " ,  ", net_data$Days_from_Start[node], " days since start"))
  
  
   
    legend("bottomleft",                            
          legend = c("Origin farm", c(farmtypes)),
          fill = c("forestgreen", colors[7], colors[6], colors[5],colors[4],colors[3]),   
          title = NULL,                       
          cex = 0.7,                         # Adjust size of text
          box.lwd = 0.5,                     # Adjust the width of the legend box
          box.lty = "solid",                 # Adjust the line type of the legend box
          box.col = "black")

   dev.off()
 }

# Create the gif ----------------------------------------------------------
dir_out = "/Users/annasophiebjerremandjensen/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet(1)/Speciale/Code/DataAnalysis/Gif_pics_Farm100"
## list file names and read in by increasing order
imgs <- mixedsort(list.files(dir_out, full.names = TRUE))

img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 1 frames per second
img_animated <- image_animate(img_joined, fps = 1)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = paste0(dir_out,"/Farm", farm_nr,".gif"))



