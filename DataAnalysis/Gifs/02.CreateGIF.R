# -------------------------------------------------------------------------
#                 Creating a GIF for moving patterns 
# -------------------------------------------------------------------------
# Initialize -----------------------------------------------------------
source(file.path("../Model/Initialize/Load_packages.R"))
source(file.path("../Model/Initialize/Define_Functions.R"))
rawData = FALSE # Change if you have access to row data
source(file.path("../Model/Initialize/LoadNClean_Data.R"))

contact_farms_outputlist <- readRDS("../Data/DataAnalysis/Full_contact_farms.rds")

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
farm_nr = "2336659471"
start_farm_ID = contact_farms_outputlist[[farm_nr]]$farmID[1]
start_farm_type = info_all_farms$Farm_type[which(info_all_farms$Farmid == start_farm_ID)]
start_farm_type_english = info_all_farms %>% filter(Farmid == start_farm_ID) %>% pull(Farm_type_english)
start_date = as.Date("2020-02-01")
# farm_nr = 2336656811
# start_farm_ID = farms_infected$farmID[1]
# start_farm_type = info_all_farms %>% filter(Farmid == start_farm_ID) %>% pull(Farm_type)
# start_date = move.data$DATE_MOVEMENT[1]


# select data from farm 2336641601 and clean it
net_data = contact_farms_outputlist[[farm_nr]][-1,] %>% 
  mutate(days_since_start = as.integer(contact_farms_outputlist[[farm_nr]][-1,]$date - start_date)) %>% 
  select("From" = contactFarm, "To" = farmID, "Days_from_Start" = days_since_start)

# net_data = farms_infected[-1,] %>% 
#   mutate(days_since_start = as.integer(date_of_infection - start_date)) %>% 
#   select("From" = farmID_spreader, "To" = farmID, "Days_from_Start" = days_since_start)

# Create a combined id for from to 
net_data = net_data %>% rowwise() %>% mutate(samletID = paste(min(From,To), max(From,To), sep = ""))

# Keep only unique combined IDs and distinct connections
net_data = net_data %>% distinct(To, .keep_all = T)

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
x_lim <- c(ifelse(min(coordinates[,1]) < 0, floor(min(coordinates[,1]))-4, ceiling(min(coordinates[,1]))-4),
           ifelse(max(coordinates[,1]) < 0, floor(max(coordinates[,1])), ceiling(max(coordinates[,1]))))
y_lim<- c(ifelse(min(coordinates[,2]) < 0, floor(min(coordinates[,2])-2), ceiling(min(coordinates[,2]))-2),
          ifelse(max(coordinates[,2])< 0, floor(max(coordinates[,2]))+1, ceiling(max(coordinates[,2])))+1)

## create a directory to which the images will be written
dir_out <- file.path(dirname(getwd()), paste0("DataAnalysis/Gifs/Output_GIFs/GIF_Farm", start_farm_ID))
#If there already exist a directory for this herd rome it 
if (dir.exists(dir_out)) {
  unlink(dir_out, recursive = TRUE)  # Remove the existing directory
}
# Create the directory to storage
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
     # node_farmtypes[i] = ifelse(length(which(move.data$FARMID_SENDER == nodes[i])) > 0,
     #                            move.data$SENDER_FARMTYPE[which(move.data$FARMID_SENDER == nodes[i])[1]],
     #                            move.data$RECEIVER_FARMTYPE[which(move.data$FARMID_RECEIVER == nodes[i])[1]])
     node_farmtypes[i] = info_all_farms %>% filter(Farmid == nodes[i]) %>% pull(Farm_type)
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
   node_color[1] = "yellow"
   
   
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
                vertex.cex = rep(1.5,length(nodes)), # size nodes 
                vertex.col = node_color, #
                displaylabels = F, # show the node names
                label.pos = 5, # display the names directly over nodes
                main = paste("Moving pattern for farm", start_farm_ID, ",", start_farm_type_english),
                xlab =  paste0(format(start_date + net_data$Days_from_Start[node],"%e. %b %Y"), " ,  ", net_data$Days_from_Start[node], " days since start"))
  
  
   
    legend("bottomleft",                            
          legend = c("Origin farm", "15 11", "15 46", "15 41", "15 17", "15 13"),
          fill = c("yellow", colors[7], colors[6], colors[5],colors[4],colors[3]),   
          title = "Herd type",                       
          cex = 0.7,                         # Adjust size of text
          box.lwd = 0.5,                     # Adjust the width of the legend box
          box.lty = "solid",                 # Adjust the line type of the legend box
          box.col = "black")

   dev.off()
 }



# Create the gif ----------------------------------------------------------
#dir_out = "/Users/annasophiebjerremandjensen/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet(1)/Speciale/Code/DataAnalysis/Gifs/Gif_pics_Farm100"
## list file names and read in by increasing order

imgs <- mixedsort(list.files(dir_out, full.names = TRUE))
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 4 frames per second
img_animated <- image_animate(img_joined, fps = 4, optimize = TRUE)
## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = paste0(dir_out,"/Farm", start_farm_ID,".gif"))



# Save as a video using the av package ------------------------------------
# Create a temporary directory to store the individual frames
temp_dir <- tempdir()
frame_files <- vector("character", length(img_list))
for (i in seq_along(img_list)) {
  frame_files[i] <- file.path(temp_dir, sprintf("frame_%04d.png", i))
  image_write(img_list[[i]], path = frame_files[i])
}

# Create a video from the frames
video_path <- paste0(dir_out, "/Farm", start_farm_ID, ".mp4")
av::av_encode_video(frame_files, output = video_path, framerate = 4)

# Clean up the temporary files
unlink(temp_dir, recursive = TRUE)
