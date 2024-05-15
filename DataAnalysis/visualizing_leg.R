# Visualizing 

# network -----------------------------------------------------------------


library(statnet)
num_nodes = 10

my_sociomatrix <- matrix(round(runif(num_nodes*num_nodes)), # edge values
                         nrow = num_nodes, #nrow must be same as ncol
                         ncol = num_nodes)

diag(my_sociomatrix) <- 0

net <- as.network(x = my_sociomatrix, # the network object
                  directed = TRUE, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  matrix.type = "adjacency" # the type of input
)

network.vertex.names(net) <- LETTERS[1:10]
network.vertex.names(net) <- c("Susan","Rachel","Angela","Carly","Stephanie","Tom","Mike","Tony","Matt","Steven")


# Create the variable
gender <- c(rep("Female",num_nodes/2),rep("Male",num_nodes/2))
# Take a look at our variable
print(gender)
# Add it to the network object
set.vertex.attribute(net, # the name of the network object
                     "Gender", # the name we want to reference the variable by in that object
                     gender # the value we are giving that variable
) 

age <- round(rnorm(num_nodes,20,3))
set.vertex.attribute(net,"Age",age)

summary.network(net, # the network we want to look at
                print.adj = FALSE # if TRUE then this will print out the whole adjacency matrix.
)


node_colors <- rep("",num_nodes)
for(i in 1:num_nodes){
  if(get.node.attr(net,"Gender")[i] == "Female"){
    node_colors[i] <- "lightblue"
  }else{
    node_colors[i] <- "maroon"
  }
}
print(node_colors)


pdf("Network_Plot_farm24.pdf", # name of pdf (need to include .pdf)
    width = 10, # width of resulting pdf in inches
    height = 10 # height of resulting pdf in inches
) 
plot.network(net, # our network object
             vertex.col = node_colors, # color nodes by gender
             vertex.cex = (age)/5, # size nodes by their age
             displaylabels = T, # show the node names
             label.pos = 5 # display the names directly over nodes
)
dev.off() # finishes plotting and finalizes pdf




num_nodes <- 40
num_edges <- 80

node_names <- rep("",num_nodes)
for(i in 1:num_nodes){
  node_names[i] <- paste("person",i,sep = "_")
}
print(node_names)


edgelist <- matrix("",nrow= num_edges,ncol = 2)

for(i in 1:num_edges){
  edgelist[i,] <- sample(x= node_names, # the names we want to sample from
                         size = 2, # sender and receiver
                         replace = FALSE # we do not allow self edges
  ) 
}
print(edgelist)


net2 <- network.initialize(num_nodes)
network.vertex.names(net2) <- node_names


net2[as.matrix(edgelist)] <- 1
income <- round(rnorm(num_nodes,mean = 50000,sd = 20000))
set.vertex.attribute(net2,"Income",income)




# Mere network ------------------------------------------------------------
nodes = as.character(nodes)
# Udtræk de sidste to tegn fra hvert element
farms_short <- substr(nodes, nchar(nodes) - 1, nchar(nodes))

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
net <- as.network(x = m, # the network object
                  directed = TRUE, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  matrix.type = "adjacency") # the type of input

# Add farm names/IDs to the network
network.vertex.names(net) <- c(farms_short)

# Add the number of days after start date the given farm was in contact
days_after_start = dend.data$Days_from_Start
set.vertex.attribute(net,"Days",days_after_start)

# Add color to the start farm 
node_colors = ifelse(as.numeric(nodes %in%start_farm) == 1, "limegreen", "tomato")


summary.network(net, # the network we want to look at
                print.adj = FALSE # if TRUE then this will print out the whole adjacency matrix.
)


# # Saves the plot in the current directory
# pdf("Network_Plot_farm24.pdf", # name of pdf (need to include .pdf)
#     width = 10, # width of resulting pdf in inches
#     height = 10 # height of resulting pdf in inches
# ) 
plot.network(net, # our network object
             vertex.cex = c(1, 1.5, 2, 3.5, 4, 5, 5.25, 5.75, 5.75, 6.5), # size nodes by their age
             vertex.col = node_colors, # color nodes by gender
             displaylabels = T, # show the node names
             label.pos = 5 # display the names directly over nodes
)
dev.off()









# dendograms --------------------------------------------------------------
# libraries
library(ggplot2)
library(ggraph)
library(igraph)
library(tidyverse)
library(dendextend)
library(colormap)
library(kableExtra)



# dendogram1 --------------------------------------------------------------
# create a data frame 
data=data.frame(
  level1="CEO",
  level2=c( rep("boss1",4), rep("boss2",4)),
  level3=paste0("mister_", letters[1:8]))

# transform it to a edge list!
edges_level1_2 = data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 = data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edge_list=rbind(edges_level1_2, edges_level2_3)

# Now we can plot that
mygraph <- graph_from_data_frame( edge_list )
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point(color="#69b3a2", size=3) +
  geom_node_text(
    aes(  label=c("CEO", "Manager", "Manager", LETTERS[8:1]) ), 
    hjust=c(1,0.5, 0.5, rep(0,8)), 
    nudge_y = c(-.02, 0, 0, rep(.02,8)),
    nudge_x = c(0, .3, .3, rep(0,8))
  ) +
  theme_void() +
  coord_flip() +
  scale_y_reverse() 


# dendogram2 --------------------------------------------------------------

options(knitr.table.format = "html")
# Load the data
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyUndirecterWeighted.csv", header=T, row.names="Cities.", sep=",") %>% as.matrix
colnames(data) <- gsub("\\.", " ", colnames(data))

# show data
tmp <- data %>% as.data.frame() %>% select(1,3,6) %>% .[c(1,3,6),]
tmp[is.na(tmp)] <- "-"
tmp %>% kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

# Perform hierarchical cluster analysis.
dend <- as.dist(data) %>% 
  hclust(method="ward.D") %>% 
  as.dendrogram()

# Plot with Color in function of the cluster
leafcolor <- colormap(colormap = colormaps$viridis, nshades = 5, format = "hex", alpha = 1, reverse = FALSE)
par(mar=c(1,1,1,7))
dend %>%
  set("labels_col", value = leafcolor, k=5) %>%
  set("branches_k_color", value = leafcolor, k = 5) %>%
  plot(horiz=TRUE, axes=FALSE)



# dendogram3 --------------------------------------------------------------
# Load data
data <- scale(dend.data)
dist.res <- dist(data)
hc <- hclust(dist.res, method = "ward.D2")
dend <- as.dendrogram(hc)
plot(dend)

dend.data = dend.data %>% select(-samletID)
dend.data$To = as.character(dend.data$To)


# Library
library(tidyverse)

# Data
head(mtcars)

dend.data = dend.data %>% select(To, Days_from_Start) #%>% add_row("To" = start_farm, "Days_from_Start" = 0)
# Clusterisation using 3 variables
dend.data %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() -> dend

# Plot
par(mar=c(7,3,1,1))  # Increase bottom margin to have the complete label
plot(dend)
