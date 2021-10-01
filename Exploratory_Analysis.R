library("igraph")

setwd("C:\\Users\\shaun\\OneDrive\\Desktop\\FOSSEE\\codes")

# Note: for privacy purposes, the real names were removed from the dataset and each user was assigned a unique number
# the dataset used can be found online
# Loading the dataset
network <- read.csv("data.csv", header = FALSE, sep = " ", col.names = c("Source", "Target"))



# Exploratory analysis on the data
str(network)
head(network)

# converting data to igraph format
GraphNetwork <- graph.data.frame(network, directed = T)
GraphNetwork <- connect(GraphNetwork, 100, mode = c("all", "out", "in", "total"))


# First plot of raw dataset
plot.igraph(GraphNetwork, layout = layout.fruchterman.reingold, 
            vertex.label = NA,
            vertex.label.cex = 1,
            vertex.size = 4, edge.curved = TRUE,
            main = "All nodes & edges",
            edge.arrow.size = 0)


# Identifying nodes with degree less than 50 and removing them
bad.network<-V(GraphNetwork)[degree(GraphNetwork)<=50] 
GraphNetwork<-delete.vertices(GraphNetwork, bad.network)


###Number of edges per vertex (relationships per people)
table(degree(GraphNetwork))
str(GraphNetwork)
V(GraphNetwork)
E(GraphNetwork)

plot.igraph(GraphNetwork, layout = layout.fruchterman.reingold, 
            vertex.label=V(GraphNetwork)$name,
            vertex.size = degree(GraphNetwork), edge.curved = TRUE,
            main = "Filtered dataset visualised, degree(node)>=50",
            edge.arrow.size = 0.25)

deg.in <- degree(GraphNetwork, mode = "in")
deg.in

####Compute shortest paths in and out between each pairs of nodes
shortest_path_in <- shortest.paths(GraphNetwork, mode='in')
shortest_path_out <- shortest.paths(GraphNetwork, mode = 'out')
shortest_path_in
shortest_path_out

### Centrality measures
degree.met <- degree(GraphNetwork)
between.met <- betweenness(GraphNetwork)
close.met <- closeness(GraphNetwork)
eigen.met <- evcent(GraphNetwork)$vector
core.met <- graph.coreness(GraphNetwork)
metrices <- data.frame(degree.met, between.met, close.met, eigen.met, core.met)
metrices

#Density
density.GraphNetwork = graph.density(GraphNetwork)
density.GraphNetwork

#Reciprocity
recprocity.GraphNetwork = reciprocity(GraphNetwork)
recprocity.GraphNetwork
