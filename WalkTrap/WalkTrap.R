library("igraph")
setwd("C:\\Users\\shaun\\OneDrive\\Desktop\\FOSSEE\\codes")

start_time = Sys.time()
# Note: for privacy purposes, the real names were removed from the dataset and each user was assigned a unique number
# the dataset used can be found online
# Loading the dataset
network = read.csv("data.csv", header = FALSE, sep = " ", col.names = c("Source", "Target"))
y = data.frame(network$Source, network$Target)
net = graph.data.frame(y,directed=F)


# converting data to igraph format
GraphNetwork = graph.data.frame(network, directed = T)
GraphNetwork = connect(GraphNetwork, 100, mode = c("all", "out", "in", "total"))


# Compute shortest paths in and out between each pairs of nodes
shortest_path_in = shortest.paths(GraphNetwork, mode='in')
shortest_path_out = shortest.paths(GraphNetwork, mode = 'out')
shortest_path_in
shortest_path_out


#walktrap Algorithm
WalkTrapNetwork = walktrap.community(GraphNetwork, steps = 5, modularity = TRUE)
WalkTrapNetwork
str(WalkTrapNetwork)
is_hierarchical(WalkTrapNetwork)

dendrogram_network = as.dendrogram(WalkTrapNetwork)
plot(dendrogram_network, main = "Dendrogram for Walktrap Community")


clusternet = cluster_walktrap(net)
plot(clusternet, net,vertex.size=13,vertex.label.cex=0.7,edge.arrow.distance=1, edge.color = "black", main = "Walktrap Community",layout = layout.fruchterman.reingold) 

end_time = Sys.time()

# Computation time = 
end_time - start_time

# clustering coefficient
transitivity(GraphNetwork)

# Algorithm used for clustering
algorithm(clusternet)

# hierarchy level at which each node cuts out
cut_at(clusternet,steps = 5)

# Total communities found = 
max(membership(clusternet))

# Communities = 
communities(clusternet)

#modularity
modularity(clusternet)

# Suggested no. of communities for maximum modularity = 
length(cluster_optimal(GraphNetwork))

# Suggested modularity = 
modularity(cluster_optimal(GraphNetwork))