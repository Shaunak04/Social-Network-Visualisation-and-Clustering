library("igraph")
library("igraphdata")
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

# Centrality measures
between.met = betweenness(GraphNetwork)


# plot(edge.g.network, GraphNetwork, edge.arrow.size = 0.25, main = "Edge Betweenness Community")
EdgeNetwork = edge.betweenness.community(GraphNetwork)
EdgeNetwork
str(EdgeNetwork)
is_hierarchical(EdgeNetwork)


clusternet = cluster_edge_betweenness(net)
dendrogramNetwork = as.dendrogram(clusternet)
plot(dendrogramNetwork, main = "Dendrogram for Edge Betweenness Method")
plot(clusternet, net,vertex.size=13,main = "Edge Betweenness Community",vertex.label.cex=0.7,edge.arrow.distance=1, edge.color = "black", layout = layout.kamada.kawai) 


end_time = Sys.time()

# Computation time = 
end_time - start_time


# Algorithm used for clustering
algorithm(clusternet)

# hierarchy level at which each node cuts out
cut_at(clusternet,steps = 5)

# Total communities found = 
max(membership(clusternet))

# Communities = 
communities(clusternet)

modularity(clusternet)

# Suggested no. of communities for maximum modularity = 
length(cluster_optimal(GraphNetwork))

# Suggested modularity = 
modularity(cluster_optimal(GraphNetwork))
