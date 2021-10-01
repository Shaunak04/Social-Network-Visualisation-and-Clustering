library("igraph")
setwd("C:\\Users\\shaun\\OneDrive\\Desktop\\FOSSEE\\codes")

start_time = Sys.time()
# Note: for privacy purposes, the real names were removed from the dataset and each user was assigned a number

# Loading the dataset
network <- read.csv("data.csv", header = FALSE, sep = " ", col.names = c("Source", "Target"))
y = data.frame(network$Source, network$Target)
net = graph.data.frame(y,directed=F)

GraphNetwork = graph.data.frame(network, directed = F)
GraphNetwork = connect(GraphNetwork, 100, mode = c("all", "out", "in", "total"))

edges = as.data.frame(network)

InfomapNetwork = infomap.community(GraphNetwork)
is_hierarchical(InfomapNetwork)
clusternet <- cluster_infomap(net)
#plot(clusternet, net,vertex.size=13,vertex.label.cex=0.7,edge.arrow.distance=1, edge.color = "black", main = "Infomap Community",layout = layout.fruchterman.reingold) 

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

#modularity
modularity(clusternet)

# Suggested no. of communities for maximum modularity = 
length(cluster_optimal(GraphNetwork))

# Suggested modularity = 
modularity(cluster_optimal(GraphNetwork))
