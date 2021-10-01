library("igraph")
setwd("C:\\Users\\shaun\\OneDrive\\Desktop\\FOSSEE\\codes")

start_time = Sys.time()
# Note: for privacy purposes, the real names were removed from the dataset and each user was assigned a number

# Loading the dataset
network = read.csv("data.csv", header = FALSE, sep = " ", col.names = c("Source", "Target"))
y = data.frame(network$Source, network$Target)
net = graph.data.frame(y,directed=F)

# converting data to igraph syntax/format
GraphNetwork = graph.data.frame(network, directed = T)
GraphNetwork = connect(GraphNetwork, 100, mode = c("all", "out", "in", "total"))

# Relationship per person
#table(degree(GraphNetwork))

# Leading Eigen vector ALGORITHM
undirectedGraphNetwork = as.undirected(GraphNetwork)
E(undirectedGraphNetwork)
undirectedGraphNetwork = simplify(undirectedGraphNetwork)
cl.GraphNetwork = leading.eigenvector.community(undirectedGraphNetwork)
is_hierarchical(cl.GraphNetwork)
str(cl.GraphNetwork)
dendrogram_network = as.dendrogram(cl.GraphNetwork)
plot(dendrogram_network, main = "Dendrogram for Eigenvector Community")


clusternet = cluster_leading_eigen(net)
#plot(clusternet, net,vertex.size=13,main = "Leading Eigenvector Community",vertex.label.cex=0.7,edge.arrow.distance=1, edge.color = "black", layout = layout.auto) 

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

#modularity = 
modularity(clusternet)

# Suggested no. of communities for maximum modularity = 
length(cluster_optimal(GraphNetwork))

# Suggested modularity = 
modularity(cluster_optimal(GraphNetwork))
