library("igraph")
setwd("C:\\Users\\shaun\\OneDrive\\Desktop\\FOSSEE\\codes")

start_time = Sys.time()

# Note: for privacy purposes, the real names were removed from the dataset and each user was assigned a number
# Loading the dataset
network = read.csv("data.csv", header = FALSE, sep = " ", col.names = c("Source", "Target"))
y = data.frame(network$Source, network$Target)
net = graph.data.frame(y,directed=F)
net = simplify(net)

# converting data to igraph syntax/format
GraphNetwork = graph.data.frame(network, directed = F)
GraphNetwork = connect(GraphNetwork, 100, mode = c("all", "out", "in", "total"))

# Relationship per person
#table(degree(GraphNetwork))


#FASTGREEDY ALGORITHM
undirectedGraphNetwork = as.undirected(GraphNetwork)
E(undirectedGraphNetwork)
undirectedGraphNetwork = simplify(undirectedGraphNetwork)
FSGraphNetwork = fastgreedy.community(undirectedGraphNetwork)
plot(as.dendrogram(FSGraphNetwork), main = "Dendrogram for Fast Greedy community")


clusternet = cluster_fast_greedy(net)
#plot(clusternet, net,vertex.size=13,vertex.label.cex=0.7,edge.arrow.distance=1, edge.color = "black", main = "Fast Greedy Community",layout = layout.fruchterman.reingold) 

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

# modularity of this clustered network = 
modularity(clusternet)

# optimal suggested modularity = 
length(cluster_optimal(GraphNetwork))
