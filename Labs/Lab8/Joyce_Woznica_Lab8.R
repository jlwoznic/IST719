#
# Author: Joyce Woznica
# Class: IST 719
# Date: 3/2/2020
# Subject: Lab 8, Week 8
#
# Lab 8
#---------------- Package Load -------------------
library (igraph)

link.data <- read.csv("/Users/joycewoznica/IST719/Labs/Lab8/links-421-719network.csv",
                      header = TRUE, stringsAsFactors = FALSE)
node.data <- read.csv("/Users/joycewoznica/IST719/Labs/Lab8/nodes-421-719network.csv",
                      header = TRUE, stringsAsFactors = FALSE)
View(node.data)
View(link.data)
dim(node.data)

colnames(link.data)
colnames(link.data) <- gsub("\\.", "", colnames(link.data))
link.data$X <- gsub(" |-", "", link.data$X)
# column bind
cbind(link.data$X, colnames(link.data)[-1])

node.data$Name <- gsub(" |-", "", node.data$Name)
# column bind
cbind(node.data$Name, link.data$X)

# matrix
M <- as.matrix(link.data[ ,-1])
rownames(M) <- colnames(M)
dim(M)
M[is.na(M)] <- 0
M[M > 1]

#----------------- Graphing -------------------------
g <- graph_from_adjacency_matrix(M)
g
vcount(g)
ecount(g)
plot.igraph(g)

g <- simplify(g)
plot.igraph(g)

# shrink margins
par(mar = c(0,0,0,0))
plot.igraph(g)
# supress some information
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)

E(g)$arrow.size <- 0
E(g)$arrow.width <- 0
plot.igraph(g)

# add some more attributes
V(g)$color <- "gold"
V(g)$frame.color <- "white"
V(g)$label.color <- "black"
E(g)$color <- "cadetblue"
V(g)$size <- 5
plot.igraph(g)

?igraph.plotting
# JoHunter is an isolate
E(g)$curved <- 0.4
plot.igraph(g)

#---------------- Visualizing Centrality and Measuring Centrality ---------------------
plot(degree(g))
par(mar=c(3,10,1,1))
barplot(degree(g), horiz = T, las = 2) 
# mine does not show up sorted most to least like Helmsley

V(g)$degree <- degree(g)
V(g)$deg.out <- degree(g, mode = "out")
V(g)$deg.in <- degree(g, mode = "in")

# the top is the people that nominated the most to be their friends
barplot(V(g)$deg.out, names.arg = V(g)$name,
        horiz = T, las = 2)
barplot(V(g)$deg.in, names.arg = V(g)$name,
        horiz = T, las = 2)

#g.bak <- g
#g <- as.undirected(g)
#g <- g.bak

V(g)$close <- closeness(g, normalized = TRUE, mode = "all")
V(g)$bet <- betweenness(g, normalized = FALSE)

library(plotrix)
my.pallet <- colorRampPalette(c("steelblue1", "violet", "tomato", "red", "red"))

V(g)$color <- rev(my.pallet(200))[round(1 + rescale(V(g)$close, c(1,199)), 0)]
plot.igraph(g)

V(g)$size <- 2 + rescale(V(g)$degree, c(0,13))
V(g)$label.cex <- 0.7 + rescale(V(g)$bet, c(0,1.25))

plot.igraph(g)

#--------------- Visualzing Social Network Structures -------------------
cbind(V(g)$name, node.data$Name)

V(g)$class <- node.data$Class
V(g)$country <- node.data$Country
V(g)$year <- node.data$year

g <- delete_vertices(g, "JoHunter")

plot.igraph(g)

V(g)$shape <- "circle"
V(g)$shape[V(g)$class == "Wednesday"] <- "square"
V(g)$shape[V(g)$class == "Both"] <- "rectangle"

plot.igraph(g)

V(g)$color <- "gold"
V(g)$color[V(g)$country == "India"] <- "springgreen4"
V(g)$color[V(g)$country == "China"] <- "red"
V(g)$color[V(g)$class == "Both"] <- "purple"

plot.igraph(g)

V(g)$label.color <- "blue"
V(g)$label.color[V(g)$year == 1] <- "black"
plot.igraph(g)

fc <- cluster_fast_greedy(as.undirected(g))
print(modularity(fc))

membership(fc)
V(g)$cluster <- membership(fc)
length(fc)
sizes(fc)

par(mar = c(0,0,0,0))
plot_dendrogram(fc, palette = rainbow(7))
# this plot is nothing like the one he provided. 
# there are boxes in color instead of colored words

#--------------- Advanced Social Networking Topics --------------------
load("/Users/joycewoznica/IST719/Labs/Lab8/ist719networkobject.rda")
par(mar = c(0,0,0,0))
plot.igraph(g)

l <- layout_in_circle(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

l <- layout_with_fr(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

l <- layout_as_star(g, center = "LeelaDeshmukh")
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

E(g)$color <- "gray"
E(g)[from("LeelaDeshmukh")]$color <- "red"
l <- layout_as_star(g, center = "LeelaDeshmukh")
plot.igraph(g)

l <- layout_with_kk(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

V(g)$x <- 0
V(g)$y <- 0
plot.igraph(g)

coord <- cbind(V(g)$x, V(g)$y)
iteratation <- c(500, 100, 20, 10, 5, 3, 2, 1)
for (i in 1:length(iteratation))
{
  l <- layout_with_fr(g, coords = coord, dim = 2, niter = iteratation[i])
  V(g)$x <- l[,1]
  V(g)$y <- l[,2]
  plot.igraph(g)
  mtext(paste("Layout FR:", iteratation[i]), side = 3, line = 0, 
        cex = 1.5, adj = 0)
}

l <- layout_with_gem(g)
V(g)$x <- l[ ,1]
V(g)$y <- l[ ,2]
plot.igraph(g)

l <- layout_with_dh(g)
V(g)$x <- l[ ,1]
V(g)$y <- l[ ,2]
plot.igraph(g)

l <- layout_on_grid(g)
V(g)$x <- l[ ,1]
V(g)$y <- l[ ,2]
plot.igraph(g)


#------------- Barpartited Network -------------------------------
# use g to simiulate a barpartited network

my.linked.list <- data.frame(person = V(g)$name, event = V(g)$country)
g <- graph_from_data_frame(my.linked.list, directed = FALSE)

V(g)$type <- FALSE
V(g)$type[V(g)$name %in% node.data$Name] <- TRUE

l <- layout_as_bipartite(g, types = V(g)$type)
V(g)$x <- l[ ,2]
V(g)$y <- l[ ,1]
par(mar = c(0,0,0,0))
plot.igraph(g)
V(g)$size <- 0
plot.igraph(g)







