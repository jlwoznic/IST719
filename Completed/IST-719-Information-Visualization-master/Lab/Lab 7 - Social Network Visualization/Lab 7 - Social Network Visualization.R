# Author: Pan Chen
# Purpose: IST719 Lab7 - Social network, json data, word cloud and scaling data
#

class.network.fname<-file.choose()
class.data<- read.csv(class.network.fname,header=TRUE,stringsAsFactors = FALSE)

colnames(class.data)
class(class.data)

#install igraph package
M <- as.matrix(class.data[,c(3:42)])
rownames(M) <- class.data$Name
g<- graph_from_adjacency_matrix(M)
plot(g)#the layout itself has meaning that reflects something about thte data, so there are 3 dimensions


vcount(g) #how many people(nodes, vertices)
ecount(g) #how many friendships are there (#links, edges, ties)

plot(degree(g))
deg.1 <- degree(g)
par(mar=c(5,15,4,2))
barplot(sort(deg.1),horiz=T,las=2,border=NA,col="darkgreen",main="most links")


deg.2<-degree(g.mode="in")
barplot(sort(deg.2),horiz=T,las=2,border.NA,col="darkorange",main="Popular")

deg.3<-degree(g,mode="out")
barplot(sort(deg.3),horiz=T,las=2,border=NA,col="darkorchid",main="Friendly")

bet<- betweenness(g)

par(mar=c(1,1,1,1))
plot.igraph(g,edge.arrow.size=0,edge.arrow.width = 0,main="ist 719 - Tues & Wed")
#at least three dimensions in this picture

plot(bet)
my.size<-bet
par(mfrow=c(1,2))
plot(sort(bet),main="raw data",type="l")
plot(sort(my.size^(1/5)),main="sqrt scaled",type="l")


par(mar=c(1,1,1,1),mfrow=c(1,1))
V(g)$size<-3+(2*my.size^(1/5))
V(g)$color<-rgb(100,149,237,alpha=160,maxColorValue = 255)
V(g)$color[class.data$Day=="t"]<-rgb(165,42,42,alpha=160,maxColorValue = 255)
V(g)$color[class.data$Day=="B"]<-rgb(138,43,226,alpha=160,maxColorValue = 255)

plot.igraph(g,edge.arrow.size=0,edge.arrow.width = 0,main="ist 719 - Tues & Wed")

coords<-layout_with_kk(g,dim=3)
rglplot(g,layout=coords)
rgl.snapshot(filename = "/Users/cpkoywk/Library/Mobile Documents/com~apple~CloudDocs/IST719/lab7.png")

