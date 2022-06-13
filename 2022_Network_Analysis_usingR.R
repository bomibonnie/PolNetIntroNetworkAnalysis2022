##################################################################
#  Name:   Introduction to Network Analysis using R              #
#  Date:   June 13, 2022                                         #
#  Author: Bomi Lee (bomi.lee@uky.edu)                           #
#  Purpose: Create network plots                                 #
#           Calculate statistics using network data              #
#  Thanks to Dr.Elizabeth Menninga for some of this code.        #
##################################################################

# Install and load needed packages

#install.packages("igraph")
#install.packages("statnet")
#install.packages("signnet")
#install.packages("rio")


library(igraph)
library(rio)

# Set Working Directory
getwd()
#setwd("C:/Users/bomim/Documents/PolNetIntroNetworkAnalysis2022")


############Session 1############
# Load and Plot Data

node <- import("nodeList.csv")
edge <- import("edgeList.csv")

View(node)
View(edge)

# igraph (https://igraph.org/r/doc/)
## Directed, unweighted network plot

net_igraph<-graph_from_data_frame(d=edge, v=node, directed=T)
net_igraph 

windows()
plot(net_igraph,
     edge.arrow.size=.5,
     edge.curved=0.2,
     edge.color="black",
     edge.width=1.5,
     vertex.size=5, 
     vertex.frame.color="gray",
     vertex.color="white",
     vertex.label.cex=1,
     vertex.label.dist=1.2,
     vertex.label.color="black")

windows()
plot(net_igraph,
     edge.arrow.size=.5,
     #edge.curved=0.2,
     edge.color="black",
     edge.width=1.5,
     vertex.size=5, 
     vertex.frame.color="gray",
     vertex.color="white",
     vertex.label.cex=1,
     vertex.label.dist=1.2,
     vertex.label.color="black")

## Undirected, unweighted
?graph_from_data_frame
net_igraph2<-graph_from_data_frame(d=edge, v=node, directed=F)
net_igraph2 # duplicates

windows()
plot(net_igraph2,
     edge.color="black",
     edge.width=1.5,
     vertex.size=5, 
     vertex.frame.color="gray",
     vertex.color="white",
     vertex.label.cex=1,
     vertex.label.dist=1.2,
     vertex.label.color="black")

net_igraph3 <- as.undirected(net_igraph, mode='collapse')
net_igraph3

windows()
plot(net_igraph3,
     edge.color="black",
     edge.width=1.5,
     vertex.size=5, 
     vertex.frame.color="gray",
     vertex.color="white",
     vertex.label.cex=1,
     vertex.label.dist=1,
     vertex.label.color="black")

## Directed, Weighted 
net_igraph
E(net_igraph)$Weight

### Add vertex/edge attributes
V(net_igraph)$gender <- c("M","F","F","M","M","M")
V(net_igraph)$gender

E(net_igraph)
E(net_igraph)$color <- ifelse(E(net_igraph)$Weight==1, "darkred", "navy") 

windows()
plot(net_igraph,
     edge.width=1.5*E(net_igraph)$Weight,
     edge.arrow.size=.6,
     edge.curved=0.2,
     edge.color=E(net_igraph)$color,
     vertex.size=8, 
     vertex.frame.color="white", 
     vertex.color=ifelse(V(net_igraph)$gender=="M", "gray", "black"),
     vertex.label.cex=1.5,
     vertex.label.dist=1.5)

## Undirected, Weighted 
net_igraph2 ## not collapsed
E(net_igraph2)$Weight

### Add vertex/edge attributes
V(net_igraph2)$gender <- c("M","F","F","M","M","M")
V(net_igraph2)$gender

E(net_igraph2)
E(net_igraph2)$color <- ifelse(E(net_igraph2)$Weight==1, "darkred", "navy") 

windows()
plot(net_igraph2,
     edge.width=1.5*E(net_igraph)$Weight,
     #edge.arrow.size=.6,
     #edge.curved=0.2,
     edge.color=E(net_igraph2)$color,
     vertex.size=8, 
     vertex.frame.color="white", 
     vertex.color=ifelse(V(net_igraph2)$gender=="M", "gray", "black"),
     vertex.label.cex=1.5,
     vertex.label.dist=1.5)


## Real Data: Defensive Alliance (ATOP)
atop <- import("atop_sample2.csv")

head(atop)
atop1997_dat <- subset(atop, year==1997, c(stateabb1, stateabb2))

atop1997 <- graph.data.frame(atop1997_dat)
atop1997und <- as.undirected(atop1997, mode='collapse')  # Undirected, unweighted

windows()
plot(atop1997und,
     vertex.label=NA,
     edge.width=1.5, 
     vertex.size=2.5, 
     vertex.color="blue",
     layout=layout_with_kk)

windows()
plot(atop1997und,
     #vertex.label=NA,
     vertex.label.cex=0.6, 
     vertex.label.color="black",
     vertex.label.dist=1,
     edge.width=1.5, 
     vertex.size=2.5, 
     vertex.frame.color="black", 
     vertex.color="blue",
     layout=layout_with_kk)


# statnet (http://statnet.org/Workshops/ergm_tutorial.html#the_statnet_project)
detach("package:igraph", unload = TRUE)
library(statnet)
head(edge)

?network
net_statnet <- network(edge, matrix.type="edgelist") 
#matrix.type="adjacency"
#directed=T

net_statnet
class(net_statnet)

windows()
plot(net_statnet, 
     displaylabels=T)

## Undirected
net_statnet2 <- network(edge, 
                        matrix.type = "edgelist",
                        directed = FALSE) ## Error due to duplicates 

net_statnet3 <- network(edge, 
                        matrix.type = "edgelist",
                        directed = FALSE,
                        multiple = TRUE)

net_statnet3

## Weights

net_statnet%e%"Weight"

windows()
plot(net_statnet,
     displaylabels=T,
     edge.lwd=5*net_statnet%e%"Weight") 

netweighted<-as.network(edge, 
                     matrix.type="edgelist",
                     ignore.eval=FALSE)

netweighted
#ignore.eval: logical; ignore edge values?
#names.eval: optionally, the name of the attribute in which edge values should be stored

windows()
plot(netweighted,
     displaylabels=T,
     edge.lwd=5*netweighted%e%"Weight")

netweighted[,] #adjacency matrix without weight
as.sociomatrix.sna(netweighted,"Weight")
#as.sociomatrix: Coerce One or More Networks to Sociomatrix Form

### Add vertex/edge attributes
netweighted %v% "gender" <- c("M","F","F","M","M","M")
netweighted %v% "gender"

netweighted %v% "vertex.names"

netweighted %e% "color" <- ifelse(netweighted %e% "Weight"==1, "darkred",
                                  "navy")
netweighted %e% "color"

windows()
plot(netweighted,
     displaylabels=T,
     edge.lwd=5*netweighted%e%"Weight",
     edge.col=netweighted%e%"color",
     vertex.col=ifelse(netweighted%v%"gender"=="F","black","gray"))



############Session 2############
# Calculate Network Statistics

## Using statnet (count edges & triangles)
summary(net_statnet ~ edges)
atopnet<-network(atop1997_dat, matrix.type="edgelist")
summary(atopnet ~ triangles)

## Using igraph
detach("package:statnet", unload = TRUE)
library(igraph)

net_igraph<-graph_from_data_frame(d=edge, v=node, directed=T)

## Dyad
windows()
plot(net_igraph)
summary(net_igraph)

?dyad.census
#mut: The number of pairs with mutual connections.
#asym: The number of pairs with non-mutual connections.
#null: The number of pairs with no connection between them.

dyad.census(net_igraph)

## Triads
?triad.census
triad.census(net_igraph)
### E.g.030T: a->e<-b, a->b
###           a->d<-b, a->b

## Undirected
net_igraph3 <- as.undirected(net_igraph, mode='collapse')

dyad.census(net_igraph3) # number of ties
triad.census(net_igraph3) 

## Comparing density
atop1997 <- graph.data.frame(atop1997_dat)
atop1997und <- as.undirected(atop1997, mode='collapse')

atop2002_dat <- subset(atop, year==2002, c(stateabb1, stateabb2))
atop2002 <- graph.data.frame(atop2002_dat)
atop2002und <- as.undirected(atop2002, mode='collapse')

atop1997und 
atop2002und 

edge_density(atop1997und)
edge_density(atop2002und)

# Centrality
## Degree - Number of adjacent ties for a node
?degree

degree(net_igraph,
       mode = "in")

degree(net_igraph,
       mode = "out")

degree(net_igraph,
       mode = "total")

### ATOP
windows()
plot(atop1997und,
     #vertex.label=NA,
     vertex.label.cex=0.6, 
     vertex.label.color="black",
     vertex.label.dist=1,
     edge.width=1.5, 
     vertex.size=2.5, 
     vertex.frame.color="black", 
     vertex.color="blue",
     layout=layout_with_kk)

degree(atop1997und)

### Eigenvector (undirected)
eigen_centrality(atop1997und)$vector

### Betweenness (directed or undirected)
betweenness(net_igraph)
betweenness(atop1997und)

### Closeness (directed or undirected)
closeness(net_igraph)
closeness(atop1997und)


############Session 3############

## Networks including both positive and negative ties
## More details from https://mr.schochastics.net/project/signnet/

library(signnet)

pndata <- import("pndata.xlsx")
pnnet <- graph_from_data_frame(pndata, directed=F)

pnnet
E(pnnet)$sign

windows()
plot(pnnet,
     vertex.label.color="black",
     vertex.frame.color="black", 
     vertex.color="gray",
     edge.width=2,
     edge.color=ifelse(E(pnnet)$sign==-1, "darkred", "navy"))

### Number of triangles
count_signed_triangles(pnnet)

### Balanced?
balance_score(pnnet, method = "triangles") ##Aref&Wilson (2018):0 to 1

## Degree centrality
degree_signed(pnnet, type = "pos")
degree_signed(pnnet, type = "neg")
degree_signed(pnnet, type = "ratio")
degree_signed(pnnet, type = "net")


## Positive and negative centrality
pn_index(pnnet) # range: -1 to 2

## All positive ties
pnnet2 <- graph_from_data_frame(pndata, directed=F)
E(pnnet2)$sign <- 1
pn_index(pnnet2) # range: 1 to 2

## All negative ties
pnnet3 <- graph_from_data_frame(pndata, directed=F)
E(pnnet3)$sign <- -1
pn_index(pnnet3) # range: 0 to 1

## Real data: rivalry and alliance in the Middle East

pnnet_me <- import("rivdefme1980.csv")
head(pnnet_me)

pnnet_1980 <-graph_from_data_frame(d=pnnet_me, directed=F)
E(pnnet_1980)$color <- ifelse(E(pnnet_1980)$sign==-1, "darkred", "navy") 

windows()
plot(pnnet_1980,
     edge.color=E(pnnet_1980)$color,
     edge.width=1.5,
     vertex.size=4, 
     vertex.frame.color="gray", 
     vertex.color="white",
     vertex.label.cex=0.8, 
     vertex.label.color="black",
     vertex.label.dist=1,
     layout=layout_with_kk,
     main="1980")

### Number of triangles
count_signed_triangles(pnnet_1980)

### Balanced?
balance_score(pnnet_1980, method = "triangles") ##Aref&Wilson (2018):0 to 1

## Degree centrality
degree_signed(pnnet_1980, type = "pos")
degree_signed(pnnet_1980, type = "neg")
degree_signed(pnnet_1980, type = "ratio")
degree_signed(pnnet_1980, type = "net")


## Positive and negative centrality
pn_index(pnnet_1980) # range: -1 to 2

##### Q&A

##### matrix to network
