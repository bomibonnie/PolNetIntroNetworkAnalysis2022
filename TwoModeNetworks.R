##################################################################
#  Name:   R script for two-mode networks                        #
#  Date:   June 13, 2022                                         #
#  Author: Bomi Lee (bomi.lee@uky.edu)                           #
#  Purposes: Create two-mode networks                            #
#  Input: twomodeedge.csv                                        #
##################################################################

library(igraph)

## Load the data
edge <- read.csv("twomodeedge.csv", row.names = 1)

## Change to a matrix
edge2 <- as.matrix(edge)

## to an igraph object
net2 <- graph_from_incidence_matrix(edge2)
net2.bp <- bipartite.projection(net2) ## make it two-mode

## Make the "events" distict from actors
V(net2)$type ## Events and actors are distinquished in type

V(net2)$shape <- c("square", "circle")[V(net2)$type+1]
V(net2)$color <- c("gray", "white")[V(net2)$type+1]
V(net2)$lcolor <- c("navy", "black")[V(net2)$type+1]

V(net2)$size <- c(1, 0.9)[V(net2)$type+1]
V(net2)$vsize <- c(8, 5)[V(net2)$type+1]

## Plot it
windows()
plot(net2, vertex.label.color=V(net2)$lcolor, 
     vertex.label.cex=V(net2)$size,
     vertex.size=V(net2)$vsize,
     vertex.frame.color="white") 