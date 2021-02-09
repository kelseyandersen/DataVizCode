## ---- results='hide', message=FALSE----------------------------------------------------------------------------
library(igraph)
library(ggplot2)


## --------------------------------------------------------------------------------------------------------------
mat1 <- matrix(c(0, 1, 0, 0, 0, 1, 1,0, 0), 
               nrow=3, ncol=3) ### matrix function 
mat1


## --------------------------------------------------------------------------------------------------------------
mat2 <- graph_from_adjacency_matrix(mat1)
plot(mat2, edge.arrow.size = 1) ## set the size of the arrows 


## --------------------------------------------------------------------------------------------------------------
mat3 <- graph(edges=c(1,3, 3,2, 2,1), n=3, directed=T ) # use graph function and list edges 
plot(mat3, edge.arrow.size = 1)


## --------------------------------------------------------------------------------------------------------------
plot(mat3, 
     edge.arrow.size = 1, 
     vertex.color = "purple", 
     vertex.size = 25, 
     vertex.label.cex = 1)



## --------------------------------------------------------------------------------------------------------------
plot(mat3, 
     edge.arrow.size = 1, 
     vertex.color = "purple", 
     vertex.size = 25, 
     vertex.label.cex = 2, 
     vertex.label.dist = 3.5)


## --------------------------------------------------------------------------------------------------------------
plot(mat3, 
     edge.arrow.size = 1, 
     vertex.color = "purple", 
     vertex.size = 25, 
     vertex.label.cex = 2, 
     vertex.label.dist = 3.5, 
     edge.curved = .2)


## --------------------------------------------------------------------------------------------------------------
plot(mat3, 
     edge.arrow.size = 1, 
     vertex.color = "purple", 
     vertex.frame.color="purple",
     vertex.size = 25, 
     vertex.label.cex = 2, 
     vertex.label.dist = 3.5, 
     edge.curved = .2)


## --------------------------------------------------------------------------------------------------------------
plot(mat3, 
     edge.arrow.size = 1, 
     vertex.color = "purple", 
     vertex.frame.color="purple",
     vertex.size = 25, 
     vertex.label.cex = 2, 
     vertex.label.dist = 3.5, 
     edge.curved = .2,
     edge.color="purple")


## --------------------------------------------------------------------------------------------------------------
plot(mat3, 
     edge.arrow.size = 1, 
     vertex.color = "purple", 
     vertex.frame.color="purple",
     vertex.size = 25, 
     vertex.label.cex = 2, 
     vertex.label.dist = 3.5, 
     edge.curved = .2,
     vertex.shape = "square", 
     edge.color="purple")

# shape options are circle, square, 
# csquare, rectangle, crectangle, none


## --------------------------------------------------------------------------------------------------------------
plot(mat3, 
     edge.arrow.size = 1, 
     vertex.color = "purple", 
     vertex.frame.color="purple",
     vertex.size = 25, 
     vertex.label.cex = 2, 
     vertex.label.dist = 3.5, 
     edge.curved = .2,
     vertex.shape = "square",
     edge.lty = 2, 
     edge.color="purple")


## --------------------------------------------------------------------------------------------------------------
V(mat3)$color <- 'purple'
V(mat3)$size <- 25
E(mat3)$color <- 'purple'
E(mat3)$arrow.size <- 1
V(mat3)$frame.color <- 'purple'
plot(mat3)


## --------------------------------------------------------------------------------------------------------------
Nodelist <- data.frame(
                Names =c("Jim", "Carole", "Joe", "Michelle", "Jen", "Pete", "Paul", 
                         "Tim", "Jess", "Mark", "Jill", "Cam", "Kate") ,
                YearsFarming = c(8.5, 6.5, 4, 1, 3, 10, 5, 5, 5, 1, 1, 6, 6) , 
                Age = c(27, 52, 49, 32, 65, 72, 42, 67, 48, 33, 67, 75, 39) , 
                Gender = c("Male", "Female", "Male", "Female", "Female", "Male",
                           "Male","Male", "Female", "Male", "Female", "Male", 
                           "Female"))
Nodelist       


## --------------------------------------------------------------------------------------------------------------
Edgelist <- data.frame(
                  From = c("Jim", "Jim", "Jim", "Jill", "Kate", "Pete", 
                           "Pete", "Jess", "Jim", "Jim", "Pete"),
                  To = c("Carole", "Jen", "Pete", "Carole", "Joe", 
                         "Carole", "Paul", "Mark", "Cam", "Mark", "Tim"),
                  Times =c(3, 7, 6, 6, 5, 3, 2, 1, 1, 2, 5)
)
Edgelist


## --------------------------------------------------------------------------------------------------------------
FarmNetwork <- graph_from_data_frame(d = Edgelist, vertices = Nodelist, directed = T)
FarmNetwork
E(FarmNetwork) # view edges
V(FarmNetwork) # view nodes


## --------------------------------------------------------------------------------------------------------------
plot(FarmNetwork, 
     edge.arrow.size = .5, 
     vertex.color = "plum", 
     vertex.label.dist = 2.5)



## --------------------------------------------------------------------------------------------------------------
V(FarmNetwork)$color <- ifelse(V(FarmNetwork)$Gender == "Male", "#4682B4", "#C83200")
V(FarmNetwork)$frame.color <- ifelse(V(FarmNetwork)$Gender == "Male", "#4682B4", "#C83200")

plot(FarmNetwork, 
     edge.arrow.size = .5, 
     vertex.label.dist = 2.5, 
     edge.curved = .2)



## --------------------------------------------------------------------------------------------------------------
Layout1 <- layout_in_circle(FarmNetwork)

plot(FarmNetwork, 
     edge.arrow.size = .5, 
     vertex.label.dist = 2.5, 
     layout = Layout1)


## --------------------------------------------------------------------------------------------------------------
Layout2 <- layout_as_star(FarmNetwork)

plot(FarmNetwork, 
     edge.arrow.size = .5, 
     vertex.label.dist = 2.5, 
     layout = Layout2)


## --------------------------------------------------------------------------------------------------------------
Layout3 <- layout_with_kk(FarmNetwork)

plot(FarmNetwork, 
     edge.arrow.size = .5, 
     vertex.label.dist = 2.5, 
     layout = Layout3)


## --------------------------------------------------------------------------------------------------------------
Layout4<- layout_with_fr(FarmNetwork)

plot(FarmNetwork, edge.arrow.size = .5, vertex.label.dist = 2.5, layout = Layout4)


## --------------------------------------------------------------------------------------------------------------

V(FarmNetwork)$size <- V(FarmNetwork)$YearsFarming 
                # size the nodes by number of years farming

plot(FarmNetwork, 
     edge.arrow.size = .5, 
     vertex.label.dist = 2.5)


## --------------------------------------------------------------------------------------------------------------

V(FarmNetwork)$size <- V(FarmNetwork)$YearsFarming *2 
      ## scale by multiplying by 2

plot(FarmNetwork, edge.arrow.size = .5, vertex.label.dist = 2.5)


## --------------------------------------------------------------------------------------------------------------
E(FarmNetwork)$width <- E(FarmNetwork)$Times/1.5

plot(FarmNetwork, 
     edge.arrow.size = .7, 
     vertex.label.dist = 2.5, 
     edge.curved = .2, 
     layout = Layout2)


## --------------------------------------------------------------------------------------------------------------
plot(FarmNetwork, 
     edge.arrow.size = .7, 
     vertex.label.dist = 2.5, 
     edge.curved = .2, 
     layout = Layout2)
legend(x=-1.5, 
       y=-1.1, 
       c("Male","Female"), 
       pch=21, 
       pt.bg=c("#4682B4", "#C83200"),
       pt.cex=2, 
       cex=.8, 
       bty="n", 
       ncol=1)


## --------------------------------------------------------------------------------------------------------------
tiff("graph/NetworkPlot.tiff", 
     width=150, height=150, 
     units='mm',res=300)

plot(FarmNetwork, 
     edge.arrow.size = .7, 
     vertex.label.dist = 2.5, 
     edge.curved = .2, 
     layout = Layout2)
legend(x=-1.5, 
       y=-1.1, 
       c("Male","Female"), 
       pch=21, 
       pt.bg=c("#4682B4", "#C83200"),
       pt.cex=2, 
       cex=.8, 
       bty="n", 
       ncol=1)

dev.off()

