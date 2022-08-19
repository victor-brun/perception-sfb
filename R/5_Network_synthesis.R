# Creating network from the synthesis of answers
# Started: 15/06/2021

rm(list=ls()) ; library(GGally) ; library(geomnet) ; library(ggplot2) ; library(ggrepel) ; 
library(fishualize) ; library(igraph)  ; library(dplyr)  ; library(network) 
library(sna) ; library(intergraph) ; library(abind) ; library(tidyr) ; library(questionr)

df <-  read.csv("output/general_matrix_synthesis.csv")

# PLOTTING BIPARTITE NETWORK -----

# Creating two dataframes: an adjacency matrix and a df with information about the nodes
links <-  df %>%
  select(!c(Session, Sex, Residence, Age, Job_1, Job_2, Fisherman, Group, ngo_knowledge))
nodes <-  read.csv("data/nodes2.csv", sep=";")
nodes$Knowledge = as.factor(nodes$Knowledge)

# Edit links to have id as row names
links <- links %>% 
  tibble::column_to_rownames(var = "ID")

# Creating bipartite network
bip <-  network(links,
              matrix.type = "bipartite", directed = FALSE)

# Number of nodes etc for sizes
deg <- degree(bip)
deg <- deg/2

col = c("actor" = "grey", "event" = "gold")

# Get data to plot
data_ggnet <- ggnet2(bip, mode = "fruchtermanreingold", 
                     color = "mode" , palette = col, 
                     size = as.numeric(deg), edge.alpha = 0.5, edge.size = 0.01, 
                     label = TRUE, label.size = 3)$data %>%
  select(ID = label, x, y) %>%
  left_join(nodes) %>%
  left_join(df[,1:8]) %>%
  mutate(degree = deg)

d <- as.matrix(bip, matrix.type="edgelist") #This gives me a list of edges
colnames(d)<-c("id1","id2") # I name them for my convenience.

#Get a list of vertices:
mynodes<-attr(d,"vnames")
mynodes1<-as.data.frame(cbind(names1=mynodes, id=seq(1:length(mynodes)))) #Notice that the list of edges does not have names in it, just the numeric identifies, so I #want to have a list of both numerical identifiers and names.
mynodes<-as.data.frame(cbind(names=mynodes, id=seq(1:length(mynodes)),data_ggnet)) #I also want to add position coordinates.

#Add names to edgelist:
mynet<-merge(d,mynodes1, by.x="id1", by.y="id")
mynodes2<-mynodes1
colnames(mynodes2)<-c("names2","id")
mynet<-merge(mynet,mynodes2, by.x="id2", by.y="id")
mynet<-mynet[,c(3,4,2,1)]

#Combine edges and nodes into an object suitable for geomnet:
mynet1<-fortify(as.edgedf(mynet), mynodes) #You need to provide a list of edges and a list of nodes.

#Make a graph:
p <- ggplot(data = mynet1) + 
  # only network lines
  geom_net(aes(from_id = from_id, 
               to_id = to_id, x = x, y = y),
           labelon = FALSE,
           repel = TRUE,
           layout.alg = NULL,
           ecolour="light grey", 
           ealpha = 0.5, alpha = 0.1,
           size = 0.025)+
  ylab("") + xlab("") +
  # network nodes points seperate
  geom_point(aes(x = x, y = y, size = degree, 
                 shape = Type, color = Knowledge),
             data = mynodes, alpha = 0.5) +
  # only labels of more important answers 
  geom_text_repel(aes(x = x, y = y, label = names),
                  data = mynodes[mynodes$degree >= 1 &
                                   mynodes$Type == "Answer",], 
                  size = 3, max.overlaps = 200) +
  scale_x_discrete(expand=c(0.1,0.1,0.1,0.1)) +
  scale_y_discrete(expand=c(0.1,0.1,0.1,0.1)) +
  scale_color_fish_d(option = "Scarus_quoyi", na.value = "black") +
  theme_net()
p

## BIPARTITE PROJECTION -------- ---------
mynet.bp <- graph_from_incidence_matrix(links)
mynet.bp <- bipartite.projection(mynet.bp)

# Plotting two projections to test
plot(mynet.bp$proj1, vertex.label.color="black", vertex.label.dist=1)
plot(mynet.bp$proj2, vertex.label.color="black", vertex.label.dist=1)

# BIP. PROJECTION 1 (Persons): creating dataframes of nodes and edges ----
proj1.edges = igraph::as_data_frame(mynet.bp$proj1, what="edges")
proj1.nodes = igraph::as_data_frame(mynet.bp$proj1, what="vertices")

# Rename colum name of nodes to match "name" for left_join
proj1.nodes = proj1.nodes %>%
  rename(names = name)
# Rename column names of edges to match "from node" and "to node"
proj1.edges = proj1.edges %>%
  rename(names1 = from,
         names2 = to)
# Left_join to add information about the nodes
proj1.nodes = proj1.nodes %>%
  left_join(mynodes, by = "names")

# Creating object suitable for ggnet
proj1 <- fortify(as.edgedf(proj1.edges), proj1.nodes)

# Plotting projection 1
p1 <- ggplot(proj1) + 
  geom_net(aes(from_id = from_id, 
               to_id = to_id, x = x, y = y),
           labelon = FALSE,
           #repel = TRUE,
           layout.alg = NULL,
           ecolour="grey", 
           ealpha = 0.25, alpha = 0.1,
           size = 0.01) +
  ylab("") + xlab("") +
  geom_point(aes(x = x, y = y, color = Knowledge, size = degree),
             alpha = 0.5, shape=2) +
  scale_color_fish_d(option = "Scarus_quoyi") +
  theme_net()
p1


# BIP. PROJECTION 2 (Answers): creating dataframes of nodes and edges ------
proj2.edges = igraph::as_data_frame(mynet.bp$proj2, what="edges")
proj2.nodes = igraph::as_data_frame(mynet.bp$proj2, what="vertices")

# Rename colum name of nodes to match "name" for left_join
proj2.nodes = proj2.nodes %>%
  rename(names = name)
# Rename column names of edges to match "from node" and "to node"
proj2.edges = proj2.edges %>%
  rename(names1 = from,
         names2 = to)
# Left_join to add information about the nodes
proj2.nodes = proj2.nodes %>%
  left_join(mynodes, by = "names")

# Creating object suitable for ggnet
proj2 <- fortify(as.edgedf(proj2.edges), proj2.nodes)

# Plotting projection 2
p2 <- ggplot(proj2) + 
  geom_net(aes(from_id = from_id, 
               to_id = to_id, x = x, y = y),
           labelon = FALSE,
           layout.alg = NULL,
           ecolour="grey", 
           ealpha = 0.25, alpha = 0.1,
           size = 0.01) +
  ylab("") + xlab("") +
  geom_point(aes(x = x, y = y, color = Category, size = degree),
             alpha = 0.5) +
  geom_text_repel(aes(x = x, y = y, label = ID),
                  data = proj2.nodes[proj2.nodes$degree >= 1,], 
                  size = 2,
                  alpha = 0.4) +
  scale_color_fish_d(option = "Scarus_quoyi") +
  theme_net()
p2



# PROJECTION 1 (RESPONDENTS)
# Detecting communities in bipartite projection
fc <- fastgreedy.community(mynet.bp$proj1)
V(mynet.bp$proj1)$community <- fc$membership

# Creating df linking group and nodes ID
nodes <- data.frame(ID = V(mynet.bp$proj1)$name, group = V(mynet.bp$proj1)$community)
nodes$group = as.factor(as.character(nodes$group))

# Left join to add to proj1 nodes information
proj1.nodes = proj1.nodes %>%
  left_join(nodes, by = "ID")
proj1 <- fortify(as.edgedf(proj1.edges), proj1.nodes)

# Plotting bipartite projection 1 (respondents) + community detection
bip_proj_1 <- ggplot(proj1) + 
  geom_net(aes(from_id = from_id, 
               to_id = to_id, x = x, y = y),
           labelon = FALSE,
           #repel = TRUE,
           layout.alg = NULL,
           ecolour="grey", 
           ealpha = 0.1, alpha = 0.1,
           size = 0.005) +
  ylab("") + xlab("") +
  geom_point(aes(x = x, y = y, color = Knowledge, shape = group),
             alpha = 1, size = 8) +
  scale_color_fish_d(option = "Ostracion_cubicus") +
  theme_net()
bip_proj_1
