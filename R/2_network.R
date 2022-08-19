# Creating networks from the matrix of answers
# Started: 27/07/2020
# Modified: 26/01/2021

rm(list = ls())

library(GGally) ; library(geomnet) ; library(ggplot2) ; library(ggrepel) ; 
library(fishualize) ; library(igraph)  ; library(dplyr)  ; library(network) 
library(sna) ; library(intergraph) ; library(abind) ; library(tidyr)

df = read.csv("output/general_matrix.csv", sep=",")



# FORMAT : creating two groups depending on the village of origin ----
df = df %>%
  mutate(Group = case_when(df$Residence == "Batas" ~ 0,
                           df$Residence == "Mabini" ~ 0,
                           df$Residence == "Silangga" ~ 0,
                           TRUE ~ 1))
# Keeping only local people for analyses
df.locals = df %>%
  filter (Residence %in% c("Batas", "Depla", "Mabini", "Sandoval" ,"Silangga"))

# PLOTTING GLOBAL NETWORK -----

# Creating two dataframes: an adjency matrix and a df with information about the nodes
links = df %>%
  select(!c(Session, Sex, Residence, Age, Job_1, Job_2, Fisherman, Group))
nodes = read.csv("data/nodes.csv", sep=";")

# Deleting "NGO" questions
nodes = nodes %>%
  filter (Category =! "NGO")

# Edit links to have id as row names
links <- links %>% 
  tibble::column_to_rownames(var = "ID")

# Creating bipartite network
bip = network(links,
              matrix.type = "bipartite")

# Number of nodes etc for sizes
deg <- degree(bip)


col = c("actor" = "grey", "event" = "gold")

# Get data to plot
data_ggnet <- ggnet2(bip, color = "mode" , palette = col, 
       size = as.numeric(deg), edge.alpha = 0.5, 
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
               to_id = to_id,x = x, y = y),
           labelon = FALSE,
           #repel = TRUE,
           layout.alg = NULL,
           ecolour="grey", 
           ealpha = 1, alpha = 0.1,
           size = 0.025)+
  ylab("") + xlab("") +
  # network nodes points seperate
  geom_point(aes(x = x, y = y, size = degree, 
                 shape = Type, color = Residence),
             data = mynodes, alpha = 0.5) +
  # only labels of more important answers 
  geom_text_repel(aes(x = x, y = y, label = names),
                  data = mynodes[mynodes$degree >= 30 &
                                   mynodes$Type == "Answer",], 
                  size = 3, max.overlaps = 100) +
  scale_x_discrete(expand=c(0.1,0.1,0.1,0.1)) +
  scale_y_discrete(expand=c(0.1,0.1,0.1,0.1)) +
  scale_color_fish_d(option = "Scarus_quoyi", na.value = "black") +
  theme_net()
p

# Bipartite projection ---------
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
  geom_point(aes(x = x, y = y, color = Residence, size = degree),
             alpha = 0.5) +
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
                  data = proj2.nodes[proj2.nodes$degree >= 20,], 
                  size = 2,
                  alpha = 0.4) +
  scale_color_fish_d(option = "Scarus_quoyi") +
  theme_net()
p2

# ANALYSES OF RESPONSES DEPENDING ON THE GROUP ------

# Creating a new dataframe summing all the responses for each group
df2 = df.locals%>%
  group_by(Group) %>%
  select(9:183) %>%
  summarise_all(sum)
group0 = df2 %>%
  filter(Group == 0) %>%
  `/`(36)
group1 = df2 %>%
  filter(Group == 1) %>%
  `/`(22)
df3 = rbind(group0, group1)
df3[1,1]=0
df3[2,1]=1
rm(group0, group1)


# Statistical tests

# Operating chisq test for every column to compare group 0 (control) and 1 (test)
df.locals2 = df.locals %>%
  select(-c(1:8))
a = vector(mode="list", length=ncol(df.locals2)-1)
for (i in 1:(ncol(df.locals2)-1)) {
  a[[i]] = chisq.test(table(df.locals2[,i], df.locals2$Group))
  a[[i]] = a[[i]]$p.value }

stat = c("stat", abind::abind(a))

df3 = rbind(df3, stat)

df4 = t(df3) %>% 
  data.frame()

colnames(df4) = c("Control group", "Test group", "Stat")
df4$`Control group` = as.numeric(as.character(df4$`Control group`))
df4$`Test group` = as.numeric(as.character(df4$`Test group`))
df4 = df4[-1,]

# Adding to df4 the total number of respondents who gave that answer(degree/2)
df4$ID = rownames(df4)
df4 = left_join(df4, data_ggnet, by="ID")
df4$degree = df4$degree/2
df4 = df4 %>%
  select(ID, `Control group`, `Test group`, Stat, degree, Category)

# Filter when p<=0.1 and more thant one respondent
df5 = df4 %>%
  filter(as.numeric(as.character(Stat))<=0.1, (`Test group`+`Control group`)>0)


# Making a graph displaying only labels of answers where p<=0.1

# Left join to add the p-values to mynodes
df6 = df4 %>%
  select(ID, Stat, `Control group`, `Test group`)
mynodes = left_join(mynodes, df6, by="ID")
mynodes$Stat = as.numeric(as.character(mynodes$Stat))

#Make a graph:
p <- ggplot(data = mynet1) + 
  # only network lines
  geom_net(aes(from_id = from_id, 
               to_id = to_id,x = x, y = y),
           labelon = FALSE,
           #repel = TRUE,
           layout.alg = NULL,
           ecolour="grey", 
           ealpha = 0.25, alpha = 0.1,
           size = 0.025)+
  ylab("") + xlab("") +
  # network nodes points seperate
  geom_point(aes(x = x, y = y, size = degree, 
                 shape = Type, color = Residence),
             data = mynodes, alpha = 0.5) +
  # only labels of answers for which p<=0.1 
  geom_text_repel(aes(x = x, y = y, label = names),
                  data = mynodes[(mynodes$Stat <= 0.1 &
                                   (mynodes$`Test group` + mynodes$`Control group`)>0) |
                                   mynodes$degree>=20 &
                                   mynodes$Type == "Answer",], 
                  size = 3) +
  scale_x_discrete(expand=c(0.1,0.1,0.1,0.1)) +
  scale_y_discrete(expand=c(0.1,0.1,0.1,0.1)) +
  scale_color_fish_d(option = "Scarus_quoyi", na.value = "black") +
  theme_net()
p


# Bipartite projections of answers displaying only labels of answers where p<=0.1

# Left join to add the stat
proj2.nodes = left_join(proj2.nodes, df6, by="ID")
proj2.nodes$Stat = as.numeric(as.character(proj2.nodes$Stat))

# Make graph: 
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
                  data = proj2.nodes[proj2.nodes$Stat <= 0.1 & 
                                       (proj2.nodes$`Test group` + 
                                          proj2.nodes$`Control group`)>0,], 
                  size = 2,
                  alpha = 0.4) +
  scale_color_fish_d(option = "Scarus_quoyi") +
  theme_net()
p2

# Barplots to present the repartition of the most frequent answers 

df7 = df4 %>%
  filter(degree>20)

df8 = pivot_longer(df7, c(`Control group`, `Test group`))

barplot = ggplot(df8) +
  aes(x = ID, y = value) +
  geom_col(aes(fill = name), position = "dodge", width = 0.7) +
  theme_minimal()
barplot + coord_flip()


# Testing the diversity of answers in each group
# Number of answers per group
174-length(which(df6$`Control group`==0)) # 119 individual answers for control group
174-length(which(df6$`Test group`==0)) # 105 individuals answers for test group

# Diversity of answers per group
library(vegan)
diversity(df6$`Control group`, index="shannon") #4.30
diversity(df6$`Test group`, index="shannon") # 4.23
diversity(df6$`Control group`, index="simpson") # 0.9810
diversity(df6$`Test group`, index="simpson") # 0.9806

# COMMUNITY DETECTION --------

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
  geom_point(aes(x = x, y = y, color = group),
             alpha = 1, size = 8) +
  scale_color_fish_d(option = "Ostracion_cubicus") +
  theme_net()
bip_proj_1


# PROJECTION 2 (ANSWERS) -----
# Detecting communities in bipartite projection
fc <- fastgreedy.community(mynet.bp$proj2)
V(mynet.bp$proj2)$community <- fc$membership

# Creating df linking group and nodes ID
nodes <- data.frame(ID = V(mynet.bp$proj2)$name, group = V(mynet.bp$proj2)$community)
nodes$group = as.factor(as.character(nodes$group))

# Left join to add to proj1 nodes informations
proj2.nodes = proj2.nodes %>%
  left_join(nodes, by = "ID")
proj2 <- fortify(as.edgedf(proj2.edges), proj2.nodes)

# Plotting bipartite projection 2 (answers) + community detection
bip_proj_2 <- ggplot(proj2) + 
  geom_net(aes(from_id = from_id, 
               to_id = to_id, x = x, y = y),
           labelon = FALSE,
           #repel = TRUE,
           layout.alg = NULL,
           ecolour="grey", 
           ealpha = 0.25, alpha = 0.1,
           size = 0.01) +
  ylab("") + xlab("") +
  geom_point(aes(x = x, y = y, color = group),
             alpha = 0.5, size = 4) +
  geom_text_repel(aes(x = x, y = y, label = ID),
                  data = proj2.nodes[proj2.nodes$degree>=2,], 
                  size = 2,
                  alpha = 0.4) +
  scale_color_fish_d(option = "Scarus_quoyi") +
  theme_net()
bip_proj_2

