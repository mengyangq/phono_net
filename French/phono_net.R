setwd("/Users/mengyang/Documents/github/phono_net/French/")
library("igraph")
library("qgraph")
library("BSDA")
library('CINNA')
library("poweRlaw")
library("ggplot2")
library("brainGraph")
library("effsize")


nodes <- read.csv("french_phono_nodes.csv",header=T,as.is=T)

all_links <- read.csv("french_phono_links.csv",header=T,as.is=T)

replace_links <- read.csv("french_phono_replace_links.csv",header=T,as.is=T)

delins_links <- read.csv("french_phono_delins_links.csv",header=T,as.is=T)


all_net <- graph_from_data_frame(d = all_links, vertices = nodes, directed = F)
all_net <- delete.vertices(all_net, which(degree(all_net) < 1))
all_net_simple <- simplify(all_net)
all_gc <- giant_component_extract(all_net_simple, directed = FALSE)
all_gc <- all_gc[[1]]
write.csv(degree(all_gc),file = "all_gc_degree.csv") 
write.csv(as_data_frame(all_gc,what='vertices'),file = "all_gc_node.csv")

write.csv(degree(all_net_simple),file = "all_degree.csv") 
write.csv(as_data_frame(all_net_simple,what='vertices'),file = "all_node.csv")

sub_nodes <- read.csv("all_gc_node.csv",header=T,as.is=T)










replace_net <- graph_from_data_frame(d = replace_links, vertices = nodes, directed = F)
delins_net <- graph_from_data_frame(d = delins_links, vertices = nodes, directed = F)


replace_net_simple <- simplify(replace_net)
delins_net_simple <- simplify(delins_net)



replace_sub <- induced_subgraph(replace_net_simple,sub_nodes$name)
delins_sub <- induced_subgraph(delins_net_simple,sub_nodes$name)


write.csv(degree(replace_sub),file = "replace_sub-all-gc_degree.csv") 
write.csv(degree(delins_sub),file = "delins_sub-all-gc_degree.csv")



transitivity(replace_sub, type = "global", vids = NULL, weights = NULL)
transitivity(l50_sub, type = "global", vids = NULL, weights = NULL)
transitivity(m50_sub, type = "global", vids = NULL, weights = NULL)
transitivity(delins_sub, type = "global", vids = NULL, weights = NULL)

mean_distance(replace_sub, unconnected = TRUE)
mean_distance(l50_sub, unconnected = TRUE)
mean_distance(m50_sub, unconnected = TRUE)
mean_distance(delins_sub, unconnected = TRUE)


replace_sub2 <- induced_subgraph(replace_net_simple,sub_nodes2$node)
l50_sub2 <- induced_subgraph(l50_net_simple,sub_nodes2$node)
m50_sub2 <- induced_subgraph(m50_net_simple,sub_nodes2$node)
delins_sub2 <- induced_subgraph(delins_net_simple,sub_nodes2$node)

write.csv(degree(replace_sub2),file = "replace_sub2.csv") 
write.csv(degree(l50_sub2),file = "l50_sub2.csv")
write.csv(degree(m50_sub2),file = "m50_sub2.csv")
write.csv(degree(delins_sub2),file = "delins_sub2.csv")

all_cc<-transitivity(all_net_simple, type = "local", vids = NULL, weights = NULL)
replace_cc<-transitivity(replace_sub2, type = "local", vids = NULL, weights = NULL)
l50_cc<-transitivity(l50_sub2, type = "local", vids = NULL, weights = NULL)
m50_cc<-transitivity(m50_sub2, type = "local", vids = NULL, weights = NULL)


all_cc <- replace(all_cc, is.na(all_cc), -1)
replace_cc <- replace(replace_cc, is.na(replace_cc), -1)
l50_cc <- replace(l50_cc, is.na(l50_cc), -1)
m50_cc <- replace(m50_cc, is.na(m50_cc), -1)

write.csv(all_cc,file="all_cc.csv")
write.csv(replace_cc,file="replace_cc.csv")
write.csv(l50_cc,file="l50_cc.csv")
write.csv(m50_cc,file="m50_cc.csv")

write.csv(degree(all_net_simple),file = "all_degree.csv") 
write.csv(as_data_frame(all_net_simple,what='vertices'),file = "all_node.csv")
write.csv(as_data_frame(all_net_simple,what='edges'),file = "all_edge.csv")

write.csv(degree(replace_net_simple),file = "replace_degree.csv") 
write.csv(as_data_frame(replace_net_simple,what='vertices'),file = "replace_node.csv")

write.csv(degree(delins_net_simple),file = "delins_degree.csv") 
write.csv(as_data_frame(delins_net_simple,what='vertices'),file = "delins_node.csv")

write.csv(degree(l50_net_simple),file = "l50_degree.csv") 
write.csv(as_data_frame(l50_net_simple,what='vertices'),file = "l50_node.csv")

write.csv(degree(m50_net_simple),file = "m50_degree.csv") 
write.csv(as_data_frame(m50_net_simple,what='vertices'),file = "m50_node.csv")


all_cc <- transitivity(all_net_simple, type = "global", vids = NULL, weights = NULL)
all_mean_distance <- mean_distance(all_net_simple, unconnected = TRUE)
all_cc
all_mean_distance

replace_cc <- transitivity(replace_net_simple, type = "global", vids = NULL, weights = NULL)
replace_mean_distance <- mean_distance(replace_net_simple, unconnected = TRUE)
replace_cc
replace_mean_distance

delins_cc <- transitivity(delins_net_simple, type = "global", vids = NULL, weights = NULL)
delins_mean_distance <- mean_distance(delins_net_simple, unconnected = TRUE)
delins_cc
delins_mean_distance

l50_cc <- transitivity(l50_net_simple, type = "global", vids = NULL, weights = NULL)
l50_mean_distance <- mean_distance(l50_net_simple, unconnected = TRUE)
l50_cc
l50_mean_distance

m50_cc <- transitivity(m50_net_simple, type = "global", vids = NULL, weights = NULL)
m50_mean_distance <- mean_distance(m50_net_simple, unconnected = TRUE)
m50_cc
m50_mean_distance

vcount(all_net_simple)
vcount(replace_net_simple)
vcount(delins_net_simple)
vcount(l50_net_simple)
vcount(m50_net_simple)

all_gc <- giant_component_extract(all_net_simple, directed = FALSE)
all_gc <- all_gc[[1]]

replace_gc <- giant_component_extract(replace_net_simple, directed = FALSE)
replace_gc <- replace_gc[[1]]

l50_gc <- giant_component_extract(l50_net_simple, directed = FALSE)
l50_gc <- l50_gc[[1]]

m50_gc <- giant_component_extract(m50_net_simple, directed = FALSE)
m50_gc <- m50_gc[[1]]

delins_gc <- giant_component_extract(delins_net_simple, directed = FALSE)
delins_gc <- delins_gc[[1]]

write.csv(degree(all_gc),file = "all_gc_degree.csv") 
write.csv(as_data_frame(all_gc,what='vertices'),file = "all_gc_node.csv")

write.csv(degree(replace_gc),file = "replace_gc_degree.csv") 
write.csv(as_data_frame(replace_gc,what='vertices'),file = "replace_gc_node.csv")    

write.csv(degree(l50_gc),file = "l50_gc_degree.csv") 
write.csv(as_data_frame(l50_gc,what='vertices'),file = "l50_gc_node.csv") 

write.csv(degree(m50_gc),file = "m50_gc_degree.csv") 
write.csv(as_data_frame(m50_gc,what='vertices'),file = "m50_gc_node.csv")

write.csv(degree(delins_gc),file = "delins_gc_degree.csv") 
write.csv(as_data_frame(delins_gc,what='vertices'),file = "delins_gc_node.csv")

all_gc_cc <- transitivity(all_gc, type = "global", vids = NULL, weights = NULL)
replace_gc_cc <- transitivity(replace_gc, type = "global", vids = NULL, weights = NULL)
l50_gc_cc <- transitivity(l50_gc, type = "global", vids = NULL, weights = NULL)
m50_gc_cc <- transitivity(m50_gc, type = "global", vids = NULL, weights = NULL)
delins_gc_cc <- transitivity(delins_gc, type = "global", vids = NULL, weights = NULL)

all_gc_mean_distance <- mean_distance(all_gc, unconnected = TRUE)
replace_gc_mean_distance <- mean_distance(replace_gc, unconnected = TRUE)
l50_gc_mean_distance <- mean_distance(l50_gc, unconnected = TRUE)
m50_gc_mean_distance <- mean_distance(m50_gc, unconnected = TRUE)
delins_gc_mean_distance <- mean_distance(delins_gc, unconnected = TRUE)

all_gc_smallworld <- smallworldness(all_gc, B = 1000, up = 0.995, lo = 0.005)
replace_gc_smallworld <- smallworldness(replace_gc, B = 1000, up = 0.995, lo = 0.005)
l50_gc_smallworld <- smallworldness(l50_gc, B = 1000, up = 0.995, lo = 0.005)
m50_gc_smallworld <- smallworldness(m50_gc, B = 1000, up = 0.995, lo = 0.005)
delins_gc_smallworld <- smallworldness(delins_gc, B = 1000, up = 0.995, lo = 0.005)


all_gc_smallworld <- smallworldness(all_gc, B = 1000, up = 0.995, lo = 0.005)
all_gc_random_cc <- numeric(1000)
all_gc_random_mean_distance <- numeric(1000)
for (i in 1:1000) {
     rn <- erdos.renyi.game(vcount(all_gc), ecount(all_gc), type = "gnm")
     all_gc_random_cc[i] <- transitivity(rn, type = "global", vids = NULL, weights = NULL)
     all_gc_random_mean_distance[i] <- mean_distance(rn, unconnected = TRUE)
}
all_gc_cc_test <- z.test(all_gc_random_cc, sigma.x = sd(all_gc_random_cc), mu = all_gc_cc)
all_gc_mean_distance_test <- z.test(all_gc_random_mean_distance, sigma.x = sd(all_gc_random_mean_distance), mu = all_gc_mean_distance)


replace_gc_random_cc <- numeric(1000)
replace_gc_random_mean_distance <- numeric(1000)
for (i in 1:1000) {
     rn <- erdos.renyi.game(vcount(replace_gc), ecount(replace_gc), type = "gnm")
     replace_gc_random_cc[i] <- transitivity(rn, type = "global", vids = NULL, weights = NULL)
     replace_gc_random_mean_distance[i] <- mean_distance(rn, unconnected = TRUE)
}
replace_gc_cc_test <- z.test(replace_gc_random_cc, sigma.x = sd(replace_gc_random_cc), mu = replace_gc_cc)
replace_gc_mean_distance_test <- z.test(replace_gc_random_mean_distance, sigma.x = sd(replace_gc_random_mean_distance), mu = replace_gc_mean_distance)



l50_gc_random_cc <- numeric(1000)
l50_gc_random_mean_distance <- numeric(1000)
for (i in 1:1000) {
     rn <- erdos.renyi.game(vcount(l50_gc), ecount(l50_gc), type = "gnm")
     l50_gc_random_cc[i] <- transitivity(rn, type = "global", vids = NULL, weights = NULL)
     l50_gc_random_mean_distance[i] <- mean_distance(rn, unconnected = TRUE)
}
l50_gc_cc_test <- z.test(l50_gc_random_cc, sigma.x = sd(l50_gc_random_cc), mu = l50_gc_cc)
l50_gc_mean_distance_test <- z.test(l50_gc_random_mean_distance, sigma.x = sd(l50_gc_random_mean_distance), mu = l50_gc_mean_distance)




m50_gc_random_cc <- numeric(1000)
m50_gc_random_mean_distance <- numeric(1000)
for (i in 1:1000) {
     rn <- erdos.renyi.game(vcount(m50_gc), ecount(m50_gc), type = "gnm")
     m50_gc_random_cc[i] <- transitivity(rn, type = "global", vids = NULL, weights = NULL)
     m50_gc_random_mean_distance[i] <- mean_distance(rn, unconnected = TRUE)
}
m50_gc_cc_test <- z.test(m50_gc_random_cc, sigma.x = sd(m50_gc_random_cc), mu = m50_gc_cc)
m50_gc_mean_distance_test <- z.test(m50_gc_random_mean_distance, sigma.x = sd(m50_gc_random_mean_distance), mu = m50_gc_mean_distance)



delins_gc_random_cc <- numeric(1000)
delins_gc_random_mean_distance <- numeric(1000)
for (i in 1:1000) {
     rn <- erdos.renyi.game(vcount(delins_gc), ecount(delins_gc), type = "gnm")
     delins_gc_random_cc[i] <- transitivity(rn, type = "global", vids = NULL, weights = NULL)
     delins_gc_random_mean_distance[i] <- mean_distance(rn, unconnected = TRUE)
}
delins_gc_cc_test <- z.test(delins_gc_random_cc, sigma.x = sd(delins_gc_random_cc), mu = delins_gc_cc)
delins_gc_mean_distance_test <- z.test(delins_gc_random_mean_distance, sigma.x = sd(delins_gc_random_mean_distance), mu = delins_gc_mean_distance)




pdf(file = "all.pdf", width = 200, height = 200)
plot(all_gc, vertex.shape = "none", edge.width = .001,  edge.curved = .2, vertex.label = V(all_gc)$ortho)
title("All",cex.main=2)
dev.off()

pdf(file = "replace.pdf", width = 100, height = 100)
plot(replace_gc, vertex.shape = "none", edge.width = .001,  edge.curved = .2, vertex.label = V(replace_gc)$ortho)
title("Replace",cex.main=2)
dev.off()

pdf(file = "delins.pdf", width = 100, height = 100)
plot(delins_gc, vertex.shape = "none", edge.width = .001,  edge.curved = .2, vertex.label = V(delins_gc)$ortho)
title("Delete/Insert",cex.main=2)
dev.off()



all_node <- read.csv("all_node.csv",header=T,as.is=T)
all_link <- read.csv("all_edge.csv",header=T,as.is=T)

all_net_new <- graph_from_data_frame(d = all_link, vertices = all_node, directed = F)


all_cc<-transitivity(all_net_new, type = "local", vids = NULL, weights = NULL)

write.csv(all_cc,file="all_cc.csv")
