setwd("/Users/mengyang/Documents/github/phono_net/")
library("igraph")
library("qgraph")
library("BSDA")
library('CINNA')
library("poweRlaw")
library("ggplot2")
library("brainGraph")
library("effsize")


nodes <- read.csv("words.csv",header=T,as.is=T)

all_links <- read.csv("phono.csv",header=T,as.is=T)

replace_links <- read.csv("phono_replace.csv",header=T,as.is=T)

replace_l50_links <- read.csv("phono_replace_less50.csv",header=T,as.is=T)

replace_m50_links <- read.csv("phono_replace_more50.csv",header=T,as.is=T)

delins_links <- read.csv("phono_insertdelete.csv",header=T,as.is=T)

all_net <- graph_from_data_frame(d = all_links, vertices = nodes, directed = F)

replace_net <- graph_from_data_frame(d = replace_links, vertices = nodes, directed = F)

l50_net <- graph_from_data_frame(d = replace_l50_links, vertices = nodes, directed = F)

m50_net <- graph_from_data_frame(d = replace_m50_links, vertices = nodes, directed = F)

delins_net <- graph_from_data_frame(d = delins_links, vertices = nodes, directed = F)

all_net <- delete.vertices(all_net, which(degree(all_net) < 1))

replace_net <- delete.vertices(replace_net, which(degree(replace_net) < 1))

l50_net <- delete.vertices(l50_net, which(degree(l50_net) < 1))

m50_net <- delete.vertices(m50_net, which(degree(m50_net) < 1))

delins_net <- delete.vertices(delins_net, which(degree(delins_net) < 1))

all_net_simple <- simplify(all_net)
replace_net_simple <- simplify(replace_net)
l50_net_simple <- simplify(l50_net)
m50_net_simple <- simplify(m50_net)
delins_net_simple <- simplify(delins_net)

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





fr_links_top1000 <- read.csv("top1000/fr_top1000.csv",header=T,as.is=T)

fr_net_top1000 <- graph_from_data_frame(d = fr_links_top1000, vertices = nodes, directed = F)

fr_net_top1000 <- delete.vertices(fr_net_top1000, which(degree(fr_net_top1000) < 1))

fr_net_top1000_simple <- simplify(fr_net_top1000)

fr_net_top1000_gc <- giant_component_extract(fr_net_top1000_simple, directed = FALSE)

fr_net_top1000_gc <- fr_net_top1000_gc[[1]]



fr_net_top1000_gc_cc <- transitivity(fr_net_top1000_gc, type = "global", vids = NULL, weights = NULL)
fr_net_top1000_gc_mean_distance <- mean_distance(fr_net_top1000_gc, unconnected = TRUE)
fr_net_top1000_gc_comm <- cluster_fast_greedy(fr_net_top1000_gc, merges = TRUE, modularity = TRUE, membership = TRUE)
fr_net_top1000_gc_modularity <- modularity(fr_net_top1000_gc_comm)
#fr_net_top1000_gc_smallworld <- smallworldness(fr_net_top1000_gc, B = 100, up = 0.995, lo = 0.005)


fr_net_top1000_gc_random_cc <- numeric(1000)
fr_net_top1000_gc_random_mean_distance <- numeric(1000)
for (i in 1:1000) {
     rn <- erdos.renyi.game(vcount(fr_net_top1000_gc), ecount(fr_net_top1000_gc), type = "gnm")
     fr_net_top1000_gc_random_cc[i] <- transitivity(rn, type = "global", vids = NULL, weights = NULL)
     fr_net_top1000_gc_random_mean_distance[i] <- mean_distance(rn, unconnected = TRUE)
}
fr_net_top1000_gc_cc_test <- z.test(fr_net_top1000_gc_random_cc, sigma.x = sd(fr_net_top1000_gc_random_cc), mu = fr_net_top1000_gc_cc)
fr_net_top1000_gc_mean_distance_test <- z.test(fr_net_top1000_gc_random_mean_distance, sigma.x = sd(fr_net_top1000_gc_random_mean_distance), mu = fr_net_top1000_gc_mean_distance)










en_links_top1000 <- read.csv("top1000/en_top1000.csv",header=T,as.is=T)

en_net_top1000 <- graph_from_data_frame(d = en_links_top1000, vertices = nodes, directed = F)

en_net_top1000 <- delete.vertices(en_net_top1000, which(degree(en_net_top1000) < 1))

en_net_top1000_simple <- simplify(en_net_top1000)

en_net_top1000_gc <- giant_component_extract(en_net_top1000_simple, directed = FALSE)

en_net_top1000_gc <- en_net_top1000_gc[[1]]



en_net_top1000_gc_cc <- transitivity(en_net_top1000_gc, type = "global", vids = NULL, weights = NULL)
en_net_top1000_gc_mean_distance <- mean_distance(en_net_top1000_gc, unconnected = TRUE)
en_net_top1000_gc_comm <- cluster_fast_greedy(en_net_top1000_gc, merges = TRUE, modularity = TRUE, membership = TRUE)
en_net_top1000_gc_modularity <- modularity(en_net_top1000_gc_comm)
#en_net_top1000_gc_smallworld <- smallworldness(en_net_top1000_gc, B = 100, up = 0.995, lo = 0.005)


en_net_top1000_gc_random_cc <- numeric(1000)
en_net_top1000_gc_random_mean_distance <- numeric(1000)
for (i in 1:1000) {
     rn <- erdos.renyi.game(vcount(en_net_top1000_gc), ecount(en_net_top1000_gc), type = "gnm")
     en_net_top1000_gc_random_cc[i] <- transitivity(rn, type = "global", vids = NULL, weights = NULL)
     en_net_top1000_gc_random_mean_distance[i] <- mean_distance(rn, unconnected = TRUE)
}
en_net_top1000_gc_cc_test <- z.test(en_net_top1000_gc_random_cc, sigma.x = sd(en_net_top1000_gc_random_cc), mu = en_net_top1000_gc_cc)
en_net_top1000_gc_mean_distance_test <- z.test(en_net_top1000_gc_random_mean_distance, sigma.x = sd(en_net_top1000_gc_random_mean_distance), mu = en_net_top1000_gc_mean_distance)


ca_net_top1000_partial_cc <- numeric(1000)
ca_net_top1000_partial_mean_distance <- numeric(1000)
ca_net_top1000_partial_modularity <- numeric(1000)
for (i in 1:1000) {
     p_v <- sample(V(ca_net_top1000_gc),40)
     pn <- induced_subgraph(ca_net_top1000_gc, p_v)
     ca_net_top1000_partial_cc[i] <- transitivity(pn, type = "global", vids = NULL, weights = NULL)
     ca_net_top1000_partial_mean_distance[i] <- mean_distance(pn, unconnected = TRUE)
     pn_rw <- cluster_fast_greedy(pn, merges = TRUE, modularity = TRUE, membership = TRUE)
     ca_net_top1000_partial_modularity[i] <- modularity(pn_rw)
}

fr_net_top1000_partial_cc <- numeric(1000)
fr_net_top1000_partial_mean_distance <- numeric(1000)
fr_net_top1000_partial_modularity <- numeric(1000)
for (i in 1:1000) {
     p_v <- sample(V(fr_net_top1000_gc),40)
     pn <- induced_subgraph(fr_net_top1000_gc, p_v)
     fr_net_top1000_partial_cc[i] <- transitivity(pn, type = "global", vids = NULL, weights = NULL)
     fr_net_top1000_partial_mean_distance[i] <- mean_distance(pn, unconnected = TRUE)
     pn_rw <- cluster_fast_greedy(pn, merges = TRUE, modularity = TRUE, membership = TRUE)
     fr_net_top1000_partial_modularity[i] <- modularity(pn_rw)
}


en_net_top1000_partial_cc <- numeric(1000)
en_net_top1000_partial_mean_distance <- numeric(1000)
en_net_top1000_partial_modularity <- numeric(1000)
for (i in 1:1000) {
     p_v <- sample(V(en_net_top1000_gc),40)
     pn <- induced_subgraph(en_net_top1000_gc, p_v)
     en_net_top1000_partial_cc[i] <- transitivity(pn, type = "global", vids = NULL, weights = NULL)
     en_net_top1000_partial_mean_distance[i] <- mean_distance(pn, unconnected = TRUE)
     pn_rw <- cluster_fast_greedy(pn, merges = TRUE, modularity = TRUE, membership = TRUE)
     en_net_top1000_partial_modularity[i] <- modularity(pn_rw)
}

df_top1000_cc <- data.frame(num = seq(1,3000),
                    net = c(rep('ca', times=length(ca_net_top1000_partial_cc)),
                            rep('en', times = length(en_net_top1000_partial_cc)),
                            rep('fr', times = length(fr_net_top1000_partial_cc))),
                    cc = c(ca_net_top1000_partial_cc, en_net_top1000_partial_cc, fr_net_top1000_partial_cc))

t_top1000_cc<-pairwise.t.test(df_top1000_cc$cc,df_top1000_cc$net,paired=FALSE,p.adjust.method="bonferroni")

df_top1000_mean_distance <- data.frame(num = seq(1,3000),
                    net = c(rep('ca', times=length(ca_net_top1000_partial_mean_distance)),
                            rep('en', times = length(en_net_top1000_partial_mean_distance)),
                            rep('fr', times = length(fr_net_top1000_partial_mean_distance))),
                    mean_distance = c(ca_net_top1000_partial_mean_distance, en_net_top1000_partial_mean_distance, fr_net_top1000_partial_mean_distance))

t_top1000_mean_distance<-pairwise.t.test(df_top1000_mean_distance$mean_distance,df_top1000_mean_distance$net,paired=FALSE,p.adjust.method="bonferroni")


df_top1000_modularity <- data.frame(num = seq(1,3000),
                    net = c(rep('ca', times=length(ca_net_top1000_partial_modularity)),
                            rep('en', times = length(en_net_top1000_partial_modularity)),
                            rep('fr', times = length(fr_net_top1000_partial_modularity))),
                    modularity = c(ca_net_top1000_partial_modularity, en_net_top1000_partial_modularity, fr_net_top1000_partial_modularity))

t_top1000_modularity<-pairwise.t.test(df_top1000_modularity$modularity,df_top1000_modularity$net,paired=FALSE,p.adjust.method="bonferroni")


ca_net_top1000_gc_degree <- degree(ca_net_top1000_gc, v=V(ca_net_top1000_gc), normalized = FALSE)
en_net_top1000_gc_degree <- degree(en_net_top1000_gc, v=V(en_net_top1000_gc), normalized = FALSE)
fr_net_top1000_gc_degree <- degree(fr_net_top1000_gc, v=V(fr_net_top1000_gc), normalized = FALSE)

V(ca_net_top1000_gc)$label.color <- V(ca_net_top1000_gc)$category_color 
V(en_net_top1000_gc)$label.color <- V(en_net_top1000_gc)$category_color 
V(fr_net_top1000_gc)$label.color <- V(fr_net_top1000_gc)$category_color 

pdf(file = "all_top1000.pdf", width = 45, height = 15)
par(mfrow = c(1, 3),mai=c(0.22,0.22,0.22,0.22))
plot(ca_net_top1000_gc, vertex.shape = "none", edge.width = .01,  edge.curved = .2, vertex.label = V(ca_net_top1000_gc)$uni_lemma,layout = layout_with_fr(ca_net_top1000_gc))
title("Cantonese",cex.main=2)
plot(en_net_top1000_gc, vertex.shape = "none", edge.width = .01,  edge.curved = .2, vertex.label = V(en_net_top1000_gc)$uni_lemma,layout = layout_with_fr(en_net_top1000_gc))
title("English",cex.main=2)
plot(fr_net_top1000_gc, vertex.shape = "none", edge.width = .01,  edge.curved = .2, vertex.label = V(fr_net_top1000_gc)$uni_lemma, layout = layout_with_fr(fr_net_top1000_gc))
title("French",cex.main=2)
dev.off()

pdf(file = "all_top1000_t.pdf", width = 18, height = 6)
par(mfrow = c(1, 3),mai=c(0.22,0.22,0.22,0.22))
plot(ca_net_top1000_gc, vertex.shape = "none", edge.width = .01,  edge.curved = .2, vertex.label = V(ca_net_top1000_gc)$uni_lemma,layout = layout_in_circle(ca_net_top1000_gc))
title("Cantonese",cex.main=2)
plot(en_net_top1000_gc, vertex.shape = "none", edge.width = .01,  edge.curved = .2, vertex.label = V(en_net_top1000_gc)$uni_lemma,layout = layout_in_circle(en_net_top1000_gc))
title("English",cex.main=2)
plot(fr_net_top1000_gc, vertex.shape = "none", edge.width = .01,  edge.curved = .2, vertex.label = V(fr_net_top1000_gc)$uni_lemma, layout = layout_in_circle(fr_net_top1000_gc))
title("French",cex.main=2)
dev.off()


mean(ca_net_top1000_gc_degree)
mean(en_net_top1000_gc_degree)
mean(fr_net_top1000_gc_degree)

vcount(ca_net_top1000_gc)
vcount(en_net_top1000_gc)
vcount(fr_net_top1000_gc)

ca_net_top1000_gc_cc
en_net_top1000_gc_cc
fr_net_top1000_gc_cc

ca_net_top1000_gc_mean_distance
en_net_top1000_gc_mean_distance
fr_net_top1000_gc_mean_distance

ca_net_top1000_gc_modularity
en_net_top1000_gc_modularity
fr_net_top1000_gc_modularity

mean(ca_net_top1000_gc_random_cc, na.rm=TRUE)
mean(en_net_top1000_gc_random_cc, na.rm=TRUE)
mean(fr_net_top1000_gc_random_cc, na.rm=TRUE)


mean(ca_net_top1000_gc_random_mean_distance, na.rm=TRUE)
mean(en_net_top1000_gc_random_mean_distance, na.rm=TRUE)
mean(fr_net_top1000_gc_random_mean_distance, na.rm=TRUE)

mean(ca_net_top1000_gc_random_modularity, na.rm=TRUE)
mean(en_net_top1000_gc_random_modularity, na.rm=TRUE)
mean(fr_net_top1000_gc_random_modularity, na.rm=TRUE)


t.test(ca_net_top1000_partial_cc,en_net_top1000_partial_cc)
cohen.d(ca_net_top1000_partial_cc,en_net_top1000_partial_cc) 
t.test(ca_net_top1000_partial_cc,fr_net_top1000_partial_cc)
cohen.d(ca_net_top1000_partial_cc,fr_net_top1000_partial_cc) 
t.test(en_net_top1000_partial_cc,fr_net_top1000_partial_cc)
cohen.d(en_net_top1000_partial_cc,fr_net_top1000_partial_cc) 

t.test(ca_net_top1000_partial_mean_distance,en_net_top1000_partial_mean_distance)
cohen.d(ca_net_top1000_partial_mean_distance,en_net_top1000_partial_mean_distance) 
t.test(ca_net_top1000_partial_mean_distance,fr_net_top1000_partial_mean_distance)
cohen.d(ca_net_top1000_partial_mean_distance,fr_net_top1000_partial_mean_distance) 
t.test(en_net_top1000_partial_mean_distance,fr_net_top1000_partial_mean_distance)
cohen.d(en_net_top1000_partial_mean_distance,fr_net_top1000_partial_mean_distance) 


t.test(ca_net_top1000_partial_modularity,en_net_top1000_partial_modularity)
cohen.d(ca_net_top1000_partial_modularity,en_net_top1000_partial_modularity) 
t.test(ca_net_top1000_partial_modularity,fr_net_top1000_partial_modularity)
cohen.d(ca_net_top1000_partial_modularity,fr_net_top1000_partial_modularity) 
t.test(en_net_top1000_partial_modularity,fr_net_top1000_partial_modularity)
cohen.d(en_net_top1000_partial_modularity,fr_net_top1000_partial_modularity) 