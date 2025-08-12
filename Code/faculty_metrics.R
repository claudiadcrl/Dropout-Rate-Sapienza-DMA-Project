library(ggplot2)
library(dplyr)
library(igraph)
library(RColorBrewer)
library(sqldf)

# Preparing the data ------------------------------------------------------

# Start by filtering the faculties in order to keep the actual faculties only.

faculties_list<-c("ARCHITETTURA", "ECONOMIA", "FARMACIA E MEDICINA", "GIURISPRUDENZA","INGEGNERIA CIVILE E INDUSTRIALE", "INGEGNERIA DELL'INFORMAZIONE, INFORMATICA E STATISTICA", "LETTERE E FILOSOFIA", "MEDICINA E ODONTOIATRIA", "MEDICINA E PSICOLOGIA", "SCIENZE MATEMATICHE, FISICHE E NATURALI", "SCIENZE POLITICHE, SOCIOLOGIA, COMUNICAZIONE", "SCUOLA DI INGEGNERIA AEROSPAZIALE")

dropout2 <- dropout_filtered %>%
  filter(facolta %in% faculties_list) %>%
  group_by(facolta) %>%
  summarise(
    total = n(),
    dropout = mean(Dropout == TRUE)
  ) %>%
  arrange(desc(dropout))

dropout2$facolta <- factor(dropout2$facolta, levels = dropout2$facolta)
View(dropout2)

# Then, perform a self-join to find faculties with similar dropout rates.

pairs <- sqldf("
  SELECT 
    A.facolta AS from_f,
    B.facolta AS to_f,
    A.dropout AS from_dropout,
    B.dropout AS to_dropout,
    ABS(A.dropout - B.dropout) AS rate_diff,
    1.0 / (1.0 + ABS(A.dropout - B.dropout)) AS similarity
  FROM dropout2 A
  JOIN dropout2 B
    ON A.facolta < B.facolta
   AND ABS(A.dropout - B.dropout) < 0.1
")
pairs<-rename(pairs, from=from_f)
pairs<-rename(pairs, to=to_f)

min_sim <- min(pairs$similarity)
max_sim <- max(pairs$similarity)

temp<-(pairs$similarity - min_sim)/(max_sim - min_sim)
pairs$similarity_norm <- 1+1*temp
View(pairs)

# Derive the nodes and edges and the corresponding weights and palettes.

nodes <- dropout2
edges <- pairs
fnet <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)
E(fnet)$weight <- edges$similarity_norm
dropout_values <- V(fnet)$dropout
node_colors <- colorRampPalette(c("green", "yellow", "red"))(100)[as.numeric(cut(dropout_values, breaks = 100))]

# Plotting the network ----------------------------------------------------

# Plot the network using the Fruchterman-Reingold (force-directed) layout.

plot(fnet,
     layout=layout_with_fr(fnet)*5,
     vertex.color = node_colors,
     vertex.label = V(fnet)$name,
     vertex.size = 10,
     vertex.frame.color = NA,
     vertex.label.cex = 0.7,
     vertex.label.font = 2,     
     vertex.label.family = "sans",  
     vertex.label.color="black",
     edge.color = "grey",
     edge.width = E(fnet)$weight*2,
     main = "Network of Faculties by Dropout Rates")

legend(
  "topright",
  legend = c("Low", "Medium", "High"),
  fill = colorRampPalette(c("green", "yellow", "red"))(3),
  border = NA,
  title = "Dropout Rate",
  bty = "n")

# Network metrics ---------------------------------------------------------

# Start by looking at the number of nodes and edges of the network.

vcount(fnet) # Number of nodes
ecount(fnet) # Number of edges

# Then, study the interconnectivity of the network by looking at the distribution
# of the local clustering coefficient of each node and use it to find the global
# clustering coefficient of the entire network.

transitivity(fnet, type = "local") # Local clustering coefficient of each node
transitivity(fnet, type = "global") # Global clustering coefficient

# Overall, the network seems to be very interconnected, with outliers being
# Medicine and Space Engineering departments due to the higher dropout rates.
# In particular, the local clustering coefficient for Space Engineering is NaN
# because its only connection is with the Medicine Department (ki = 1 -> Ci = 0).

# At this point, it is also possible to perform centrality evaluations through the
# following metrics:
# 1) Closeness centrality: How quickly a node can reach the other nodes.
# 2) Betweenness centrality: How much control a node has on the information flow.
# 3) Eigenvector centrality: How influential a node is according to the number
#                            (quantity) and weight (quality) of connections.

closeness(fnet, normalized = TRUE) # Closeness centrality
betweenness(fnet, normalized = TRUE) # Betweenness centrality
eigen_centrality(fnet)$vector # Eigenvector centrality

# Overall, the following trends can be observed:
# 1) Closeness: Most nodes have a closeness centrality between 0.4 and 0.5 (apart
#               from Space Engineering), suggesting no relevant observations.
# 2) Betweenness: Most nodes have a betweenness centrality close to or equal to 0
#                 (except for Medicine and Law), meaning that information flow is
#                 not overly relevant (idk how to word this).
# 3) Eigenvector: Apart from Medicine, Space Engineering and Engineering, most
#                 nodes have high eigenvector centrality scores, suggesting strong
#                 relevance (likely due to their strong interconnectivity).