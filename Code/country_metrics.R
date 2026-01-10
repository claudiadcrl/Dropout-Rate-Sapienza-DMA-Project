library(ggplot2)
library(dplyr)
library(igraph)
library(RColorBrewer)
library(sqldf)
library(countrycode)

# Preparing the data ------------------------------------------------------

# Start by deriving the translated birth countries and the corresponding ISO2 codes.

dropout_rate <- dropout_filtered %>%
  group_by(nazioneNascita) %>%
  summarise(
    total = n(),
    dropout_rate = mean(Dropout == TRUE)
  ) %>%
  filter(total > 10) %>%
  arrange(desc(dropout_rate))

dropout_rate$nazioneNascita <- factor(dropout_rate$nazioneNascita, levels = dropout_rate$nazioneNascita)

dropout_rate$birthCountry <- countrycode(dropout_rate$nazioneNascita,
                                         origin = "country.name.it",
                                         destination = "country.name")
dropout_rate <- dropout_rate %>% 
  relocate(birthCountry, .after = nazioneNascita)
missing <- dropout_rate[is.na(dropout_rate$birthCountry), "nazioneNascita"]
unique(missing)

dropout_rate[34, "birthCountry"] <- "Ghana"
dropout_rate[39, "birthCountry"] <- "USSR" # Placeholder value for simplicity
dropout_rate[43, "birthCountry"] <- "Russia"

dropout_rate$country_code <- countrycode(dropout_rate$birthCountry,
                                         origin = "country.name",
                                         destination = "iso2c")

View(dropout_rate)

# Then, perform a self-join to find countries with similar dropout rates.

dropout_pairs <- sqldf("
  SELECT 
    A.nazioneNascita AS from_country,
    B.nazioneNascita AS to_country,
    A.dropout_rate AS from_dropout,
    B.dropout_rate AS to_dropout,
    ABS(A.dropout_rate - B.dropout_rate) AS rate_diff,
    1.0 / (1.0 + ABS(A.dropout_rate - B.dropout_rate)) AS similarity
  FROM dropout_rate A
  JOIN dropout_rate B
    ON A.nazioneNascita < B.nazioneNascita
   AND ABS(A.dropout_rate - B.dropout_rate) < 0.1
")
min_sim <- min(dropout_pairs$similarity)
max_sim <- max(dropout_pairs$similarity)

temp <- (dropout_pairs$similarity - min_sim)/(max_sim - min_sim)
dropout_pairs$similarity_norm <- 1+temp
View(dropout_pairs)

nodes <- dropout_rate
dropout_pairs <- rename(dropout_pairs, from=from_country)
dropout_pairs <- rename(dropout_pairs, to=to_country)
edges <- dropout_pairs

net <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

# Derive the nodes and edges and the corresponding weights and palettes.

V(net)$country_code <- nodes$country_code
E(net)$weight <- edges$similarity_norm
dropout_values <- V(net)$dropout_rate
node_colors <- colorRampPalette(c("green", "yellow", "red"))(100)[as.numeric(cut(dropout_values, breaks = 100))]

# Plotting the network ----------------------------------------------------

# Plot the network using the Fruchterman-Reingold (force-directed) layout.

plot(net,
     layout=layout_with_fr(net)*5,
     vertex.color = node_colors,
     vertex.label = V(net)$country_code,
     vertex.size = 10,
     vertex.frame.color = NA,
     vertex.label.cex = 0.7,
     vertex.label.font = 2,     
     vertex.label.family = "sans",  
     vertex.label.color="black",
     edge.color = "grey",
     edge.width = E(net)$weight*2,
     main = "Network of Countries by Dropout Rates")

legend(
  "topright",
  legend = c("Low", "Medium", "High"),
  fill = colorRampPalette(c("green", "yellow", "red"))(3),
  border = NA,
  title = "Dropout Rate",
  bty = "n")

# Network metrics ---------------------------------------------------------

# Start by looking at the number of nodes and edges of the network.

vcount(net) # Number of nodes
ecount(net) # Number of edges

# Then, study the interconnectivity of the network by looking at the distribution
# of the local clustering coefficient of each node and use it to find the global
# clustering coefficient of the entire network.

transitivity(net, type = "local") # Local clustering coefficient of each node
transitivity(net, type = "global") # Global clustering coefficient

# Overall, the network appears to be quite interconnected, especially for countries
# with a higher/lower dropout rate, thus resulting in a high global clustering
# coefficient.

# At this point, it is also possible to perform centrality evaluations through the
# following metrics:
# 1) Closeness centrality: How quickly a node can reach the other nodes.
# 2) Betweenness centrality: How much control a node has on the information flow.
# 3) Eigenvector centrality: How influential a node is according to the number
#                            (quantity) and weight (quality) of connections.

closeness(net, normalized = TRUE) # Closeness centrality
betweenness(net, normalized = TRUE) # Betweenness centrality
eigen_centrality(net)$vector # Eigenvector centrality

# Overall, the following trends can be observed:
# 1) Closeness: Apart with countries at the "ends" of the spectrum (such as Nepal
#               or Afghanistan), which have lower scores, most countries appear to
#               have a closeness centrality score between 0.4 and 0.5, suggesting
#               no relevant observations.
# 2) Betweenness: All countries have a betweenness centrality score close to or
#                 equal to zero, suggesting no importance over information flows.
# 3) Eigenvector: Similar results to closeness centrality, but countries from
#                 Nigeria to Iran (in the order) appear to have very high scores,
#                 suggesting that they are more influential within the network.