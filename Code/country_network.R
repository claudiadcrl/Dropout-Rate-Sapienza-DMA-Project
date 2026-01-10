library(ggplot2)
library(dplyr)
library(countrycode)
library(igraph)
library(sqldf)

# Create the dataframe ----------------------------------------------------

# Start by taking birth countries and the corresponding dropout rates.

dropout_rate <- dropout_filtered %>%
  group_by(nazioneNascita) %>%
  summarise(
    total = n(),
    dropout_rate = mean(Dropout == "TRUE")
  ) %>%
  filter(total > 10) %>%
  arrange(desc(dropout_rate))

dropout_rate$nazioneNascita <- factor(dropout_rate$nazioneNascita, levels = dropout_rate$nazioneNascita)

# In order to translate the various countries, take a look at the countries in the
# maps library.

all_regions <- map("world", plot = FALSE)$names
country_names <- sapply(strsplit(all_regions, ":"), `[`, 1) # Get rid of regions
unique_countries <- unique(country_names) # Take each country at most once
unique_countries

# Use the countrycode library for Italian-English translations.
# N.B.: Unmatched countries will be translated to NA.

dropout_rate$birthCountry <- countrycode(dropout_rate$nazioneNascita,
                                         origin = "country.name.it",
                                         destination = "country.name")
dropout_rate <- dropout_rate %>% 
  relocate(birthCountry, .after = nazioneNascita)

# There are some NA or mismatched translations, which, however, can be easily
# fixed manually with the help of the unique_countries list.

dropout_rate[13, "birthCountry"] <- "Democratic Republic of the Congo"
dropout_rate[14, "birthCountry"] <- "USA"
dropout_rate[21, "birthCountry"] <- "Republic of Congo"
dropout_rate[31, "birthCountry"] <- "UK"
dropout_rate[34, "birthCountry"] <- "Ghana"
dropout_rate[39, "birthCountry"] <- "USSR" # Placeholder value for simplicity
dropout_rate[43, "birthCountry"] <- "Russia"
dropout_rate[65, "birthCountry"] <- "Macedonia"

# Self-join the dataset ---------------------------------------------------

# For simplicity, the network will connect all countries with similar dropout
# rates, so consider a self-join on the dataset.

dropout_rate_comparisons <- sqldf("
                                  SELECT 
                                  a.nazioneNascita as nazione_from,
                                  b.nazioneNascita as nazione_to,
                                  a.birthCountry as country_from,
                                  b.birthCountry as country_to,
                                  a.dropout_rate as dropout_rate_from,
                                  b.dropout_rate as dropout_rate_to,
                                  ABS(a.dropout_rate - b.dropout_rate) AS rate_difference
                                  FROM dropout_rate as a
                                  JOIN dropout_rate as b
                                  ON a.birthCountry < b.birthCountry
                                  AND ABS(a.dropout_rate - b.dropout_rate) < 0.1")
View(dropout_rate_comparisons)

# Create the network ------------------------------------------------------

library(igraph)

# Assume `result` is the filtered self-joined dataframe with edges
# Assume `df` is the original dataframe with dropout rates

# Start by creating the edge list by considering all possible combinations.

edges <- dropout_rate_comparisons[, c("country_from", "country_to")]

# Then, create (and adjust) nodes with country name and dropout rate (for plotting).

nodes <- dropout_rate[!duplicated(dropout_rate$birthCountry), c("birthCountry", "dropout_rate")] # Remember to feature *all* countries
colnames(nodes) <- c("name", "dropout_rate") # Standard igraph requirement

# At this point, create the network.

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

# Furthermore, create the color palette for the nodes.

dropout_values <- V(g)$dropout_rate
node_colors <- colorRampPalette(c("green", "yellow", "red"))(100)[as.numeric(cut(dropout_values, breaks = 100))]

# Lastly, plot the graph, adding a legend for understandability.

plot(g,
     layout = layout_in_circle(g),
     vertex.color = node_colors,
     vertex.label = V(g)$name,
     vertex.size = 15,
     vertex.label.cex = 0.5,
     edge.color = "grey",
     main = "Network of Countries by Dropout Rates"
)

legend("topright", legend = c("Low", "Medium", "High"),
       fill = colorRampPalette(c("green", "yellow", "red"))(3),
       title = "Dropout Rate")

# Network Metrics ---------------------------------------------------------

# The basics: getting the number of nodes and edges of the network.

vcount(g) # Number of nodes
ecount(g) # Number of edges

# Now, focus on the clustering coefficient, which denotes how interconnected the
# nodes are, both locally and globally.

transitivity(g, type = "local") # Local clustering coefficient of each node
transitivity(g, type = "global") # Global clustering coefficient

# The results suggest strong connections within the network as, indeed, countries
# with similar dropout rates will be tightly connected.

# Lastly, focus on centrality, which denotes how "influent" the nodes are.

closeness_vector <- closeness(g, normalized = TRUE) # Closeness: how quickly a node can reach other nodes
betweenness_vector <- betweenness(g, normalized = TRUE) # Betweenness: how much a node controls information flows
eigen_vector <- eigen_centrality(g)$vector # Eigenvector: how influent a node is in associations

# The results suggest the following:
# Closeness: The nodes that are quicker to reach other nodes are then ones with
#            "medium" dropout rates.
# Betweenness: No node seems able to control the entire information flow as most
#              nodes have a betweenness centrality < 0.1.
# Eigenvector: Again, the nodes with "medium" dropout rates seem to be the most
#              influential.

# Extra: Centrality histograms.

hist(closeness_vector,
     main =  "Closeness Centrality Distribution",
     xlab = "Closeness",
     col = "#B0BF1A",
     breaks = 100)

hist(betweenness_vector,
     main =  "Betweenness Centrality Distribution",
     xlab = "Betweenness",
     col = "#9DC183",
     breaks = 100)

hist(eigen_vector,
     main =  "Eigenvector Centrality Distribution",
     xlab = "Eigenvector Centrality",
     col = "#006400",
     breaks = 100)