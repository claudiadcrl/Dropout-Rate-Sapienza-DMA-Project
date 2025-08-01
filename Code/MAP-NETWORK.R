install.packages(c("igraph", "RColorBrewer"))
library(igraph)
library(RColorBrewer)
library(sqldf)

dropout_rate <- dropout_filtered %>%
  group_by(nazioneNascita) %>%
  summarise(
    total = n(),
    dropout_rate = mean(Dropout == "True")
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

View(dropout_rate)


# MAP ---------------------------------------------------------------------

world_map <- map_data("world")

# To color the map according to dropout rates, it is necessary to first understand
# which countries are present in the World Map.
world_countries <- dropout_rate %>%
  filter(!is.na(birthCountry)) %>%
  filter(birthCountry %in% unique_countries) %>%
  mutate(birthCountry = as.character(birthCountry))
View(world_countries)

setdiff(dropout_rate$birthCountry, world_countries$birthCountry) # Check differences

# At this point, it is necessary to join these data with the world data to have
# the dropout rates in handy for the plot.
map_df <- left_join(world_map, world_countries, by = c("region" = "birthCountry"))
View(map_df)

# Lastly, create the plot of the world map by dropout rates.
ggplot(map_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray90", color = "white") +
  geom_polygon(data = subset(map_df, !is.na(dropout_rate)),
               aes(fill = dropout_rate), color = "white") +
  scale_fill_gradient(low = "green", high = "red", na.value = "gray90") +
  labs(title = "Dropout Rates by Country", fill = "Dropout Rate") +
  theme_minimal() +
  coord_fixed(1.3) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


# NETWORK -----------------------------------------------------------------

#Make dataset with connections
#Take dropout_rate, which has the countries, the total of students and the dropout rates
#Join it with itself on conditions:
#                                   - Countries must be different
#                                   - difference in dropout rate <0.1
#                                   - to avoid duplicates: first country < second country (alphabetical order)

# Add ISO2 country codes
dropout_rate$country_code <- countrycode(dropout_rate$birthCountry,
                                         origin = "country.name",
                                         destination = "iso2c")

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

temp<-(dropout_pairs$similarity - min_sim)/(max_sim - min_sim)
dropout_pairs$similarity_norm <- 1+0*temp
View(dropout_pairs)

nodes<-dropout_rate
dropout_pairs<-rename(dropout_pairs, from=from_country)
dropout_pairs<-rename(dropout_pairs, to=to_country)
edges<-dropout_pairs

net <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

V(net)$country_code<-nodes$country_code
E(net)$weight <- edges$similarity_norm
dropout_values <- V(net)$dropout_rate
node_colors <- colorRampPalette(c("green", "yellow", "red"))(100)[as.numeric(cut(dropout_values, breaks = 100))]

View(dropout_pairs)
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
     main = "Network of Countries by Dropout Rates"
)
legend(
  "topright",
  legend = c("Low", "Medium", "High"),
  fill = colorRampPalette(c("green", "yellow", "red"))(3),
  border = NA,
  title = "Dropout Rate",
  bty = "n"
)
