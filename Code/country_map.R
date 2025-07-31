install.packages("countrycode")
library(ggplot2)
library(dplyr)
library(maps)
library(countrycode)

# Create and fix the dataframe --------------------------------------------

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
missing <- dropout_rate[is.na(dropout_rate$birthCountry), "nazioneNascita"]
unique(missing)

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

# Creating the World Map --------------------------------------------------

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
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90") +
  labs(title = "Dropout Rates by Country", fill = "Dropout Rate") +
  theme_minimal() +
  coord_fixed(1.3) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())