colnames(dropout_filtered)

library(ggplot2)
library(dplyr)
library(maps)
library(countrycode)

# This file checks for associations between dropout rates and student profile,
# checking for age, provenience and gender.

# Age ---------------------------------------------------------------------

# Start by plotting the age distribution by dropout status.

ggplot(dropout_filtered[!is.na(dropout_filtered$eta), ], aes(x = eta, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 100) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Histogram of Age by Dropout Status",
    x = "Age",
    y = "Count",
    fill = "Dropout Status") +
  theme_minimal()

# In order to check for an association between age and dropout status, it is possible
# to use the two-sided t test, which checks whether two groups of a variable have
# the same mean or not.
# H0: mX = mY.
# H1: mX and mY are different.

# Start by checking the assumptions of normality and homogeneity of variance.

qqnorm(na.omit(dropout_filtered$eta), main = "Q-Q Plot of Age")
qqline(na.omit(dropout_filtered$eta), col = "red")

values_true <- na.omit(dropout_filtered$eta[dropout_filtered$Dropout == TRUE])

qqnorm(values_true, main = "Q-Q Plot of Age for Dropout = TRUE")
qqline(values_true, col = "blue")

values_false <- na.omit(dropout_filtered$eta[dropout_filtered$Dropout == FALSE])

qqnorm(values_false, main = "Q-Q Plot of Age for Dropout = FALSE")
qqline(values_false, col = "darkgreen")

var.test(eta ~ Dropout, data = dropout_filtered)

# The age distribution is highly skewed and not normal, although the sample size
# is large enough to apply the central limit theorem and tolerate the skewness.
# In particular, since the groups have different variances, it is better to try
# to run Welch's t test to check whether the two groups have the same mean.

t.test(eta ~ Dropout, data = dropout_filtered, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# N.B.: The means differ by 0.1, although this result is considered to be 
#       statistically significant.

# Age Groups --------------------------------------------------------------

# Due to the issues with the assumptions of the t test, it can come in handy to
# bin age into groups and run the Chi-Squared test for double checking the results.

# Start by binning age into groups and plot the group distribution.

breaksAge <- c(18, 22, 26, 30, Inf) # breaks
labelsAge <- c("18-21", "22-25", "26-29", "30+") # labels

dropout_filtered$ageRange <- cut(dropout_filtered$eta, breaks = breaksAge, labels = labelsAge, right = TRUE, include.lowest = TRUE)
dropout_filtered <- dropout_filtered %>% 
  relocate(ageRange, .after = eta)

ggplot(dropout_filtered, aes(x = ageRange, fill = Dropout)) +
  geom_bar(position = "fill") +  # Proportion per group
  scale_fill_manual(
    values = c("TRUE" = "#FF6961",
               "FALSE" = "#77DD77")) +
  labs(
    title = "Dropout Status Proportion by Age Group",
    x = "Age Group",
    y = "Proportion",
    fill = "Dropout Status") +
  theme_minimal()

# At this point, it is possible to study the association between dropout status
# and age groups by running the Chi-Squared test, which assesses the statistical
# significance of an association.
# H0: The association is not statistically significant.
# H1: The association is statistically significant.

# Run the test by organizing the data into a contingency table.

chisq_age_groups <- table(dropout_filtered$ageRange, dropout_filtered$Dropout)
age_groups_result <- chisq.test(chisq_age_groups)
age_groups_result$expected # The assumptions hold true.

# Having found p < 0.05, the null hypothesis is rejected, suggesting that the
# association between dropout status and age/age groups is indeed relevant.

# Provenience -------------------------------------------------------------

# Start by plotting the provenience distribution by dropout status.

ggplot(dropout_filtered, aes(x = cittadinanza, fill = Dropout)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("TRUE" = "#FF6961",
               "FALSE" = "#77DD77")) +
  labs(
    title = "Dropout Status by Citizenship (ITA/EU/Extra-EU)",
    x = "Citizenship",
    y = "Count",
    fill = "Dropout Status") +
  theme_minimal()

# In order to check for an association between provenience and dropout status,
# it is possible to use the Chi-Squared test, which assesses the statistical
# significance of an association.
# H0: The association is not statistically significant.
# H1: The association is statistically significant.

# Run the test by organizing the data into a contingency table.

chisq_citizenship <- table(dropout_filtered$cittadinanza, dropout_filtered$Dropout)
citizenship_result <- chisq.test(chisq_citizenship)
citizenship_result$expected # The assumptions hold true.

# Having found p > 0.05, the null hypothesis is not rejected, suggesting that there
# is no association between dropout status and citizenship.

# Alternatively, try to see what happens by organizing provenience into two groups:
# Italian and non-Italian students.

# Start by adding a new attribute to determine whether a student is Italian or not.

dropout_filtered$CitizenshipGroup <- ifelse(dropout_filtered$cittadinanza == "ITA", "IT", "NON-IT")
dropout_filtered <- dropout_filtered %>% 
  relocate(CitizenshipGroup, .after = cittadinanza)

# Then, plot the citizenship group distribution by dropout status

ggplot(dropout_filtered, aes(x = CitizenshipGroup, fill = Dropout)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("TRUE" = "#FF6961",
               "FALSE" = "#77DD77")) +
  labs(
    title = "Dropout Status by Citizenship (IT/NON-IT)",
    x = "Citizenship",
    y = "Count",
    fill = "Dropout Status") +
  theme_minimal()

# Run the test by organizing the data into a contingency table.

chisq_ita <- table(dropout_filtered$CitizenshipGroup, dropout_filtered$Dropout)
ita_result <- chisq.test(chisq_ita)
ita_result$expected # The assumptions hold true.

# Having found p > 0.05, the null hypothesis is not rejected, confirming that there
# is no relevant association between dropout status and citizenship.

# Birth Country -----------------------------------------------------------

# Alternative to provenience that uses the birth countries rather than the
# Italy/EU/Extra-EU division.

# Start by finding the dropout rates for each country.
# For simplicity, only nations with > 5 registered students will be considered.

dropout_rate <- dropout_filtered %>%
  group_by(nazioneNascita) %>%
  summarise(
    total = n(),
    dropout_rate = mean(Dropout == "TRUE")
  ) %>%
  filter(total > 10) %>%
  arrange(desc(dropout_rate))

# Then, set factor order by dropout rate for plotting.

dropout_rate$nazioneNascita <- factor(dropout_rate$nazioneNascita, levels = dropout_rate$nazioneNascita)

# At this point, it is possible to plot a flipped bar plot for dropout rates by country.

ggplot(dropout_rate, aes(x = nazioneNascita, y = dropout_rate)) +
  geom_col(fill = "gray", width = 0.4) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), # Shortens axis
    breaks = seq(0, 1, by = 0.1)) +
  labs(
    title = "Dropout Rate by Nation of Birth (N > 5)",
    x = "Nation of Birth",
    y = "Dropout Rate") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(hjust = 1, size = 8, margin = margin(t = 2)),
    axis.title.y = element_text(margin = margin(t = 15)),
    plot.margin = margin(10, 20, 10, 10),
    panel.grid = element_blank())

# Alternatively, it is possible to plot a world map of dropout rates by country.

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

# Notice that only the existing countries acknowledged by maps should be considered.

world_countries <- dropout_rate %>%
  filter(!is.na(birthCountry)) %>%
  filter(birthCountry %in% unique_countries) %>%
  mutate(birthCountry = as.character(birthCountry)) # USSR will be ignored

# Now, create an instance of the world map and join it with world_countries to
# have important information together.

world_map <- map_data("world")

map_df <- left_join(world_map, world_countries, by = c("region" = "birthCountry"))

# At this point, it is possible to plot the world map of dropout rates by country.

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

# In order to check for an association between nationality and dropout status,
# it is possible to use the Chi-Squared test, which assesses the statistical
# significance of an association.
# H0: The association is not statistically significant.
# H1: The association is statistically significant.

# Run the test by organizing the relevant data into a contingency table.

nation_counts <- dropout_filtered %>% 
  count(nazioneNascita) %>% 
  filter(n > 15) # To statisfy the assumptions

df <- dropout_filtered
filtered_df <- df %>%
  semi_join(nation_counts, by = "nazioneNascita")

chisq_nationality <- table(filtered_df$nazioneNascita, filtered_df$Dropout)
nationality_result <- chisq.test(chisq_nationality)
nationality_result$expected

# Having found p < 0.05, the null hypothesis is rejected, meaning that the
# association between dropout status and nationality is statistically significant.

# Gender ------------------------------------------------------------------

# Start by plotting the gender distribution by dropout status.

ggplot(dropout_filtered, aes(x = sesso, fill = Dropout)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("TRUE" = "#FF6961",
               "FALSE" = "#77DD77")) +
  labs(
    title = "Dropout Status by Gender",
    x = "Gender",
    y = "Count",
    fill = "Dropout Status") +
  theme_minimal()

# In order to check for an association between gender and dropout status, it is
# possible to use the Chi-Squared test, which assesses the statistical
# significance of an association.
# H0: The association is not statistically significant.
# H1: The association is statistically significant.

# Run the test by organizing the data into a contingency table.

chisq_gender <- table(dropout_filtered$sesso, dropout_filtered$Dropout)
gender_result <- chisq.test(chisq_gender)
gender_result$expected # The assumptions hold true.

# Having found p < 0.05, the null hypothesis is rejected, meaning that the
# association between dropout status and gender is statistically significant.