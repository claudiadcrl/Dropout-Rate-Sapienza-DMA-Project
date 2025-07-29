baza <- read.csv("C:\\Users\\zosia\\OneDrive\\Pulpit\\dma2 project\\dropout_filtered.csv")
View(baza)
library(dplyr)
library(ggplot2)

#grouping 3 ITA, EU, Extra-EU
df_grouped <- df %>%
  mutate(region_group = case_when(
    cittadinanza == "ITA" ~ "ITA",
    cittadinanza == "Extra-UE" ~ "Extra-EU",
    TRUE ~ "EU"  
  ))

#Counting dropouts numbers and proportions for the plot
df_summary <- df_grouped %>%
  mutate(drop_status = ifelse(Dropout, "Yes", "No")) %>%
  group_by(region_group, drop_status) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  group_by(region_group) %>%
  mutate(percent = count / sum(count))

#plots
ggplot(df_summary, aes(x = "", y = percent, fill = drop_status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~region_group) +
  labs(
    title = "Dropout Proportions by Region",
    fill = "Dropout"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2, size = 16, face = "bold")
  )

#CHI-SQUARED for ITA/Extra-EU/EU
table_region_dropout <- table(df_grouped$region_group, df_grouped$Dropout)
chisq.test(table_region_dropout)
#p-value = 0.4133 > 0.05
# failing to reject the null hypothesis




#dropouts but ITA VS NON-ITA

#grouping ITA/ NON-ITA
df_birth_grouped <- df %>%
  mutate(birth_group = ifelse(nazioneNascita == "ITALIA", "ITA", "NON-ITA"))


df_summary_nascita <- df_birth_grouped %>%
  mutate(drop_status = ifelse(Dropout, "Yes", "No")) %>%
  group_by(birth_group, drop_status) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  group_by(birth_group) %>%
  mutate(percent = count / sum(count))

#plots
ggplot(df_summary_nascita, aes(x = "", y = percent, fill = drop_status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~birth_group) +
  labs(
    title = "Dropout Proportions by Birthplace (ITA vs NON-ITA)",
    fill = "Dropout"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2, size = 16, face = "bold")
  )

#CHI-SQUARED TEST
table_birth_dropout <- table(df_birth_grouped$birth_group, df_birth_grouped$Dropout)
chisq.test(table_birth_dropout)
#p-value = 0.1978 > 0.05

#GRaph for nationalitoes amnd dropout count (doesnt quite work)
# 1. Filter for dropouts and count by nazioneNascita
df_counts <- df %>%
  filter(Dropout == TRUE) %>%
  count(nazioneNascita, sort = TRUE)

# 2. Plot the barplot
ggplot(df_counts, aes(x = reorder(nazioneNascita, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Number of Dropouts by Country of Birth",
    x = "Country of Birth",
    y = "Number of Dropouts"
  ) +
  theme_minimal()