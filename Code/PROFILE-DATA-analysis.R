library(ggplot2)
library(dplyr)

dropout_filtered<-read.csv("C:\\Users\\compu\\Downloads\\dropout_filtered.csv")

# Age ---------------------------------------------------------------------

#First visualization (by age)
ggplot(dropout_filtered[!is.na(dropout_filtered$eta), ], aes(x = eta, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 100) +
  scale_fill_manual(
    values = c("True" = "#FF6961",
               "False" = "#77DD77")) +
  labs(
    title = "Histogram of Age by Dropout Status",
    x = "Age",
    y = "Count",
    fill = "Dropout"
  ) +
  theme_minimal()


#Second visualization (by dropout status)
boxplot(eta ~ Dropout, data = dropout_filtered,
        main = "Age by Dropout Status",
        xlab = "Dropout",
        ylab = "Age",
        col = c("#77DD77", "#FF6961"))

#T test, assume normality by CLT and different variances
result<-t.test(eta ~ Dropout, data = dropout_filtered)
print(result)
#statistically significant difference in mean age between the two dropout groups 

#To confirm association, do BINNING + CHI-SQUARED
breaksAge <- c(18, 22, 26, 30, Inf) # breaks
labelsAge <- c("18-21", "22-25", "26-29", "30+") # labels

dropout_filtered$ageRange <- cut(dropout_filtered$eta, breaks = breaksAge, labels = labelsAge, right = TRUE, include.lowest = TRUE)
dropout_filtered <- dropout_filtered %>% 
  relocate(ageRange, .after = eta)
View(dropout_filtered)

#Proportional Bar Plot
ggplot(dropout_filtered, aes(x = ageRange, fill = Dropout)) +
  geom_bar(position = "fill") +  # Proportion per group
  scale_fill_manual(
    values = c("True" = "#FF6961",
               "False" = "#77DD77")) +
  labs(
    title = "Dropout Proportion by Age Group",
    x = "Age Group",
    y = "Proportion",
    fill = "Dropout Status"
  ) +
  theme_minimal()

#Contingency table
table_data <- table(dropout_filtered$ageRange, dropout_filtered$Dropout)

#Chi-Square Test
chi_result <- chisq.test(table_data)
print(chi_result)
#p value (2.2e-16) << 0.05 --> significant association between age group and dropout



# Gender ------------------------------------------------------------------

# Gender is a binary variable that tells whether a student is a male or a female.
# For this reason, it can be studied through the Chi-Squared test.
# H0: The association is not statistically significant.
# H1: The association is statistically significant.

# Start by plotting the sideways bar plot of Dropout by Gender.

ggplot(dropout_filtered, aes(x = sesso, fill = Dropout)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("True" = "#FF6961",
               "False" = "#77DD77")) +
  labs(
    title = "Dropouts by Gender",
    x = "Gender",
    y = "Count",
    fill = "Dropout Status"
  ) +
  theme_minimal()

# Then, carry out the Chi-Squared test.

chisq_gender <- table(dropout_filtered$sesso,
                      dropout_filtered$Dropout)
gender_result <- chisq.test(chisq_gender)
gender_result$expected # check that the assumptions hold true
print(gender_result)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the test
# suggests an association between Gender and Dropouts.
# Possible association: males are proportionally less likely to drop out? YES

#Proportional Bar Plot
ggplot(dropout_filtered, aes(x = sesso, fill = Dropout)) +
  geom_bar(position = "fill") +  # Proportion per group
  scale_fill_manual(
    values = c("True" = "#FF6961",
               "False" = "#77DD77")) +
  labs(
    title = "Dropout Proportion by Gender",
    x = "Gender",
    y = "Proportion",
    fill = "Dropout Status"
  ) +
  theme_minimal()


# Citizenship ---------------------------------------------
#I gave more options compared to pie chart
#Barplot
ggplot(dropout_filtered, aes(x = cittadinanza, fill = Dropout)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("True" = "#FF6961",
               "False" = "#77DD77")) +
  labs(title = "Dropout status by Citizenship Group", y = "Count", x = "Citizenship", fill = "Dropout") +
  theme_minimal()

#Proportional barplot
ggplot(dropout_filtered, aes(x = cittadinanza, fill = Dropout)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("True" = "#FF6961",
               "False" = "#77DD77")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Dropout Proportion by Citizenship", y = "Proportion", x = "Citizenship", fill = "Dropout Status") +
  theme_minimal()

#CHI-SQUARED for ITA/Extra-EU/EU
table_region_dropout <- table(dropout_filtered$cittadinanza, dropout_filtered$Dropout)
chisq.test(table_region_dropout)
#p-value = 0.4133 > 0.05
# failing to reject the null hypothesis
#-->no association between citizenship and dropout



# ITA vs non-ITA ----------------------------------------------------------
#idk how informative it can be, might cut if presentation is too long


#grouping ITA/ NON-ITA
df_birth_grouped <- dropout_filtered %>%
  mutate(birth_group = ifelse(nazioneNascita == "ITALIA", "ITA", "NON-ITA"))

#Plot
#Proportional barplot
ggplot(df_birth_grouped, aes(x = birth_group, fill = Dropout)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c("True" = "#FF6961",
               "False" = "#77DD77")) +
  labs(title = "Dropout Proportion by Citizenship", y = "Proportion", x = "Citizenship", fill = "Dropout Status") +
  theme_minimal()

#CHI-SQUARED TEST
table_birth_dropout <- table(df_birth_grouped$birth_group, df_birth_grouped$Dropout)
chisq.test(table_birth_dropout)
#p-value = 0.1978 > 0.05
#no association


# Nationality -------------------------------------------------------------

# Step 1: Calculate dropout rate per nation (only nations with > 5 students)
dropout_rate <- dropout_filtered %>%
  group_by(nazioneNascita) %>%
  summarise(
    total = n(),
    dropout_rate = mean(Dropout == "True")
  ) %>%
  filter(total > 10) %>%
  arrange(desc(dropout_rate))

# Step 2: Set factor order by dropout rate for plotting
dropout_rate$nazioneNascita <- factor(dropout_rate$nazioneNascita, levels = dropout_rate$nazioneNascita)

# Step 3: Plot flipped dropout rate barplot
ggplot(dropout_rate, aes(x = nazioneNascita, y = dropout_rate)) +
  geom_col(fill = "gray", width = 0.4) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), #shortens axis
    breaks = seq(0, 1, by = 0.1)  # Add a tick every 10%
  ) +
  labs(
    title = "Dropout Rate by Nation of Birth (N > 5)",
    x = "Nation of Birth",
    y = "Dropout Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(hjust = 1, size = 8, margin = margin(t = 2)),
    axis.title.y = element_text(margin = margin(t = 15)),
    plot.margin = margin(10, 20, 10, 10),
    panel.grid = element_blank() 
  )

#Countries plot (i'll do it later with igraph if i manage)

#CHI-SQUARED TEST
#Need to filter out those countries for which we have less than 5 students per dropout status by requirements of test
nation_counts <- dropout_filtered %>%
  count(nazioneNascita) %>%
  filter(n > 15)#less didn't achieve requirements
df<-dropout_filtered
df_filtered <- df %>%
  semi_join(nation_counts, by = "nazioneNascita")

table_nation <- table(df_filtered$nazioneNascita, df_filtered$Dropout)
chisq.test(table_nation)
#p value (1.706e-05) << 0.05
#THERE IS ASSOCIATION


#Network and map are kept separate