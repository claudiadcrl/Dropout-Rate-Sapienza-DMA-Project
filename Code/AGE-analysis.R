library(ggplot2)
library(dplyr)

dropout_filtered<-read.csv("C:\\Users\\compu\\Downloads\\dropout_filtered.csv")


# First experiments (IGNORE) -------------------------------------------------------
#First visualization (by age)
ggplot(dropout_filtered[!is.na(dropout_filtered$eta), ], aes(x = eta, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 100) +
  scale_fill_manual(
    values = c("True" = "orange",
               "False" = "#2A52BE")) +
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
        col = c("#2A52BE", "orange"))

#We can see that the distribution of people who dropped out is right skewed, while the other is symmetric
#Median of students who dropped out is slightly lower
#Spread is basically the same, there are outliers

#There doesn't seem to be strong association

#To confirm perform one way ANOVA to see if the mean differs

ggplot(dropout_filtered, aes(x = eta, fill = Dropout)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Age Distribution by Dropout Status",
    x = "Age",
    y = "Density",
    fill = "Dropout"
  ) +
  theme_minimal()

#First check normality
ggplot(dropout_filtered, aes(sample = eta)) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~ Dropout)

install.packages("car")
library(car)
leveneTest(eta ~ Dropout, data = dropout_filtered)
mean(dropout_filtered$eta[dropout_filtered$Dropout == "True"])
mean(dropout_filtered$eta[dropout_filtered$Dropout == "False"])

anova_age <- aov(eta ~ Dropout, data = dropout_filtered, na.action = na.omit)
summary(anova_age)
# The null hypothesis is rejected.
#p=0.00117<0.05
#Mean varies --> see how

#Run post-hoc test to see how they differ
TukeyHSD(anova_age)

#The mean age value is 0.107 lower for dropout students compared to non-dropout.
#The adjusted p-value is 0.00117, which is well below 0.05, meaning this difference is statistically significant

#There doesn't seem to be association between age and dropout


# What i can actually do --------------------------------------------------

#First visualization (by age)
ggplot(dropout_filtered[!is.na(dropout_filtered$eta), ], aes(x = eta, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 100) +
  scale_fill_manual(
    values = c("True" = "orange",
               "False" = "#2A52BE")) +
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
        col = c("#2A52BE", "orange"))

#Since i can't do ANOVA or t-test because of different variance
#do BINNING and do Chi-Squared
# + contingency table and mosaic plot/barplot

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
    values = c("True" = "orange",
               "False" = "#2A52BE")) +
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

