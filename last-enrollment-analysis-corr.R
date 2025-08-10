last_e <- read.csv("C:\\Users\\zosia\\OneDrive\\Pulpit\\dma2 project\\dropout_filtered.csv")
View(last_e)
library(dplyr)
library(ggplot2)


#Boxplot for last booked and passed exams
#i had to add restrictions for it to work , outliers where messing them up
df %>%
  select(ultSup, ultPren) %>%
  filter(ultSup < 75, ultPren < 75) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "ExamType", values_to = "Value") %>%
  ggplot(aes(x = ExamType, y = Value, fill = ExamType)) +
  geom_boxplot() +
  labs(title = "Cleaned Boxplots of Last Passed and Booked Exams",
       y = "Exam Number", x = "Exam Type") +
  theme_minimal()

#Histpgram of the enrollment year
df %>%
  ggplot(aes(x = annoAccaCors)) +
  geom_histogram(binwidth = 1, fill = "#2C3E50", color = "white") +
  labs(
    title = "Histogram of Last Enrollment Year (annoAccaCors)",
    x = "Enrollment Year",
    y = "Count"
  ) +
  theme_minimal()


#ANOVA on last passed exam
# Run ANOVA for last passed exam (ultSup)
anova_sup <- aov(ultSup ~ Dropout, data = df)
summary(anova_sup)
#There is a statistically significant difference in the ultSup values
#(last passed exam) between dropout groups.

