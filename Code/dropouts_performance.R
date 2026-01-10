library(ggplot2)
library(dplyr)

# This file checks for associations between dropout rates and student performance,
# checking for course year, faculty, obtained credits, last activities and grades.

# Course Year -------------------------------------------------------------

# Start by plotting the course year distribution by dropout status.

ggplot(dropout_filtered[!is.na(dropout_filtered$annoCorso), ], aes(x = Dropout, y = annoCorso, fill = Dropout)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Boxplot of Course Year by Dropout Status",
    x = "Dropout Status",
    y = "Course Year",
    fill = "Dropout Status") +
  theme_minimal()

# In order to check for an association between course year and dropout status, it
# is possible to use the two-sided t test, which checks whether two groups of a
# variable have the same mean or not.
# H0: mX = mY.
# H1: mX and mY are different.

# Start by checking the assumptions of normality and homogeneity of variance.

qqnorm(na.omit(dropout_filtered$annoCorso), main = "Q-Q Plot of Course Year")
qqline(na.omit(dropout_filtered$annoCorso), col = "red")

var.test(annoCorso ~ Dropout, data = dropout_filtered)

t.test(annoCorso ~ Dropout, data = dropout_filtered, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# However, the distribution seems highly skewed, so I would avoid studying the t
# test and try the Chi-Squared test.

# Faculty -----------------------------------------------------------------

# Start by filtering the data in order to keep the actual faculties only.

faculties_list <- c("ARCHITETTURA", "ECONOMIA", "FARMACIA E MEDICINA", "GIURISPRUDENZA","INGEGNERIA CIVILE E INDUSTRIALE", "INGEGNERIA DELL'INFORMAZIONE, INFORMATICA E STATISTICA", "LETTERE E FILOSOFIA", "MEDICINA E ODONTOIATRIA", "MEDICINA E PSICOLOGIA", "SCIENZE MATEMATICHE, FISICHE E NATURALI", "SCIENZE POLITICHE, SOCIOLOGIA, COMUNICAZIONE", "SCUOLA DI INGEGNERIA AEROSPAZIALE")

dropout_filtered_2 <- dropout_filtered %>%
  filter(facolta %in% faculties_list) %>%
  group_by(facolta) %>%
  summarise(
    total = n(),
    dropout = mean(Dropout == TRUE)
  ) %>%
  arrange(desc(dropout))

dropout_filtered_2$facolta <- factor(dropout2$facolta, levels = dropout2$facolta)
View(dropout_filtered_2)

# Then, plot the dropout rates for each faculty.

ggplot(dropout_filtered_2, aes(x = facolta, y = dropout)) +
  geom_col(fill = "gray", width = 0.3) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.1)) +
  labs(
    title = "Dropout Rate by Faculty",
    x = "Dropout Rate",
    y = "Faculty") +
  theme_minimal(base_size = 10) +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    axis.text.x = element_text(angle = 45, hjust = 1))

# In order to check for an association between faculty and dropout status, it is
# possible to use the Chi-Squared test, which assesses the statistical
# significance of an association.
# H0: The association is not statistically significant.
# H1: The association is statistically significant.

# Start by taking the relevant faculties that satisfy the assumptions of the test.

faculty_counts <- dropout_filtered %>%
  count(facolta) %>%
  filter(facolta %in% faculties_list) %>%
  filter(n > 5)

df <- dropout_filtered

df_filtered <- df %>%
  semi_join(faculty_counts, by = "facolta")
View(df_filtered)

# Run the test by organizing the data into a contingency table.

chisq_faculty <- table(df_filtered$facolta, df_filtered$Dropout)
faculty_result <- chisq.test(chisq_faculty)
faculty_result$expected

# Having found p < 0.05, the null hypothesis is rejected, meaning that the
# association between dropout status and faculty is statistically significant.

# Obtained Credits --------------------------------------------------------

# Start by creating a new attribute denoting the proportion of CFU acquired so far.

df_cfu<-dropout_filtered
df_cfu$ratio<-df_cfu$cfuTake/df_cfu$creditiTotali

df_clean <- df_cfu[
  is.finite(df_cfu$ratio) & 
    !is.na(df_cfu$Dropout), ]

# Then, plot the distribution of obtained credits by dropout status.

ggplot(df_clean, aes(x = Dropout, y = ratio, fill = Dropout)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Boxplot of CFU Ratio by Dropout Status",
    x = "Dropout Status",
    y = "CFU Ratio",
    fill = "Dropout Status") +
  theme_minimal()

# In order to check for an association between CFU ratio and dropout status, it
# is possible to use the two-sided t test, which checks whether two groups of a
# variable have the same mean or not.
# H0: mX = mY.
# H1: mX and mY are different.

# Start by checking the assumptions of normality and homogeneity of variance.

qqnorm(na.omit(df_clean$ratio), main = "Q-Q Plot of CFU Ratio")
qqline(na.omit(df_clean$ratio), col = "red")

var.test(ratio ~ Dropout, data = df_clean, na.action = na.omit)

t.test(ratio ~ Dropout, data = df_clean, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.

# However, since the FALSE group contains all the outliers with ratio > 1, it can
# come in handy to rerun the test by ignoring the outliers to see what happens.

df_clean_2 <- df_cfu[
  is.finite(df_cfu$ratio) & 
    !is.na(df_cfu$Dropout) & 
    df_cfu$ratio <= 1, ]

# Start by checking the assumptions of normality and homogeneity of variance.

qqnorm(na.omit(df_clean_2$ratio), main = "Q-Q Plot of CFU Ratio")
qqline(na.omit(df_clean_2$ratio), col = "red")

var.test(ratio ~ Dropout, data = df_clean_2, na.action = na.omit)

t.test(ratio ~ Dropout, data = df_clean_2, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.

# Last Enrollment ---------------------------------------------------------

# Start by plotting the distribution by dropout status.

ggplot(dropout_filtered, aes(x = ultIscr, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Histogram of Last Enrollment by Dropout Status",
    x = "Last Enrollment (Years)",
    y = "Count",
    fill = "Dropout Status") +
  theme_minimal()

# In order to check for an association between enrollment time and dropout status,
# it is possible to use the two-sided t test, which checks whether two groups of
# a variable have the same mean or not.
# H0: mX = mY.
# H1: mX and mY are different.

# Start by checking the assumptions of normality and homogeneity of variance.

qqnorm(na.omit(dropout_filtered$ultIscr), main = "Q-Q Plot of Last Enrollment")
qqline(na.omit(dropout_filtered$ultIscr), col = "red")

var.test(ultIscr ~ Dropout, data = dropout_filtered, na.action = na.omit)

t.test(ultIscr ~ Dropout, data = dropout_filtered, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# N.B.: Due to a poor approximation, these results could be less reliable.

# Last Booked Exam --------------------------------------------------------

# Start by plotting the distribution by dropout status.

ggplot(dropout_filtered, aes(x = ultPren, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity") +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Histogram of Last Booked Exam by Dropout Status",
    x = "Last Booked Exam (Months)",
    y = "Count",
    fill = "Dropout Status") +
  theme_minimal()

# In order to check for an association between last booking and dropout status,
# it is possible to use the two-sided t test, which checks whether two groups of
# a variable have the same mean or not.
# H0: mX = mY.
# H1: mX and mY are different.

# Start by checking the assumptions of normality and homogeneity of variance.

qqnorm(na.omit(dropout_filtered$ultPren), main = "Q-Q Plot of Last Booking")
qqline(na.omit(dropout_filtered$ultPren), col = "red")

var.test(ultPren ~ Dropout, data = dropout_filtered, na.action = na.omit)

t.test(ultPren ~ Dropout, data = dropout_filtered, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# N.B.: Due to a poor approximation, these results could be less reliable.

# Last Passed Exam --------------------------------------------------------

# Start by plotting the distribution by dropout status.
# N.B.: The outlier 2238 is ignored in the visualization.

p <- ggplot(dropout_filtered[dropout_filtered$ultSup <= 1000, ], aes(x = ultSup, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Histogram of Last Passed Exam by Dropout Status",
    x = "Last Passed Exam (Months)",
    y = "Count",
    fill = "Dropout Status") +
  theme_minimal()

# In order to check for an association between last passed exam and dropout status,
# it is possible to use the two-sided t test, which checks whether two groups of
# a variable have the same mean or not.
# H0: mX = mY.
# H1: mX and mY are different.

# Start by checking the assumptions of normality and homogeneity of variance.

qqnorm(na.omit(dropout_filtered$ultSup), main = "Q-Q Plot of Last Passed Exam")
qqline(na.omit(dropout_filtered$ultSup), col = "red")

var.test(ultSup ~ Dropout, data = dropout_filtered, na.action = na.omit)

t.test(ultSup ~ Dropout, data = dropout_filtered, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# N.B.: Apart from the outlier, the normal approximation seems to work fairly well.

# Grade Average -----------------------------------------------------------

# Start by filtering the invalid values, such as 0 or NA.

avg_dropout <- dropout_filtered %>% 
  filter(!is.na(mediaVoto), mediaVoto != 0)

# Then, plot the grade average distribution by dropout status.

ggplot(avg_dropout, aes(x = Dropout, y = mediaVoto, fill = Dropout)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Boxplot of Grade Average by Dropout Status",
    x = "Dropout Status",
    y = "Grade Average",
    fill = "Dropout Status") +
  theme_minimal()

# In order to check for an association between grade average and dropout status,
# it is possible to use the two-sided t test, which checks whether two groups of
# a variable have the same mean or not.
# H0: mX = mY.
# H1: mX and mY are different.

# Start by checking the assumptions of normality and homogeneity of variance.

qqnorm(na.omit(avg_dropout$mediaVoto), main = "Q-Q Plot of Grade Average")
qqline(na.omit(avg_dropout$mediaVoto), col = "red")

var.test(mediaVoto ~ Dropout, data = avg_dropout)

t.test(mediaVoto ~ Dropout, data = avg_dropout, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# However, the distribution does not seem approximately normal, so the results
# could be unreliable.

# High School Diploma Score -----------------------------------------------

# Start by filtering the invalid values, such as 0 or NA.

unique(dropout_filtered$votoDiploma) # Notice that there is a 2 as well

dip_dropout <- dropout_filtered %>% 
  filter(!is.na(votoDiploma), votoDiploma != 0)

# Then, plot the diploma score distribution by dropout status.

ggplot(dip_dropout, aes(x = Dropout, y = votoDiploma, fill = Dropout)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Boxplot of Diploma Score by Dropout Status",
    x = "Dropout Status",
    y = "High School Diploma Score",
    fill = "Dropout Status") +
  theme_minimal()

# In order to check for an association between grade average and dropout status,
# it is possible to use the two-sided t test, which checks whether two groups of
# a variable have the same mean or not.
# H0: mX = mY.
# H1: mX and mY are different.

# Start by checking the assumptions of normality and homogeneity of variance.

qqnorm(na.omit(dip_dropout$votoDiploma), main = "Q-Q Plot of Diploma Score")
qqline(na.omit(dip_dropout$votoDiploma), col = "red")

var.test(votoDiploma ~ Dropout, data = dip_dropout)

t.test(votoDiploma ~ Dropout, data = dip_dropout, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# While there is a skew towards the -3 zone, I would consider this to be fairly
# approximately normal, especially compared to the other two distributions.