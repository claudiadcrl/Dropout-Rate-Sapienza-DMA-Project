library(ggplot2)
library(dplyr)

# Course Year -------------------------------------------------------------

# Start by plotting the course year distribution by dropout status.

ggplot(dropout_filtered[!is.na(dropout_filtered$annoCorso), ], aes(x = annoCorso, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 10) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Histogram of Course Year by Dropout Status",
    x = "Course Year",
    y = "Count",
    fill = "Dropout Status") +
  theme_minimal()

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

# Course Year (Binned) ----------------------------------------------------

# Start by plotting the bar plot of binned course year.

ggplot(dropout_filtered[!is.na(dropout_filtered$annoCorso), ], aes(x = factor(annoCorso), fill = Dropout)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("TRUE" = "#FF6961",
               "FALSE" = "#77DD77")) +
  labs(
    title = "Dropouts by Course Year",
    x = "Course Year",
    y = "Count",
    fill = "Dropout Status") +
  theme_minimal()

# At this point, it is possible to study the association between dropout status
# and course year by running the Chi-Squared test, which assesses the statistical
# significance of an association.
# H0: The association is not statistically significant.
# H1: The association is statistically significant.

# Run the test by organizing the data into a contingency table.

chisq_course_year <- table(factor(dropout_filtered$annoCorso), dropout_filtered$Dropout)
course_year_result <- chisq.test(chisq_course_year)
course_year_result$expected # The assumptions do not hold true.

# Having found p < 0.05, the null hypothesis is rejected, meaning that the
# association between dropout status and gender is statistically significant,
# although it should be noticed that the assumptions on the expected frequencies
# fail as Eij < 5 for some combinations.

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