colnames(dropout_filtered)

library(ggplot2)
library(dplyr)

# This file checks for association between dropout rates and student wealth,
# checking for tax brackets and ISEE.

isee_dropout <- dropout_filtered %>% 
  filter(!is.na(valoreIntero))
View(isee_dropout)

# ISEE --------------------------------------------------------------------

# Plot the ISEE distribution by dropout status.

ggplot(isee_dropout, aes(x = valoreIntero, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 100) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Histogram of ISEE by Dropout Status",
    x = "ISEE",
    y = "Count",
    fill = "Dropout Status"
  ) +
  theme_minimal()

ggplot(isee_dropout, aes(x = Dropout, y = valoreIntero, fill = Dropout)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Boxplot of ISEE by Dropout Status",
    x = "Dropout Status",
    y = "ISEE",
    fill = "Dropout Status") +
  theme_minimal()

# In order to check for an association between ISEE and dropout status, it is
# possible to use the two-sided t test, which checks whether two groups of a
# variable have the same mean or not.
# H0: mX = mY.
# H1: mX and mY are different.

# Start by checking the assumptions of normality and homogeneity of variance.

qqnorm(na.omit(isee_dropout$valoreIntero), main = "Q-Q Plot of ISEE")
qqline(na.omit(isee_dropout$valoreIntero), col = "red")

var.test(valoreIntero ~ Dropout, data = isee_dropout)

t.test(valoreIntero ~ Dropout, data = isee_dropout, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# While the distribution can be approximated to a normal distribution, the results
# could be less accurate due to an outlier, so consider applying a log-transformation
# using the log1p(x) = log(1 + x) function.

# Plot the log-transformed ISEE distribution by dropout status.

ggplot(isee_dropout, aes(x = log1p(valoreIntero), fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 100) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Histogram of log-transformed ISEE by Dropout Status",
    x = "log1p(ISEE)",
    y = "Count",
    fill = "Dropout Status"
  ) +
  theme_minimal()

ggplot(isee_dropout, aes(x = Dropout, y = log1p(valoreIntero), fill = Dropout)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Boxplot of log-transformed ISEE by Dropout Status",
    x = "Dropout Status",
    y = "log1p(ISEE)",
    fill = "Dropout Status") +
  theme_minimal()

# Check again the assumptions of normality and homogeneity of variance.

qqnorm(na.omit(log1p(isee_dropout$valoreIntero)), main = "Q-Q Plot of log-transformed ISEE")
qqline(na.omit(log1p(isee_dropout$valoreIntero)), col = "red")

var.test(log1p(valoreIntero) ~ Dropout, data = isee_dropout)

t.test(log1p(valoreIntero) ~ Dropout, data = isee_dropout, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, enforcing the aforementioned
# results.
# However, the Q-Q plot of log-transformed ISEE shows a stronger skew, meaning
# that the results could be less reliable.

# Tax Bracket -------------------------------------------------------------

# Since the ISEE analysis could be less accurate due to outliers/skewness, it can
# come in handy to bin ISEE into a categorical variable by exploiting Sapienza's
# tax brackets and then run the Chi-Squared test.

breaks <- c(0, 15000, 30000, 60000, Inf)
labels <-c("1", "2", "3", "4")

isee_dropout$bracket <- cut(isee_dropout$valoreIntero, breaks = breaks, labels = labels, right = TRUE, include.lowest = TRUE)

# Start by plotting the tax bracket distribution by dropout status.

ggplot(isee_dropout, aes(x = bracket, fill = Dropout)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c(
      "TRUE" = "#FF6961",
      "FALSE" = "#77DD77")) +
  labs(
    title = "Dropout Status Proportion by Tax Bracket",
    x = "Tax Bracket",
    y = "Count",
    fill = "Dropout Status"
  ) +
  theme_minimal()

# At this point, it is possible to study the association between dropout status
# and tax brackets by running the Chi-Squared test, which assesses the statistical
# significance of an association.
# H0: The association is not statistically significant.
# H1: The association is statistically significant.

# Run the test by organizing the data into a contingency table.

chisq_bracket <- table(isee_dropout$bracket, isee_dropout$Dropout)
bracket_result <- chisq.test(chisq_bracket)
bracket_result$expected

# Having found p < 0.05, the null hypothesis is rejected, suggesting that the
# association between dropout status and tax brackets is statistically relevant.