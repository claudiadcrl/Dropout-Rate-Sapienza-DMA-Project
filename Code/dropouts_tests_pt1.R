colnames(dropout_filtered)

library(ggplot2)
library(dplyr)

# Profile: Gender ---------------------------------------------------------

# Gender is a binary variable that tells whether a student is a male or a female.
# For this reason, it can be studied through the Chi-Squared test.
# H0: The association is not statistically significant.
# H1: The association is statistically significant.

# Start by plotting the sideways bar plot of Dropout by Gender.

ggplot(dropout_filtered, aes(x = sesso, fill = Dropout)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("TRUE" = "#FF6961",
               "FALSE" = "#77DD77")) +
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

# Having found p < 0.05, the null hypothesis is rejected, meaning that the test
# suggests an association between Gender and Dropouts.
# Possible association: males are proportionally less likely to drop out?

# Wealth: Tax Bracket -----------------------------------------------------

# Tax Bracket is a quantitative variable representing the tax bracket a student
# belongs to based on their ISEE, although it can easily be factored into a
# categorical variable.
# For this reason, it can be studied through the Chi-Squared test.
# H0: The association is not statistically significant.
# H1: The association is statistically significant.

# Start by plotting the sideways bar plot of Dropout by Tax Bracket.

ggplot(dropout_filtered[!is.na(dropout_filtered$bracket), ], aes(x = factor(bracket), fill = Dropout)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("TRUE" = "#FF6961",
               "FALSE" = "#77DD77")) +
  labs(
    title = "Dropouts by Tax Bracket",
    x = "Tax Bracket",
    y = "Count",
    fill = "Dropout Status"
  ) +
  theme_minimal()

# Then, carry out the Chi-Squared test.

bracket_df <- data.frame(bracket = as.factor(dropout_filtered$bracket),
                         Dropout = dropout_filtered$Dropout)
clean_bracket_df <- bracket_df[!is.na(bracket_df$bracket), ]

chisq_bracket <- table(clean_bracket_df$bracket,
                       clean_bracket_df$Dropout)
bracket_result <- chisq.test(chisq_bracket)
bracket_result$expected # check that the assumptions hold true

# Having found p < 0.05, the null hypothesis is rejected, meaning that the test
# suggests an association between Tax Bracket and Dropouts.
# Possible association: Bracket 3 seems to have a higher dropout proportion.

# N.B.: For the test, NA values have been ignored.

# Wealth: ISEE ------------------------------------------------------------

# ISEE is a quantitative variable denoting a student's economic condition.
# For this reason, it can be studied through the ANOVA test against dropouts.
# H0: The groups have the same mean.
# H1: The groups have different means.

# Start by plotting the (jittered) box plot of ISEE factored by Dropout.
# N.B.: The box plot is horrendous, and using outlier.shape = NA to hide outliers
#       does not improve the visualization of the data.

ggplot(dropout_filtered[!is.na(dropout_filtered$valoreIntero), ], aes(x = Dropout, y = valoreIntero, fill = Dropout)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(aes(color = Dropout),
              width = 0.2, size = 1.5, alpha = 0.7) +
  scale_fill_manual(
    values = c("FALSE" = "#FF6961",
               "TRUE" = "#77DD77")) +
  scale_color_manual(
    values = c("FALSE" = "#FF6961",
               "TRUE" = "#77DD77")) +
  labs(title = "ISEE by Dropout Status",
       x = "Dropout Status",
       y = "ISEE") +
  theme_minimal()

# Then, carry out the ANOVA test.

anova_isee <- aov(valoreIntero ~ Dropout, data = dropout_filtered, na.action = na.omit)
summary(anova_isee)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the test
# suggests that the average ISEE varies according to dropout status.
# Since Dropout only has two possible values, I would skip the TukeyHSD test to
# see which group is the one with a different mean.

# However, ISEE is severely affected by an outlier, so, to avoid issues, it can
# be more convenient to apply a log transformation in order to preserve information.

# Again, start by plotting the box plot of ISEE factored by Dropout.
# N.B.: The scaling is weird so if we want to use this plot we should fix it first.

ggplot(dropout_filtered[!is.na(dropout_filtered$valoreIntero), ], aes(x = Dropout, y = valoreIntero, fill = Dropout)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  scale_y_continuous(trans = "log1p") +
  scale_fill_manual(
    values = c("FALSE" = "#FF6961",
               "TRUE" = "#77DD77")) +
  scale_color_manual(
    values = c("FALSE" = "#FF6961",
               "TRUE" = "#77DD77")) +
  labs(
    title = "Log-Transformed ISEE by Dropout Status",
    x = "Dropout Status",
    y = "log1p(ISEE)") +
  theme_minimal()

# Then, carry out the ANOVA test.

log_anova_isee <- aov(log1p(valoreIntero) ~ Dropout, data = dropout_filtered, na.action = na.omit)
summary(log_anova_isee)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the test
# results are the same even after applying the transformation.
# In fact, only the sum/mean squares and the F statistic change, whereas the degrees
# of freedom and the p-value are the same.

# N.B.: For both versions of the test, NA values have been ignored.

# Trying the T test for ISEE ----------------------------------------------

# Take this alternative with a grain of salt as neither the Shapiro test nor the
# Kolmogorov-Smirnov tests worked to check for the normality assumption.

# Alternative check using QQ-Plots.

values_true <- na.omit(dropout_filtered$valoreIntero[dropout_filtered$Dropout == TRUE])

qqnorm(values_true, main = "Q-Q Plot: valoreIntero (Dropout == TRUE)")
qqline(values_true, col = "blue")

values_false <- na.omit(dropout_filtered$valoreIntero[dropout_filtered$Dropout == FALSE])

qqnorm(values_false, main = "Q-Q Plot: valoreIntero (Dropout == FALSE)")
qqline(values_false, col = "darkgreen")

# The T test tries to determine whether the difference in mean of two groups of
# the same variable is statistically significant.
# H0: m1 = m2.
# H1: m1 and m2 are different, or one between m1 < m2 or m1 > m2.

t.test(valoreIntero ~ Dropout, data = dropout_filtered, na.action = na.omit)
t.test(log1p(valoreIntero) ~ Dropout, data = dropout_filtered, na.action = na.omit)

# Having found p < 0.05 in both cases, the null hypothesis is rejected, meaning
# that the means of the two groups have a statistically significant difference.

# Trying the T test for Age (Carlo) ----------------------------------------------

# Create the QQ-Plots to assess whether data are approximately normally distributed.

values_true <- na.omit(dropout_filtered$eta[dropout_filtered$Dropout == TRUE])

qqnorm(values_true, main = "Q-Q Plot: eta (Dropout == TRUE)")
qqline(values_true, col = "blue")

values_false <- na.omit(dropout_filtered$eta[dropout_filtered$Dropout == FALSE])

qqnorm(values_false, main = "Q-Q Plot: eta (Dropout == FALSE)")
qqline(values_false, col = "darkgreen")

# The plots suggest that the normality assumption fails.

# Plot a sideways box plot of age.

ggplot(dropout_filtered[!is.na(dropout_filtered$eta), ], aes(x = Dropout, y = eta, fill = Dropout)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  scale_fill_manual(
    values = c("FALSE" = "#FF6961",
               "TRUE" = "#77DD77")) +
  scale_color_manual(
    values = c("FALSE" = "#FF6961",
               "TRUE" = "#77DD77")) +
  labs(title = "Age by Dropout Status",
       x = "Dropout Status",
       y = "Age") +
  theme_minimal()

# Run the T test.

t.test(eta ~ Dropout, data = dropout_filtered, na.action = na.omit)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the means
# across the groups are different, although the difference is really small.
