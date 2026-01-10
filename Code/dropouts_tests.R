colnames(dropout_complete)

# Statistical tests -------------------------------------------------------

library(ggplot2)
library(dplyr)

# Study the dataframe to see if there is an association between dropouts and other
# factors, such as, but not limited to, gender, ISEE, average and so on.

# Two tests will be used in particular:
# 1) Chi-Squared test: Associations between two variables.
#    H0: The association is not statistically significant.
#    H1: The association is statistically significant.
#    N.B.: Remeber to make sure that the assumptions always hold
# 2) ANOVA test: Check if average values differ among groups of a variable.
#    H0: The mean is the same for each group.
#    H1: At least one group has a different mean (find it via TukeyHSD).
#    N.B.: Remember how the variance is decomposed for the test (SST = SSR + SSE).

# If possible, check other possible associations within the dataset to expand the
# research question.

# Chi-Squared test on gender ----------------------------------------------

ggplot(dropout_complete, aes(x = sesso, fill = Dropout)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("TRUE" = "#2A52BE",
               "FALSE" = "#003399")) +
  labs(
    title = "Dropouts by Gender",
    x = "Gender",
    y = "Count",
    fill = "Dropout Status"
  ) +
  theme_minimal()

# This plot does not seem to suggest an association between dropouts and gender.

chisq_gender <- table(dropout_complete$sesso,
                      dropout_complete$Dropout)
gender_result <- chisq.test(chisq_gender)
gender_result$expected

# Despite what the bar plot suggests, the test rejects the null hypothesis,
# suggesting a significant association between the variables.

# Chi-Squared test on course year (binned) --------------------------------

ggplot(dropout_complete, aes(x = factor(annoCorso), fill = Dropout)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("TRUE" = "#2A52BE",
               "FALSE" = "#003399")) +
  labs(
    title = "Dropouts by Course Year",
    x = "Course Year",
    y = "Count",
    fill = "Dropout Status"
  ) +
  theme_minimal()

temp <- data.frame(annoCorso = as.factor(dropout_complete$annoCorso),
                   Dropout = dropout_complete$Dropout)
clean_temp <- temp[!is.na(temp$annoCorso),]
chisq_year <- table(clean_temp$annoCorso,
                    clean_temp$Dropout)
year_result <- chisq.test(chisq_year)
year_result$expected

# This test cannot be used as the assumptions (expected frequency >= 5) fails.

# Chi-Squared test on provenience -----------------------------------------

ggplot(dropout_complete, aes(x = cittadinanza, fill = Dropout)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("TRUE" = "#2A52BE",
               "FALSE" = "#003399")) +
  labs(
    title = "Dropouts by Provenience",
    x = "Provenience",
    y = "Count",
    fill = "Dropout Status"
  ) +
  theme_minimal()

# This plot does not seem to suggest an association between dropouts and gender.

chisq_provenience <- table(dropout_complete$cittadinanza,
                           dropout_complete$Dropout)
provenience_result <- chisq.test(chisq_provenience)
provenience_result$expected

# Since p > 0.5, the null hypothesis is kept, suggesting that provenience and
# dropouts are not statistically correlated.
# The only issue is that most students are Italian, so maybe the imbalance could
# cause a bias?

# Chi-Squared test on enrollment type -------------------------------------

ggplot(dropout_complete, aes(x = tipoIscrizione, fill = Dropout)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("TRUE" = "#2A52BE",
               "FALSE" = "#003399")) +
  labs(
    title = "Dropouts by Enrollment Type",
    x = "Provenience",
    y = "Count",
    fill = "Dropout Status"
  ) +
  theme_minimal()

# The plot may suggest a weak association, although "Ripetente" seems problematic.

chisq_enrollment <- table(dropout_complete$tipoIscrizione,
                          dropout_complete$Dropout)
enrollment_results <- chisq.test(chisq_enrollment)
enrollment_results$expected

# The test rejects the null hypothesis, suggesting an association between the variables.
# Maybe the association is T:Ripetente, F:anything else.
# N.B.: Try to see what happens without Ripetente.

# ANOVA test on ISEE ------------------------------------------------------

ggplot(dropout_complete[!is.na(dropout_complete$valoreIntero), ], aes(x = valoreIntero, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 50) +
  scale_fill_manual(
    values = c("TRUE" = "#2A52BE",
               "FALSE" = "#003399")) +
  labs(
    title = "Histogram of ISEE by Dropout Status",
    x = "ISEE",
    y = "Count",
    fill = "Dropout"
  ) +
  theme_minimal()

anova_isee <- aov(valoreIntero ~ Dropout, data = dropout_complete, na.action = na.omit)
summary(anova_isee)

# The test rejects the null hypothesis on the non-NA values.
# N.B.: Here we only have two categories, so this test may not be state of the art statistics.

# ANOVA test on age -------------------------------------------------------

ggplot(dropout_complete[!is.na(dropout_complete$eta), ], aes(x = eta, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 100) +
  scale_fill_manual(
    values = c("TRUE" = "#2A52BE",
               "FALSE" = "#003399")) +
  labs(
    title = "Histogram of Age by Dropout Status",
    x = "Age",
    y = "Count",
    fill = "Dropout"
  ) +
  theme_minimal()

anova_age <- aov(eta ~ Dropout, data = dropout_complete, na.action = na.omit)
summary(anova_age)

# The null hypothesis is rejected.