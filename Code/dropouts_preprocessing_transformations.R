colnames(dropout_complete)

library(ggplot2)
library(dplyr)

# Removing unnecessary attributes -----------------------------------------

# Since identification flag and course type are not informative, it is possible
# to get rid of them to simplify the workload.

dropout_filtered <- dropout_complete %>% 
  select(-c("flagIdentificato", "tipoCorso"))

View(dropout_filtered)

# Faculty preprocessing ---------------------------------------------------

# Remove the students belonging to a "department" rather than to a "faculty" for
# simplicity/correctness.

filtered_data <- dropout_filtered %>% 
  filter(!grepl("^Dipartimento", facolta)) # remove the matching values

unique(filtered_data$facolta)

# At this point, plot the faculty distribution.
ggplot(filtered_data, aes(x = facolta, fill = facolta)) +
  geom_bar() +
  labs(title = "Bar Plot of Faculty", x = "Faculty", y = "Count") +
  guides(fill = FALSE) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# N.B.: I know it would be crazy but can we find a way to include the legend as
#       well in the plot? Or at least understand where each faculty is located.

# Gender preprocessing ----------------------------------------------------

# Change gender values according to the paper's convention: T = "M", F = "F".

dropout_filtered <- dropout_filtered %>%
  mutate(sesso = ifelse(sesso, "M", "F"))
View(dropout_filtered)

ggplot(dropout_filtered, aes(x = sesso, fill = sesso)) +
  geom_bar() +
  labs(title = "Bar Plot of Gender", x = "Gender", y = "Count")

# Can we just change the plot colours? Because the default is an eyesore.

# Binning ISEE into a categorical variable --------------------------------

# First ISEE transformation: bin it into a categorical variable using the tax
# brackets from Sapienza (the more recent the better).

breaks <- c(0, 15000, 30000, 60000, Inf) # breaks
labels <- c("1", "2", "3", "4") # labels

dropout_filtered$bracket <- cut(temp$isee, breaks = breaks, labels = labels, right = TRUE, include.lowest = TRUE)
dropout_filtered <- dropout_filtered %>% 
  relocate(bracket, .after = valoreIntero)
View(dropout_filtered)

ggplot(dropout_filtered, aes(x = bracket, fill = bracket)) +
  geom_bar() +
  labs(title = "Bar Plot of ISEE Tax Brackets", x = "Tax Bracket", y = "Count")

# N.B.: If we want to exclude NA or 0 values we can do the filtering before the
#       binning.

ggplot(dropout_filtered[!is.na(dropout_filtered$valoreIntero) & dropout_filtered$valoreIntero != 0, ], aes(x = bracket, fill = bracket)) +
  geom_bar() +
  labs(title = "Bar Plot of ISEE Tax Brackets (NA/0 Excluded)", x = "Tax Bracket", y = "Count")

# ISEE transformations (for outliers) -------------------------------------

# The original distribution is strongly right skewed due to an outlier.
ggplot(dropout_filtered, aes(x = valoreIntero)) +
  geom_histogram(
    color = "black",
    fill = "skyblue",
    binwidth = 15000
  ) +
  labs(
    title = "ISEE Histogram",
    x = "ISEE",
    y = "Count"
  )

# Set an ISEE limit to 150000.
ggplot(dropout_filtered, aes(x = valoreIntero)) +
  geom_histogram(
    color = "black",
    fill = "skyblue",
    binwidth = 15000
  ) +
  labs(
    title = "ISEE Histogram",
    x = "ISEE",
    y = "Count"
  ) +
  coord_cartesian(xlim = c(0, 150000))

# Do something similar for the density function.

# The original distribution is affected by an extreme value.
ggplot(filtered_data, aes(x = valoreIntero)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(title = "Distribution of ISEE", x = "ISEE", y = "Probability")

# Set an ISEE limit to 150000.
ggplot(filtered_data, aes(x = valoreIntero)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  coord_cartesian(xlim = c(0, 150000)) +
  labs(title = "Distribution of ISEE", x = "ISEE", y = "Probability")

# N.B.: geom_density gives a warning message due to NA values.
#       We should consider taking non-NA values only in the plot via boolean mask.

# Try to apply a transformation to see if the data can become more informative or
# less affected by skewness/outliers.
# For completeness, I look at the original distribution and at the PDF.

# Log transformation: To deal with 0 values, use log1p(x) = log(1 + x).
ggplot(dropout_filtered, aes(x = log1p(valoreIntero))) +
  geom_histogram(
    color = "black",
    fill = "skyblue",
    binwidth = 1
  ) +
  labs(
    title = "Log-Transformed ISEE Histogram",
    x = "log(1 + ISEE)",
    y = "Count"
  )

ggplot(filtered_data, aes(x = log1p(valoreIntero))) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(title = "Log Distribution of ISEE", x = "log(1 + ISEE)", y = "Probability")

# Root transformation: Just use sqrt(x) as all valid values are non-negative.

ggplot(dropout_filtered, aes(x = sqrt(valoreIntero))) +
  geom_histogram(
    color = "black",
    fill = "skyblue",
    binwidth = 100
  ) +
  labs(
    title = "Root-Transformed ISEE Histogram",
    x = "sqrt(ISEE)",
    y = "Count"
  )

ggplot(filtered_data, aes(x = sqrt(valoreIntero))) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(title = "Root Distribution of ISEE", x = "sqrt(ISEE)", y = "Probability")

# Overall: The log transformation reduces skewness but has a weird PDF, although
#          it seems to handle the outlier fairly well.
#          The root transformation reduces skewness but has a weird PDF and does
#          not get rid of the outlier.

# Last passed exam transformations (for outliers) -----------------------

max(dropout_filtered$ultPren, na.rm = TRUE) # okay (keep it as it is)
max(dropout_filtered$ultSup, na.rm = TRUE) # outlier (try to preprocess it)

# The original distribution is strongly right skewed due to an outlier.
ggplot(dropout_filtered, aes(x = ultSup)) +
  geom_histogram(
    color = "black",
    fill = "steelblue",
    binwidth = 10
  ) +
  labs(
    title = "Time Since Last Passed Exam Histogram",
    x = "Elapsed Time (Months)",
    y = "Count"
  )

# Set a limit with the second highest value.
ggplot(dropout_filtered, aes(x = ultSup)) +
  geom_histogram(
    color = "black",
    fill = "steelblue",
    binwidth = 10
  ) +
  labs(
    title = "Time Since Last Passed Exam Histogram",
    x = "Elapsed Time (Months)",
    y = "Count"
  ) +
  coord_cartesian(xlim = c(0, 200))

# Do something similar for the density function.

# The original distribution is affected by an extreme value.
ggplot(filtered_data, aes(x = ultSup)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Distribution of Last Passed Exam", x = "Elapsed Time (Months)", y = "Probability")

# Set a limit with the second highest value.
ggplot(filtered_data, aes(x = valoreIntero)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  coord_cartesian(xlim = c(0, 200)) +
  labs(title = "Distribution of Last Passed Exam", x = "Elapsed Time (Months)", y = "Probability")

# N.B.: geom_density gives a warning message due to NA values.
#       We should consider taking non-NA values only in the plot via boolean mask.

# Try to apply a transformation to see if the data can become more informative or
# less affected by skewness/outliers.
# For completeness, I look at the original distribution and at the PDF.

# Log transformation: To deal with 0 values, use log1p(x) = log(1 + x).
ggplot(dropout_filtered, aes(x = log1p(ultSup))) +
  geom_histogram(
    color = "black",
    fill = "steelblue",
    binwidth = 1
  ) +
  labs(
    title = "Log-Transformed Last Passed Histogram",
    x = "log(1 + ultSup)",
    y = "Count"
  )

ggplot(filtered_data, aes(x = log1p(ultSup))) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Log Distribution of Last Passed", x = "log(1 + ultSup)", y = "Probability")

# Root transformation: Just use sqrt(x) as all valid values are non-negative.

ggplot(dropout_filtered, aes(x = sqrt(valoreIntero))) +
  geom_histogram(
    color = "black",
    fill = "steelblue",
    binwidth = 50
  ) +
  labs(
    title = "Root-Transformed Last Passed Histogram",
    x = "sqrt(ultSup)",
    y = "Count"
  )

ggplot(filtered_data, aes(x = sqrt(valoreIntero))) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Root Distribution of Last Passed", x = "sqrt(ultSup)", y = "Probability")

# Overall: The log transformation reduces the skew but has a weird PDF, although
#          it seems to handle weird/higher values fairly well.
#          The root transformation is able to reduce skewness and seems to concentrate
#          values well, although the histogram and the PDF both feature a weird
#          peak.