dropout_complete<-read.csv("C:\\Users\\compu\\Downloads\\yid_out_2m.csv", sep = ";")
View(dropout_complete)
colnames(dropout_complete)

library(ggplot2)
library(dplyr)



# Modifications to the dataset --------------------------------------------

#What we did was: eliminate uninformative columns (Identification and Course Type),
#applied BINNING to make a new categorical variable for tax brackets
#and replaced True and False with M and F in gender column

dropout_filtered <- dropout_complete %>% 
  select(-c("flagIdentificato", "tipoCorso"))

View(dropout_filtered)

dropout_filtered <- dropout_filtered %>%
  mutate(sesso = ifelse(sesso, "M", "F"))
View(dropout_filtered)

#For ISEE I'll keep this division, the tax brackets of Sapienza are like 39 
#(updated to 2014, couldn't find anything else after that)
breaks <- c(0, 15000, 30000, 60000, Inf) # breaks
labels <- c("1", "2", "3", "4") # labels

#Tax brackets:
# 1: 0 - 15.000
# 2: 15.001 - 30.000
# 3: 30.001 - 60.000
# 4: >60.000

dropout_filtered$bracket <- cut(temp$isee, breaks = breaks, labels = labels, right = TRUE, include.lowest = TRUE)
dropout_filtered <- dropout_filtered %>% 
  relocate(bracket, .after = valoreIntero)
View(dropout_filtered)

write.csv(dropout_filtered,"C:\\Users\\compu\\Downloads\\dropout_filtered.csv", row.names = FALSE)

# Useful in the future (transformations)----------------------------------------------------

#Faculty
filtered_data <- dropout_filtered %>% 
  filter(!grepl("^Dipartimento", facolta)) # remove the matching values

unique(filtered_data$facolta)


#ISEE
min(dropout_complete$valoreIntero, na.rm = TRUE) #0
max(dropout_complete$valoreIntero, na.rm = TRUE) #7513842

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
ggplot(dropout_filtered[!is.na(dropout_filtered$valoreIntero), ], aes(x = valoreIntero)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(title = "Distribution of ISEE", x = "ISEE", y = "Probability")

# Set an ISEE limit to 150000.
ggplot(dropout_filtered[!is.na(dropout_filtered$valoreIntero), ], aes(x = valoreIntero)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  coord_cartesian(xlim = c(0, 150000)) +
  labs(title = "Distribution of ISEE", x = "ISEE", y = "Probability")
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



# Last passed exam transformations (for outliers)

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
#IN THIS CASE IT'S NOT JUST AN OUTLIER BUT A COMPLETELY UNREALISTIC VALUE (ERROR)
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
ggplot(dropout_filtered, aes(x = ultSup)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Distribution of Last Passed Exam", x = "Elapsed Time (Months)", y = "Probability")

# Set a limit with the second highest value.
ggplot(dropout_filtered, aes(x = ultSup)) +
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

ggplot(dropout_filtered, aes(x = log1p(ultSup))) +
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

# Visualizations ----------------------------------------------------------

#Course Year
unique(dropout_complete$annoCorso)

ggplot(dropout_complete, aes(x = factor(annoCorso), fill = annoCorso)) +
  geom_bar() +
  labs(title = "Bar Plot of Course Year", x = "Course Year", Y = "Count")

#Age
unique(dropout_complete$eta)

hist(dropout_complete$eta,
     main = "Histogram of Student Age",
     xlab = "Age",
     ylab = "Count",
     breaks = 50,
     col = "skyblue",
     border = "black")

ggplot(dropout_complete, aes(x = factor(eta), fill = eta)) +
  geom_bar() +
  labs(title = "Bar Plot of Student Age", x = "Age", Y = "Count")

#Citizenship
ggplot(dropout_complete, aes(x = cittadinanza, fill = cittadinanza)) +
  geom_bar() +
  labs(title = "Bar Plot of Citizenship", x = "Citizenship", Y = "Count")

#Faculty (DO IT ON DATA WITHOUT DEPARTMENT)
ggplot(filtered_data, aes(x = facolta, fill = facolta)) + 
  geom_bar() + 
  labs(title = "Bar Plot of Faculty", x = "Faculty", y = "Count") +  # Corrected 'Y' to 'y'
  guides(fill = FALSE)+
  theme(
    axis.text.x = element_blank(),     # Remove x-axis labels
    axis.ticks.x = element_blank()     # Remove x-axis ticks
  )

#First/Last Enrollment
ggplot(dropout_complete, aes(x = factor(primaIscr), fill = primaIscr)) +
  geom_bar() +
  labs(title = "Bar Plot of First Enrolment", x = "Time", Y = "Count")

ggplot(dropout_complete, aes(x = factor(ultIscr), fill = ultIscr)) +
  geom_bar() +
  labs(title = "Bar Plot of Last Enrolment", x = "Time", Y = "Count")

#Gender
ggplot(dropout_filtered, aes(x = sesso, fill = sesso)) +
  geom_bar() +
  labs(title = "Bar Plot of Gender", x = "Gender", y = "Count")

#Enrollment type
ggplot(dropout_complete, aes(x = tipoIscrizione, fill = tipoIscrizione)) +
  geom_bar() +
  labs(title = "Bar Plot of Student Type", x = "Student Type", Y = "Count")

#ISEE
ggplot(dropout_filtered, aes(x = bracket, fill = bracket)) +
  geom_bar() +
  labs(title = "Bar Plot of ISEE Tax Brackets", x = "Tax Bracket", y = "Count")

ggplot(dropout_filtered[!is.na(dropout_filtered$valoreIntero) & dropout_filtered$valoreIntero != 0, ], aes(x = bracket, fill = bracket)) +
  geom_bar() +
  labs(title = "Bar Plot of ISEE Tax Brackets (NA/0 Excluded)", x = "Tax Bracket", y = "Count")

# Booked/Current CFU 
hist(dropout_complete$cfuPrenotati,
     main = "Histogram of Booked Credits",
     xlab = "Booked Credits",
     ylab = "Count",
     breaks = 50,
     col = "blue",
     border = "black")

hist(dropout_complete$cfuTake,
     main = "Histogram of Current Credits",
     xlab = "Current Credits",
     ylab = "Count",
     breaks = 50,
     col = "navy",
     border = "black")

#Last Booked/Passed Exam (in months)
ggplot(dropout_filtered, aes(x = ultPren)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  coord_cartesian(xlim = c(0, max(dropout_filtered$ultPren))) +
  labs(title = "Density Plot of Time Since Last Booked Exam", x = "Time (days/months?)", y = "Density")

ggplot(dropout_filtered, aes(x = ultSup)) +
  geom_density(fill = "seagreen", alpha = 0.6) +
  coord_cartesian(xlim = c(0, 193)) +
  labs(title = "Density Plot of Time Since Last Passed Exam", x = "Time (days/months?)", y = "Density")

#Grade Average
hist(dropout_complete$mediaVoto,
     main = "Histogram of Grade Average",
     xlab = "Grade Average",
     ylab = "Count",
     breaks = 50,
     col = "cyan",
     border = "black")

# Since some entries have 0 values, I will try setting a constraint of reducing the
# x-axis to [18, 30].
#The 0 values are for who hasn't taken exams yet

hist(dropout_complete$mediaVoto,
     main = "Histogram of Grade Average",
     xlab = "Grade Average",
     ylab = "Count",
     xlim = c(18, 30),
     breaks = 50,
     col = "cyan",
     border = "black")

#High School Diploma grade
hist(dropout_complete$votoDiploma,
     main = "Histogram of High School Diploma Score",
     xlab = "Diploma Score",
     ylab = "Count",
     breaks = 50,
     col = "aquamarine",
     border = "black")

# For some reason, there is a 0 among these values. I will try setting a constraint
# of reducing the x-axis to [60, 100].
#Different countries have different "passing requirements", so it could be that someone
#has less than 60. No need for rescaling, they have already been rescaled by Tolomei on a 100 scale

hist(dropout_complete$votoDiploma,
     main = "Histogram of High School Diploma Score",
     xlab = "Diploma Score",
     ylab = "Count",
     xlim = c(60, 100),
     breaks = 50,
     col = "aquamarine",
     border = "black")

#Dropout
dropout_complete %>% count(Dropout)

ggplot(dropout_complete, aes(x = Dropout, fill = Dropout)) +
  geom_bar() +
  labs(title = "Bar Plot of Dropout", x = "Dropout", Y = "Count")
