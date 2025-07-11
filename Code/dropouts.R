colnames(dropout_complete)

# Data preprocessing and preparation --------------------------------------

library(ggplot2)
library(dplyr)

# Look at the various features and try to normalize data where needed.
# N.B.: I will skip some attributes as it would not make sense to process them.

# Course Year -------------------------------------------------------------

unique(dropout_complete$annoCorso) # how tf are there people on the 18th year...

ggplot(dropout_complete, aes(x = factor(annoCorso), fill = annoCorso)) +
  geom_bar() +
  labs(title = "Bar Plot of Course Year", x = "Course Year", Y = "Count")

# Most students happen to be in their first three years.
# Most of the "fuori corso" students are instead in their fourth/fifth year.

# Overall, I would keep the data as they are and just do factor() to make it
# categorical (useful for tests imo).

# Age ---------------------------------------------------------------------

unique(dropout_complete$eta) # I want to meet the 52 year old student(s)

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

# The distribution presents a strong right skew as most values are centered around
# 22-25 years of age, although values range from 19 to 52 (I say anything >= 42
# should be considered as an outlier).

# I tried using a log transformation, but it did not seem informative to me so I would
# discard it and keep the data as they are.

# Citizenship -------------------------------------------------------------

ggplot(dropout_complete, aes(x = cittadinanza, fill = cittadinanza)) +
  geom_bar() +
  labs(title = "Bar Plot of Citizenship", x = "Citizenship", Y = "Count")

# The plot suggests that most students are from Italy.
# I would focus on birth country though because it could be more informative to
# understand from which parts of EU/non-EU the international students are from.

# Faculty -----------------------------------------------------------------

unique(dropout_complete$facolta)

ggplot(dropout_complete, aes(x = facolta, fill = facolta)) +
  geom_bar() +
  labs(title = "Bar Plot of Faculty", x = "Faculty", Y = "Count")

# The plot literally crashed lmao, but I would keep the category for tests.

# Identification ----------------------------------------------------------

ggplot(dropout_complete, aes(x = factor(flagIdentificato), fill = flagIdentificato)) +
  geom_bar() +
  labs(title = "Bar Plot of Identification", x = "Identification", Y = "Count")

# I personally do not think this is informative but you guys let me know.

# Birth Country -----------------------------------------------------------

unique(dropout_complete$nazioneNascita)

# I would use this for tests. It could come in handy to see tests on whether there
# exists an association between dropouts an country (or we can just stick to the
# Italy VS EU VS Non-EU as the grouping would be way easier).

# First/Last Enrolment ----------------------------------------------------

# These variables denote how many years have passed since the first/last enrolment.

ggplot(dropout_complete, aes(x = factor(primaIscr), fill = primaIscr)) +
  geom_bar() +
  labs(title = "Bar Plot of First Enrolment", x = "Time", Y = "Count")

ggplot(dropout_complete, aes(x = factor(ultIscr), fill = ultIscr)) +
  geom_bar() +
  labs(title = "Bar Plot of Last Enrolment", x = "Time", Y = "Count")

# The first enrolment features a much longer tail compared to the last enrolment.
# We can make pairwise comparisons when testing on enrolment years.

# Gender ------------------------------------------------------------------

# N.B.: The variable takes values TRUE/FALSE, but the paper does not specify which
#       is which (ah yes the two genders: TRUE and FALSE).

ggplot(dropout_complete, aes(x = sesso, fill = sesso)) +
  geom_bar() +
  labs(title = "Bar Plot of Gender", x = "Gender", Y = "Count")

# The plot suggests there are more FALSE students than TRUE students.

# I would do Chi-Squared to tests dropout against gender to see if males/females
# seem to be more likely to drop out compared to the others or not.

# Course Type -------------------------------------------------------------

# This should distinguish courses by type, but I am not sure if it is going to be
# more useful than faculty or viceversa.

# Enrolment Type ----------------------------------------------------------

unique(dropout_complete$tipoIscrizione)

ggplot(dropout_complete, aes(x = tipoIscrizione, fill = tipoIscrizione)) +
  geom_bar() +
  labs(title = "Bar Plot of Student Type", x = "Student Type", Y = "Count")

# Most students are "in corso", whereas "ripetente" is a sort of outlier.

# We can also make comparisons on grade average or dropout rates among enrolment
# types.

# ISEE --------------------------------------------------------------------

unique(dropout_complete$valoreIntero)
max(!is.na(dropout_complete$valoreIntero))

hist(dropout_complete$valoreIntero,
     main = "Histogram of ISEE",
     xlab = "ISEE",
     ylab = "Count",
     breaks = 500,
     col = "gold",
     border = "black")

# R is weird with the scale. I tried fixing on xlim but it was giving me issues.

# I will try making a bar plot dividing ISEE entries according to official ranges.
# Reference: https://www.polimi.it/studenti/tasse-universitarie/graduazione-del-contributo-onnicomprensivo
# Yes I know the stat is from Polimi but I did not find one for Sapienza.

temp <- data.frame(
  id = dropout_complete$ID_Stud,
  isee = dropout_complete$valoreIntero
)

breaks <- c(0, 23120, 27000, 31000, 40000, 51000, 63000, 75000, 95000, Inf) # breaks
labels <- c("1", "2", "3", "4", "5", "6", "7", "8", "Full") # labels

temp$range <- cut(temp$isee, breaks = breaks, labels = labels, right = TRUE, include.lowest = TRUE)
View(temp)

ggplot(temp, aes(x = range, fill = range)) +
  geom_bar() +
  labs(title = "Bar Plot of ISEE Ranges", x = "Range", y = "Count")

# The bar plot suggests that most students' declared ISEE lies either in range 1
# or in range 5.
# I kept NA, but we can filter using dplyr when making temp so that the bar plot
# does not feature students with no declared ISEE.

# Booked/Current CFU ------------------------------------------------------

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

# The booked CFU distribution is strongly right skewed, although I do not think
# this variable is informative.

# The current CFU distribution is also skewed. I would have applied a log transformation,
# but since the possible values also depend on the course type (current credits are
# bounded by total credits, which are 180 for bachelor's, 120 for master's, 300 for ciclo
# unico (law, architecture) and iirc 360 for medicine).

hist(log(dropout_complete$cfuTake),
     main = "Log Histogram of Current Credits",
     xlab = "Log of Current Credits",
     ylab = "Count",
     breaks = 50,
     col = "navy",
     border = "black")

# Judging from the plot I would not do the transformation anyways.

# Last Booked/Passed Exam -------------------------------------------------

# These variables denote how many years (?) have passed since the last booking/passed
# exam.

ggplot(dropout_complete, aes(x = factor(ultPren), fill = ultPren)) +
  geom_bar() +
  labs(title = "Bar Plot of Last Exam Booking", x = "Time", Y = "Count")

ggplot(dropout_complete, aes(x = factor(ultSup), fill = ultSup)) +
  geom_bar() +
  labs(title = "Bar Plot of Last Passed Exam", x = "Time", Y = "Count")

# The last passed exam does not look informative because it seems very messy, but
# last exam booking could be interesting to analyze.

# Grade Average -----------------------------------------------------------

hist(dropout_complete$mediaVoto,
     main = "Histogram of Grade Average",
     xlab = "Grade Average",
     ylab = "Count",
     breaks = 50,
     col = "cyan",
     border = "black")

# Since some entries have 0 values, I will try setting a constraint of reducing the
# x-axis to [18, 30].

hist(dropout_complete$mediaVoto,
     main = "Histogram of Grade Average",
     xlab = "Grade Average",
     ylab = "Count",
     xlim = c(18, 30),
     breaks = 50,
     col = "cyan",
     border = "black")

# The [18, 30] average makes sense logically, but many entries would be ignored.
# Here, I would probably normalize the scores in [0, 1] if needed so that maybe
# it will be easier to find patterns during statistical tests.

# High School Diploma -----------------------------------------------------

hist(dropout_complete$votoDiploma,
     main = "Histogram of High School Diploma Score",
     xlab = "Diploma Score",
     ylab = "Count",
     breaks = 50,
     col = "aquamarine",
     border = "black")

# For some reason, there is a 0 among these values. I will try setting a constraint
# of reducing the x-axis to [60, 100].

hist(dropout_complete$votoDiploma,
     main = "Histogram of High School Diploma Score",
     xlab = "Diploma Score",
     ylab = "Count",
     xlim = c(60, 100),
     breaks = 50,
     col = "aquamarine",
     border = "black")

# I would ignore 0 and NA values and then normalize the actual diploma value to
# be in [0, 1] via x' = (x - min(X)) / (max(X) - min(X)).

# Dropout -----------------------------------------------------------------

dropout_complete %>% count(Dropout)

ggplot(dropout_complete, aes(x = Dropout, fill = Dropout)) +
  geom_bar() +
  labs(title = "Bar Plot of Dropout", x = "Dropout", Y = "Count")

# Roughly 32% of the students dropped out.
# I would use the table() command if we want to use it for tests (especially for
# Chi-Squared test).