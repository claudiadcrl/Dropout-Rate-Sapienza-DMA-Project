dropout_complete<-read.csv("C:\\Users\\compu\\Downloads\\yid_out_2m.csv", sep = ";")
View(dropout_complete)
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

filtered_data <- dropout_complete %>%
  filter(!grepl("^Dipartimento", facolta))
unique(filtered_data$facolta)

# Plot
ggplot(filtered_data, aes(x = facolta, fill = facolta)) + 
  geom_bar() + 
  labs(title = "Bar Plot of Faculty", x = "Faculty", y = "Count") +  # Corrected 'Y' to 'y'
  guides(fill = FALSE)+
  theme(
    axis.text.x = element_blank(),     # Remove x-axis labels
    axis.ticks.x = element_blank()     # Remove x-axis ticks
  )

#BARPLOT DONE WITHOUT THE PEOPLE WITH DIPARTIMENTO INSTEAD OF FACOLTA (TOO LITTLE, WRONG ENTRIES ON INFOSTUD)
#SHOULD I MODIFY THE DATASET AND NOT CONSIDER WRONG ENTRIES OR JUST EXCLUDE DATA IN ANALYSIS?

#ggplot(dropout_complete, aes(x = facolta, fill = facolta)) +
  #geom_bar() +
  #labs(title = "Bar Plot of Faculty", x = "Faculty", Y = "Count") +
  #guides(fill = FALSE)  

# Identification ----------------------------------------------------------

dropout_filtered <- dropout_complete %>%
  select(-flagIdentificato)
View(dropout_filtered)

#ggplot(dropout_complete, aes(x = factor(flagIdentificato), fill = flagIdentificato)) +
  #geom_bar() +
  #labs(title = "Bar Plot of Identification", x = "Identification", Y = "Count")

#REMOVED FLAGIDENTIFICATO ON COPY OF DATASET 

# Birth Country -----------------------------------------------------------

unique(dropout_complete$nazioneNascita)

# First/Last Enrollment ----------------------------------------------------

# These variables denote how many years have passed since the first/last enrollment.

ggplot(dropout_complete, aes(x = factor(primaIscr), fill = primaIscr)) +
  geom_bar() +
  labs(title = "Bar Plot of First Enrolment", x = "Time", Y = "Count")

ggplot(dropout_complete, aes(x = factor(ultIscr), fill = ultIscr)) +
  geom_bar() +
  labs(title = "Bar Plot of Last Enrolment", x = "Time", Y = "Count")

# The first enrollment features a much longer tail compared to the last enrollment.
# We can make pairwise comparisons when testing on enrollment years.

# Gender ------------------------------------------------------------------

#val = {"M": True, "F": False}

#IF WE WANT TO CHANGE DATASET VALUES

dropout_filtered <- dropout_filtered %>%
  mutate(sesso = ifelse(sesso, "M", "F"))
View(dropout_modified)

ggplot(dropout_filtered, aes(x = sesso, fill = sesso)) +
  geom_bar() +
  labs(title = "Bar Plot of Gender", x = "Gender", y = "Count")
  
#IF WE WANT TO CHANGE IT AT ANALYSIS

ggplot(dropout_complete, aes(x = sesso, fill = sesso)) +
  geom_bar() +
  labs(title = "Bar Plot of Gender", x = "Gender", y = "Count") +  # Corrected 'Y' to 'y'
  scale_x_discrete(labels = c("False" = "F", "True" = "M")) +
  scale_fill_discrete(labels = c("False" = "F", "True" = "M"))


#Old code:
#ggplot(dropout_complete, aes(x = sesso, fill = sesso)) +
  #geom_bar() +
  #labs(title = "Bar Plot of Gender", x = "Gender", Y = "Count")

# The plot suggests there are more FALSE students than TRUE students.

# I would do Chi-Squared to tests dropout against gender to see if males/females
# seem to be more likely to drop out compared to the others or not.

# Course Type -------------------------------------------------------------

#CHECK WHAT IT IS
#FROM DOCUMENTATION:"tipoCorso : type of the program code. The meaning of the code is unknown"
#I'D SAY WE CAN REMOVE IT

dropout_filtered <- dropout_filtered %>%
  select(-tipoCorso)
View(dropout_filtered)

#FOR STUDENT TYPE
#1 : ex-509
#◦ 2 : DM-270 with study plan
#◦ 3 (or others): DM-270 without study plan
#◦ 0 : not enrolled
#IDK IF IT CAN BE INFORMATIVE, WE COULD REMOVE IT

# Enrollment Type ----------------------------------------------------------

unique(dropout_complete$tipoIscrizione)

ggplot(dropout_complete, aes(x = tipoIscrizione, fill = tipoIscrizione)) +
  geom_bar() +
  labs(title = "Bar Plot of Student Type", x = "Student Type", Y = "Count")

# Most students are "in corso", whereas "ripetente" is a sort of outlier.

# We can also make comparisons on grade average or dropout rates among enrollment
# types.

# ISEE --------------------------------------------------------------------

min(dropout_complete$valoreIntero, na.rm = TRUE) #0
max(dropout_complete$valoreIntero, na.rm = TRUE) #7513842

filtered_data <- dropout_filtered %>%
  filter(!is.na(valoreIntero) & valoreIntero != 0)

# Plot the histogram
ggplot(filtered_data, aes(x = valoreIntero)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  coord_cartesian(xlim = c(0, 150000)) +
  labs(title = "Distribution of ISEE", x = "ISEE", y = "Count")

#RESCALE ISEE or TURN CATEGORICAL (WITH SAPIENZA TAX BRAKES)
#the problem is that the highest Isee is 7.5M, so the distribution is veeeery right skewed
#Do we: rescale, apply a transformation or make categorical? I can look into what we've done in class
#I think we might first add a column to get the categorical range (tax bracket) and then transform




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

#THEY'RE BOTH IN MONTHS
#max is 2238 months --> 186 years??
#I'd exclude unrealistic values and set a range from the next biggest value (193 months-->16 years)
  
ggplot(dropout_filtered, aes(x = ultPren)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  coord_cartesian(xlim = c(0, max(dropout_filtered$ultPren))) +
  labs(title = "Density Plot of Time Since Last Booked Exam", x = "Time (days/months?)", y = "Density")
#weird, very wavy

ggplot(dropout_filtered, aes(x = ultSup)) +
  geom_density(fill = "seagreen", alpha = 0.6) +
  coord_cartesian(xlim = c(0, 193)) +
  labs(title = "Density Plot of Time Since Last Passed Exam", x = "Time (days/months?)", y = "Density")
#very right skewed


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
#-->To see later, but i'd say we can keep it like this

# High School Diploma -----------------------------------------------------

hist(dropout_complete$votoDiploma,
     main = "Histogram of High School Diploma Score",
     xlab = "Diploma Score",
     ylab = "Count",
     breaks = 50,
     col = "aquamarine",
     border = "black")

#THEY HAVE BEEN RESCALED IN CODE 

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
