library(ggplot2)
library(dplyr)
library(igraph)
library(RColorBrewer)
library(sqldf)


dropout_filtered<-read.csv("C:\\Users\\compu\\Downloads\\dropout_filtered.csv")

# Faculty -----------------------------------------------------------------

#Visualization

#Follow same structure used for nationalities
# Step 1: Calculate dropout rate per faculty
#FILTER OUT DEPARTMENTS and INTERFACOLTA'
#keep only the 11 official faculties (for example, no psicologia 1 or scienze statistiche on their own)
faculties_list<-c("ARCHITETTURA", "ECONOMIA", "FARMACIA E MEDICINA", "GIURISPRUDENZA","INGEGNERIA CIVILE E INDUSTRIALE", "INGEGNERIA DELL'INFORMAZIONE, INFORMATICA E STATISTICA", "LETTERE E FILOSOFIA", "MEDICINA E ODONTOIATRIA", "MEDICINA E PSICOLOGIA", "SCIENZE MATEMATICHE, FISICHE E NATURALI", "SCIENZE POLITICHE, SOCIOLOGIA, COMUNICAZIONE", "SCUOLA DI INGEGNERIA AEROSPAZIALE")

dropout2 <- dropout_filtered %>%
  filter(facolta %in% faculties_list) %>%
  group_by(facolta) %>%
  summarise(
    total = n(),
    dropout = mean(Dropout == "True")
  ) %>%
  arrange(desc(dropout))

# Step 2: Set factor order by dropout rate for plotting
dropout2$facolta <- factor(dropout2$facolta, levels = dropout2$facolta)
View(dropout2)


# Step 3: Plot dropout rate barplot
ggplot(dropout2, aes(x = facolta, y = dropout)) +
  geom_col(fill = "gray", width = 0.3) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.1) # Add a tick every 10%
  ) +
  labs(
    title = "Dropout Rate by Faculty",
    x = "Dropout Rate",
    y = "Faculty"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )





#CHI-SQUARED TEST

#Need to filter out those faculties for which we have less than 5 students per dropout status by requirements of test
faculty_counts <- dropout_filtered %>%
  count(facolta) %>%
  filter(facolta %in% faculties_list) %>%
  filter(n > 5)#less didn't achieve requirements
df<-dropout_filtered
df_filtered <- df %>%
  semi_join(faculty_counts, by = "facolta")
View(df_filtered)

table_nation <- table(df_filtered$facolta, df_filtered$Dropout)
chisq.test(table_nation)

#p value (2.2e-16) << 0.05
#THERE IS ASSOCIATION





#NETWORK

#Make dataset with connections (edges)
#Take dropout2, which has the faculties, the total of students and the dropout rates
#Join it with itself on conditions:
#                                   - Faculties must be different
#                                   - difference in dropout rate <0.1 (10%)
#                                   - to avoid duplicates: first faculty < second faculty (alphabetical order)

pairs <- sqldf("
  SELECT 
    A.facolta AS from_f,
    B.facolta AS to_f,
    A.dropout AS from_dropout,
    B.dropout AS to_dropout,
    ABS(A.dropout - B.dropout) AS rate_diff,
    1.0 / (1.0 + ABS(A.dropout - B.dropout)) AS similarity
  FROM dropout2 A
  JOIN dropout2 B
    ON A.facolta < B.facolta
   AND ABS(A.dropout - B.dropout) < 0.1
")
pairs<-rename(pairs, from=from_f)
pairs<-rename(pairs, to=to_f)

min_sim <- min(pairs$similarity)
max_sim <- max(pairs$similarity)

temp<-(pairs$similarity - min_sim)/(max_sim - min_sim)
pairs$similarity_norm <- 1+1*temp
View(pairs)

nodes<-dropout2
edges<-pairs
fnet <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)
E(fnet)$weight <- edges$similarity_norm
dropout_values <- V(fnet)$dropout
node_colors <- colorRampPalette(c("green", "yellow", "red"))(100)[as.numeric(cut(dropout_values, breaks = 100))]

plot(fnet,
     layout=layout_with_fr(fnet)*5,
     vertex.color = node_colors,
     vertex.label = V(fnet)$name,
     vertex.size = 10,
     vertex.frame.color = NA,
     vertex.label.cex = 0.7,
     vertex.label.font = 2,     
     vertex.label.family = "sans",  
     vertex.label.color="black",
     edge.color = "grey",
     edge.width = E(fnet)$weight*2,
     main = "Network of Faculties by Dropout Rates"
)
legend(
  "topright",
  legend = c("Low", "Medium", "High"),
  fill = colorRampPalette(c("green", "yellow", "red"))(3),
  border = NA,
  title = "Dropout Rate",
  bty = "n"
)


# Course Year -------------------------------------------------------------

# Start by plotting the course year distribution by dropout status.

ggplot(dropout_filtered[!is.na(dropout_filtered$annoCorso), ], aes(x = annoCorso, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 10) +
  scale_fill_manual(
    values = c(
      "True" = "#FF6961",
      "False" = "#77DD77")) +
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
      "True" = "#FF6961",
      "False" = "#77DD77")) +
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

ggplot(dropout_filtered[!is.na(dropout_filtered$annoCorso), ], aes(sample = log(annoCorso))) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~ Dropout)
#log improves it a bit, so we can assume normality by clt

var.test(log(annoCorso) ~ Dropout, data = dropout_filtered) #diff variances --> t test

t.test(log(annoCorso) ~ Dropout, data = dropout_filtered, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
#BOTH NORMAL AND WITH LOG TRANSFORMATION NULL HYPOTHESIS IS REJECTED --> COHERENT

# However, the distribution seems highly skewed, so I would avoid studying the t
# test and try the Chi-Squared test.


#BINNING (run chi squared to confirm results of t test)
# Start by plotting the bar plot of binned course year.

ggplot(dropout_filtered[!is.na(dropout_filtered$annoCorso), ], aes(x = factor(annoCorso), fill = Dropout)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("True" = "#FF6961",
               "False" = "#77DD77")) +
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

counts <- dropout_filtered %>%
  count(annoCorso, Dropout)
dropout_filtered2 <- dropout_filtered %>%
  semi_join(
    counts %>% filter(n > 9), #so that we meet assumptions
    by = c("annoCorso", "Dropout")
  )

chisq_course_year <- table(factor(dropout_filtered2$annoCorso), dropout_filtered2$Dropout)
course_year_result <- chisq.test(chisq_course_year)
print(course_year_result) # The assumptions do not hold true.

# Having found p < 0.05, the null hypothesis is rejected, meaning that the
# association between dropout status and course year is statistically significant

#Results coherent with t test

# CFU Ratio ---------------------------------------------------------------

df_cfu<-dropout_filtered
df_cfu$ratio<-df_cfu$cfuTake/df_cfu$creditiTotali
#The people that have a ratio >1 are people that probably come from another course/ have more degrees, so they have done more exams
#Kept since smoothes out a bit the distribution
df_clean <- df_cfu[
  is.finite(df_cfu$ratio) & 
    !is.na(df_cfu$Dropout), 
]

#Density plot
ggplot(df_clean, aes(x = ratio, fill = Dropout, color = Dropout)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Dropout) +
  scale_fill_manual(values = c("True" = "red", "False" = "green")) +
  scale_color_manual(values = c("True" = "red", "False" = "green")) +
  labs(
    title = "Density of Credit Ratio by Dropout Status",
    x = "Ratio of CFU Taken to Total Credits",
    y = "Density"
  ) +
  theme_minimal()

#Boxplot
boxplot(ratio ~ Dropout, data = df_clean,
        main = "CFU ratio by Dropout Status",
        xlab = "Dropout",
        ylab = "CFU ratio",
        col = c("#77DD77", "#FF6961")
)

#CHECK ANOVA ASSUMPTIONS
#transformations don't make it normal
#normal log excludes all 0s, which takes away info, while log1p makes distribution worse
#sqrt doesn't help either
ggplot(df_clean, aes(sample = ratio)) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~ Dropout)

#cannot really perform test (too skewed to assume normality by clt, even if obs>30 for each group)

#t test (in case we can assume normality by clt, even if not exactly correct)
result<-t.test(ratio ~ Dropout, data = df_clean)
print(result)

#p-value(2.2e-16)<0.05 --> difference in mean ratio between dropout statuses is statistically significant
#mean ratio for Dropout=False: 0.5076315
#mean ratio for Dropout=True: 0.2523544

#same results obtained also by applying log (which doesn't help with skeweness) and
#by removing outliers (cfu ratio >1)
#binning not feasible in this case

# Last Enrollment ---------------------------------------------------------

# Start by plotting the distribution by dropout status.

ggplot(dropout_filtered, aes(x = ultIscr, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  scale_fill_manual(
    values = c(
      "True" = "#FF6961",
      "False" = "#77DD77")) +
  labs(
    title = "Histogram of Last Enrollment by Dropout Status",
    x = "Last Enrollment (Years)",
    y = "Count",
    fill = "Dropout Status") +
  theme_minimal()

#Proportional barplot (maybe clearer?)
ggplot(dropout_filtered, aes(x = ultIscr, fill = Dropout)) +
  geom_bar(position = "fill") +  # Proportion per group
  scale_fill_manual(
    values = c(
      "True" = "#FF6961",
      "False" = "#77DD77")) +
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

ggplot(dropout_filtered, aes(sample = log(ultIscr))) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~ Dropout)

var.test(ultIscr ~ Dropout, data = dropout_filtered, na.action = na.omit)

t.test(ultIscr ~ Dropout, data = dropout_filtered, na.action = na.omit, var.equal = FALSE)
# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# N.B.: Due to a poor approximation, these results could be less reliable.

#With log transformation
ggplot(dropout_filtered, aes(sample = log(ultIscr))) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~ Dropout)

var.test(log(ultIscr) ~ Dropout, data = dropout_filtered, na.action = na.omit)

t.test(log(ultIscr) ~ Dropout, data = dropout_filtered, na.action = na.omit, var.equal = FALSE)
# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
#Results consistent with previous result

#Binning not really feasible(?)



# Last Booked Exam --------------------------------------------------------

# Start by plotting the distribution by dropout status.

ggplot(dropout_filtered, aes(x = ultPren, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity") +
  scale_fill_manual(
    values = c(
      "True" = "#FF6961",
      "False" = "#77DD77")) +
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

ggplot(dropout_filtered, aes(sample = ultPren)) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~ Dropout)

var.test(ultPren ~ Dropout, data = dropout_filtered, na.action = na.omit)

t.test(ultPren ~ Dropout, data = dropout_filtered, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# mean in group False: 3.988821
# mean in group True: 7.486217
# N.B.: Due to a poor approximation, these results could be less reliable.

#With log1p transformation (log not feasible for 0 values)
ggplot(dropout_filtered, aes(sample = log1p(ultPren))) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~ Dropout)

var.test(log1p(ultPren) ~ Dropout, data = dropout_filtered, na.action = na.omit)

t.test(log1p(ultPren) ~ Dropout, data = dropout_filtered, na.action = na.omit, var.equal = FALSE)
# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# mean in group False: 1.378371            
# mean in group True: 2.084591

#Results consistent with previous result

#try binning
ggplot(dropout_filtered, aes(x = factor(ultPren), fill = Dropout)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("True" = "#FF6961",
               "False" = "#77DD77")) +
  labs(
    title = "Dropouts by Last Booked Exam",
    x = "Last Booked Exam",
    y = "Count",
    fill = "Dropout Status") +
  theme_minimal()

counts <- dropout_filtered %>%
  count(ultPren, Dropout)
dropout_filtered2 <- dropout_filtered %>%
  semi_join(
    counts %>% filter(n > 9), #so that we meet assumptions
    by = c("ultPren", "Dropout")
  )

chisq_course_year <- table(factor(dropout_filtered2$ultPren), dropout_filtered2$Dropout)
course_year_result <- chisq.test(chisq_course_year)
print(course_year_result)
# Having found p < 0.05, the null hypothesis is rejected, meaning that the
# association between dropout status and course year is statistically significant

#Results coherent with t test



# Last Passed Exam --------------------------------------------------------

# Start by plotting the distribution by dropout status.
#IN THIS CASE IT'S NOT JUST AN OUTLIER BUT A COMPLETELY UNREALISTIC VALUE (ERROR)
#Always ignore it, not just in visualization
p<-dropout_filtered[dropout_filtered$ultSup <= 1000, ]
ggplot(p, aes(x = ultSup, fill = Dropout)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20) +
  scale_fill_manual(
    values = c(
      "True" = "#FF6961",
      "False" = "#77DD77")) +
  labs(
    title = "Histogram of Last Passed Exam by Dropout Status",
    x = "Last Passed Exam (Months)",
    y = "Count",
    fill = "Dropout Status") +
  theme_minimal()

#maybe fix colors?

# In order to check for an association between last passed exam and dropout status,
# it is possible to use the two-sided t test, which checks whether two groups of
# a variable have the same mean or not.
# H0: mX = mY.
# H1: mX and mY are different.

# Start by checking the assumptions of normality and homogeneity of variance.

qqnorm(na.omit(p$ultSup), main = "Q-Q Plot of Last Passed Exam")
qqline(na.omit(p$ultSup), col = "red")

ggplot(p, aes(sample = ultSup)) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~ Dropout)

var.test(ultSup ~ Dropout, data = p, na.action = na.omit)

t.test(ultSup ~ Dropout, data = p, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# mean in group False: 6.520383
# mean in group True: 16.692780
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
      "True" = "#FF6961",
      "False" = "#77DD77")) +
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

ggplot(avg_dropout, aes(sample = mediaVoto)) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~ Dropout)

var.test(mediaVoto ~ Dropout, data = avg_dropout) #diff means --> t test

t.test(mediaVoto ~ Dropout, data = avg_dropout, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# mean in group False: 25.58394 
# mean in group True: 24.91012 
# Assume normality by CLT

#Binning
ggplot(avg_dropout, aes(x = factor(mediaVoto), fill = Dropout)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("True" = "#FF6961",
               "False" = "#77DD77")) +
  labs(
    title = "Dropouts by Average Grade",
    x = "Average Grade",
    y = "Count",
    fill = "Dropout Status") +
  theme_minimal()

counts <- avg_dropout %>%
  count(mediaVoto, Dropout)
avg_dropout2 <- avg_dropout %>%
  semi_join(
    counts %>% filter(n > 5), #so that we meet assumptions
    by = c("mediaVoto", "Dropout")
  )

chisq_course_year <- table(factor(avg_dropout2$mediaVoto), avg_dropout2$Dropout)
course_year_result <- chisq.test(chisq_course_year)
print(course_year_result) # The assumptions do not hold true.

# Having found p < 0.05, the null hypothesis is rejected, meaning that the
# association between dropout status and course year is statistically significant

#Results coherent with t test


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
      "True" = "#FF6961",
      "False" = "#77DD77")) +
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

ggplot(dip_dropout, aes(sample = votoDiploma)) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~ Dropout)

var.test(votoDiploma ~ Dropout, data = dip_dropout) #diff variances --> t test

t.test(votoDiploma ~ Dropout, data = dip_dropout, na.action = na.omit, var.equal = FALSE)

# Having found p < 0.05, the null hypothesis is rejected, meaning that the groups
# have different means.
# While there is a skew towards the -3 zone, I would consider this to be fairly
# approximately normal, especially compared to the other two distributions.