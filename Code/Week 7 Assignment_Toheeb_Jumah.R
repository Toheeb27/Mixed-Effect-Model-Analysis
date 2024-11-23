#Week 07 - MC1-M231 - Programming, Data Science and Statistics 3

#Objective: Applying the understanding of Mixed effect Model by performing a comprehensive analysis on a real-world dataset using R.


# Tasks
# 1. Data Exploration.
#   a. Load the "sleepstudy" dataset.
#   b. Explore the structure of the dataset.
#   c. Visualize the data using appropriate plots to understand the distribution and relationships.
# 2. Descriptive Statistics.
#   a. Compute and report summary statistics for the key variables.
#   b. Create visualizations (e.g., histograms, boxplots) to better understand the distribution of reaction times over different days.
# 3. Fit an adequate Model(s).
# 4. Interpret the results.
# 5. Residual Analysis

########################
###1. Data Exploration##
########################

#(A): Loading the relevant library and the "sleepstudy" dataset

#Loading the relevant library
library(Matrix)
library(lme4)
library(tidyverse)

#Loading the dataset
data("sleepstudy")

#(B): Expolring the dataset content
#exploring the inbuilt detailed information of the dataset in R
?sleepstudy

#Exploring the content of the dataset using view function from tidyverse
view(sleepstudy)

#Checking the data structure of the dataset
str(sleepstudy)

#Exploring the overall summary statistics of the dataset
summary(sleepstudy)

#(C):Visualization of the dataset to understand the distribution
#A histogram plot to understand the frequency distribution of the reaction time 
sleepstudy %>% ggplot(aes(x = Reaction)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Reaction Times", x = "Reaction Time (ms)", y = "Frequency")

#A density plot to better understand the histogram by showing the plateau of the reaction time
sleepstudy %>% ggplot(aes(x = Reaction)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot of Reaction Times", x = "Reaction Time (ms)", y = "Density")

#A density plot of reaction time of each subject to understand the variation in their distribution
sleepstudy %>%ggplot(aes(x = Reaction, fill = as.factor(Subject))) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of Reaction Time by Subjects", x = "Reaction Time (ms)", y = "density")

#checking the normality of the data (reaction time) using qqnorm
qqnorm(sleepstudy$Reaction)
qqline(sleepstudy$Reaction, col = "blue")

#A Boxplot of reaction time variability accross the 18 Subject, to understand the subject with the highest and lowest reaction time
sleepstudy %>% ggplot(aes(x = factor(Subject), y = Reaction)) +
  geom_boxplot() +
  labs(title = "Boxplot Reaction Time by Subject", x = "Subject", y = "Reaction Time")

##############################
###2. Descriptive Statistics##
##############################

#(A):Computing the summary statistics of the key variables 

#The key variable in the dataset is "Reaction", which represent the reaction time. The summary statistics is below.

#Checking for NA before computing the summary statistics
anyNA(sleepstudy)

#Computing the summary statistics of the reaction time.
summary(sleepstudy$Reaction)

#Computing the Summary statistics of Reaction Time grouped by Days
summary_stats_per_day <- sleepstudy %>%
  group_by(Days) %>%
  summarise(
    Mean = mean(Reaction),        
    Median = median(Reaction),    
    SD = sd(Reaction),             
    Min = min(Reaction),           
    Max = max(Reaction),           
    Count = n()                                  
  )

print(summary_stats_per_day)
as_tibble(summary_stats_per_day)

#Computing the Summary statistics of Reaction Time grouped by Subject
summary_stats_per_subject <- sleepstudy %>%
  group_by(Subject) %>%
  summarise(
    Mean = mean(Reaction),        
    Median = median(Reaction),    
    SD = sd(Reaction),             
    Min = min(Reaction),           
    Max = max(Reaction),           
    Count = n()                                  
  )

print(summary_stats_per_subject)
as_tibble(summary_stats_per_subject)

#Calculating the average reaction time for each day of the study
avg_reaction <- sleepstudy %>%
  group_by(Days) %>%
  summarize(avg_reaction = round(mean(Reaction), 2))

print(avg_reaction)

#(B): Visualization for understanding the relationship between reaction time over different days.

#A Scatter plot of Reaction time over different Days of the study, showing the reaction time is increasing as participants are deprived of sleep.
?geom_smooth
sleepstudy %>% ggplot(aes(x = Days, y = Reaction)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Reaction Time by Days", x = "Days", y = "Reaction Time (ms)")

#A Boxplot of reaction time over different days of the study showing the variability of the median for each day despite the increasing nature of the reaction time.
sleepstudy %>% ggplot(aes(x = factor(Days), y = Reaction)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Reaction Times by Days",
       x = "Days", y = "Reaction Time (ms)")

#A histogram plot of the average reaction time per day showing the increasing or decreasing reaction times across different days of the study
avg_reaction %>% ggplot(aes(x = factor(Days), y = avg_reaction)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Reaction Times by Days",
       x = "Days", y = "Average Reaction Time (ms)")

#A line plot to better demonstrate the increasing or decreasing characteristics of average reaction times across different days throughout the study
avg_reaction %>% ggplot(aes(x = Days, y = avg_reaction)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = " Line Plot Average Reaction Times by Days",
       x = "Days", y = "Average Reaction Time (ms)")

#A density plot of reaction time of the participant for each day
sleepstudy %>%ggplot(aes(x = Reaction, fill = as.factor(Days))) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of Reaction Time by Days", x = "Reaction Time (ms)", y = "density")

#################################
###3. Fitting an adequate model##
#################################

#removing the data-point of the first two days, because it was observed from the study (Belenky et. al, 2003) that the effect of the study was sleep deprivation was noticeable from the third day. Hence the datapoint for the first three days of the study was removed.
filtered_sleepstudy <- sleepstudy %>%
  filter(Days > 2)

view(filtered_sleepstudy)

#plotting the model
model_sleepstudy <- lmer(Reaction ~ Days + (1| Subject), data = filtered_sleepstudy)

summary(model_sleepstudy)

#############################
###4. Interpret the results##
#############################

#The coefficient from the fixed Effect for Days represents the change in reaction time for each additional day of sleep deprivation.

#Random Effect (Subject): The random effect (REML estimate) shows how much individual variability affects reaction time, independent of the number of "days of sleep deprivation".

#The model estimated baseline for the average reaction time is 247.702 ms for the Day 3, which is our reference level and for each additional day of sleep deprivation, there is an estimated increase in reaction time of about 11.063 milliseconds.

#########################
###5. Residual Analysis##
#########################

#The residual analysis aimed to check how well the model fit the data and also the assumption that must be fulfilled by the model

#computing the residual values from the model
res <- residuals(model_sleepstudy)

#A Plot of the residuals to check if the model fit
plot(residuals(model_sleepstudy), main = "Residuals of Model")

#Checking for normality of the residuals to observe if our model has not violated the assumption
par(mfrow = c(1, 2))
hist(res)
qqnorm(res)
qqline(res)

#A histogram of the residuals to visualize the normality, more boldly.
data.frame(residuals = resid(model_sleepstudy)) %>% ggplot(aes(x = residuals)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")

#checking for homoscedasticity, if there is constant variance of the residuals 
plot(fitted(model_sleepstudy), res)
