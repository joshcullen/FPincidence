###########################################################
## Preliminary Analysis of Turtle Capture Data & FP Data ##
###########################################################

### directory fetch
# getwd()
# setwd("/Users/aidanperez/Documents/FP_incidence_CR/Scripts")

# library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(readr)
library(gridExtra)
library(tidyverse)
library(lubridate)
library(janitor)
library(reshape)

### import data
capture_data <- read.csv("Data/2021-10_Cm_capture_data.csv")
fp_scores <- read.csv("Data/2021-10_Cm_FP_Scores.csv")

### explore data
head(capture_data)  #probably only need to print the first few rows
summary(capture_data)
str(capture_data)

head(fp_scores)
summary(fp_scores)
str(fp_scores)
table(capture_data$Fibropapilloma.Visible, useNA = "ifany")  #counts each response, including NAs
str(capture_data$Fibropapilloma.Visible)

# Make sure all "No"s are consistent in spelling; removes any extra space
capture_data$Fibropapilloma.Visible<- gsub(pattern = " *",  #detect space(s)
                                           replacement = "", #replace w/ nothing
                                           x = capture_data$Fibropapilloma.Visible)
table(capture_data$Fibropapilloma.Visible, useNA = "ifany")  #confirm that issue is fixed

green_w_fp <- filter(capture_data, Fibropapilloma.Visible == "Yes")
green_wo_fp <- filter(capture_data, Fibropapilloma.Visible == "No")
#72 of 112 listed with FP visible / 64.28% ; 7 turtles listed as "NA"

summary(green_w_fp$FP.Balazs.Score)
table(green_w_fp$Year)






##################
#### viz data ####
##################

###fp and balaz frequency amongst sampled turtles

hist(capture_data$FP.Balazs.Score)

ggplot(capture_data, aes(x = FP.Balazs.Score)) +
    geom_histogram(binwidth = 1, colour = "grey", fill = "#008080") +
    geom_vline(aes(xintercept = mean(FP.Balazs.Score)),
               colour = "black", linetype = "dashed", size=1) +
    theme_bw() +
    ylab("Frequency\n") +
    xlab("\nFibropapilloma Balazs Score")  +
    theme(axis.text = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),
          panel.grid = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm"))


filtered_fp_tumor_score <- as.numeric(capture_data$FP.Tumor.Score)
filtered_fp_tumor_score <- filter(!is.na(FP.Tumor.Score))
hist(filtered_fp_tumor_score)

ggplot(capture_data, aes(x=filtered_fp_tumor_score))+
    geom_histogram(binwidth = 1, colour = "grey", fill = "#008080")+
    geom_vline(aes(xintercept = mean (filtered_fp_tumor_score)),
               colour = "black", linetype = "dashed", size=1) +
    theme_bw() +
    ylab("Frequency\n")+
    xlab("\nFibropapilloma Tumor Score") +
    theme(axis.text = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),
          panel.grid = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm"))

###fp visibiltiy by season

plot

###fp scores by seasons
summary(green_w_fp$Season)
summary(green_w_fp$FP.Tumor.Score)
fp_tumor_score <- as.numeric(green_w_fp$FP.Tumor.Score)
seasons <- as.integer(green_w_fp$Season)

#fp score distribution by seasons
# 1 - Spring
# 2 - Summer
# 3 - Fall
# 4 - Winter

plot(seasons, fp_tumor_score)
plot(seasons, green_w_fp$FP.Balazs.Score)

###fp scores by year

plot(green_w_fp$Year, fp_tumor_score)
plot(green_w_fp$Year, green_w_fp$FP.Balazs.Score)

###frequency of FP presence by year and season

fp_visibility <- capture_data %>%
  group_by(Year,Fibropapilloma.Visible) %>%
    summarise(fp_visibility = length((Fibropapilloma.Visible)))

# make this a stacked bar graph, with one internal bar being "yes" and one bar being "yes and no"
# have this displayed for each year


fp_year_summ<- capture_data %>%
  group_by(Year, Fibropapilloma.Visible) %>%
  tally() %>%
  drop_na() %>%
  group_by(Year) %>%
  mutate(total.N = sum(n),
         prop = n / total.N)


# Plotting counts
ggplot(fp_year_summ, aes(Year, n, fill = Fibropapilloma.Visible)) +
  geom_col() +
  scale_fill_brewer("FP Visible", palette = 'Dark2') +
  theme_bw()

# Plotting proportions
ggplot(fp_year_summ, aes(Year, prop, fill = Fibropapilloma.Visible)) +
  geom_col() +
  scale_fill_brewer("FP Visible", palette = 'Dark2') +
  theme_bw()
