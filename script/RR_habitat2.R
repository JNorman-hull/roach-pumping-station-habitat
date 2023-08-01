#RStudio 2023.03.1+446 "Cherry Blossom" Release
#R version 4.2.0
#Ex-situ experimentation to determine if introduced artificial habitat can provide safe alternative to shelter in hazardous anthropogenic structures: if you build it, they might come

#Required libraries####

library(tidyverse)
#library(dunn.test)
#library(car)
library(ggeffects)
library(DHARMa)
#library(rstatix)
library(ggpubr)
library(ggplot2)
library(glmmTMB)
#library(forcats)
#library(cowplot)

#1 Load data and prepare ####

# roach_wide.csv - wide data set where habitat counts are stored in columns c_hab, c_open, c_ps, c_both 
roach_wide=read_csv("./data/roach_wide.csv")
# roach_long - long data set where habitat counts are stacked (counted) and grouped by column hab 1 = PS, 2 = OW, 3 = AH
# hab_avail 1 = ps, 2 = AH, 3 = both
roach_long=read_csv("./data/roach_long.csv")

##1.1 Set up labels and factors ####
# Create a lookup table for variable labels
labels_table <- list(
  treatment = c('Covered (B)','Uncovered (A)'),
  ps_tank_end = c('RH', 'LH'),
  room_end = c('Far', 'Near'),
  light = c('Day', 'Night'),
  hab_avail = c('AH unavailable', 'AH available'),
  ps_avail = c('PS available','PS unavailable'),
  both_avail = c('Single Available', 'Both Available'),
  sequence = c('Baseline', 'I 1', 'I 2', 'I 3')
)

# Convert variables to factors with specific labeling using a loop
# Use the lookup table to get the labels for each variable
for (var in names(labels_table)) {
  levels <- unique(roach_wide[[var]])
  labels <- labels_table[[var]]
  roach_wide[[var]] <- factor(roach_wide[[var]], levels = levels, labels = labels)
}

# Convert other variables to factors without applying labels
other_vars <- c("hours_havail", "hours_lout", "run", "day", "trial")
roach_wide[other_vars] <- lapply(roach_wide[other_vars], as.factor)

#3 GLMM ####
##3.1 Data preparation ####

#Extract hour from time variable and store as factor for random effect modelling
roach_wide <- roach_wide %>%
  mutate(time_factor = factor(as.numeric(format(strptime(time, format = "%H:%M:%S"), "%H"))))

#standardise raw count data to scale of 0 - 1
#across function looks at each habitat count variable, divides them by the max value, returns new variable
#new variables are then adjusted to add 0.00000001 to any 0 entry
roach_wide <- roach_wide %>%
  mutate(across(c(c_hab, c_ps, c_open), ~ . / max(.), .names = "{.col}_normalized"),
         across(ends_with("_normalized"), ~ ifelse(. == 0, . + 0.0000000001, .)))

#Create new DF for binary model
roach_binary <- roach_wide %>%filter (sequence=="I 1"|sequence=="I 3")%>%filter(light=="Day")
roach_binary$binary <- ifelse(roach_binary$sequence =="I 1", 0,1)


##3.2 Data exploration ####

##3.3 Build ####
###3.3.1 AH ####

#Null model
mod1 <- glmmTMB(c_hab_normalized ~ 1, data = roach_wide%>%filter(sequence!="Baseline"),
                family = gaussian(link="log"))
summary(mod1)

#Add all fixed effects
mod1.1 <- glmmTMB(c_hab_normalized ~  sequence + light + treatment + ps_tank_end + room_end,
                  data = roach_wide%>%filter(sequence!="Baseline"),
                  family = gaussian(link="log"))
summary(mod1.1)


#Remove unwanted fixed effects
mod1.2 <- glmmTMB(c_hab_normalized ~ sequence + light + treatment,
                  data = roach_wide%>%filter(sequence!="Baseline"),
                  family = gaussian(link="log"))
summary(mod1.2)


#Add random effects
mod1.3 <- glmmTMB(c_hab_normalized ~ sequence + light + treatment +  (1 | time_factor/day/trial),
                  data = roach_wide%>%filter(sequence!="Baseline"),
                  family = gaussian(link="log"))
summary(mod1.3)

###3.3.2 PS ####

#Null model
mod2 <- glmmTMB(c_ps_normalized ~ 1, data = roach_wide%>%filter(sequence!="I 2"),
                family = gaussian(link="log"))
summary(mod2)

#Add all fixed effects
mod2.1 <- glmmTMB(c_ps_normalized ~  sequence + light + treatment + ps_tank_end + room_end,
                  data = roach_wide%>%filter(sequence!="I 2"),
                  family = gaussian(link="log"))
summary(mod2.1)


#Remove unwanted fixed effects
mod2.2 <- glmmTMB(c_ps_normalized ~ sequence + light + treatment,
                  data = roach_wide%>%filter(sequence!="I 2"),
                  family = gaussian(link="log"))
summary(mod2.2)


#Add random effects
mod2.3 <- glmmTMB(c_ps_normalized ~ sequence + light + treatment +  (1 | time_factor/day/trial),
                  data = roach_wide%>%filter(sequence!="I 2"),
                  family = gaussian(link="log"))
summary(mod2.3)


###3.3.3 OW ####

#Null model
mod3 <- glmmTMB(c_open_normalized ~ 1, data = roach_wide,
                family = gaussian(link="log"))
summary(mod3)

#Add all fixed effects
mod3.1 <- glmmTMB(c_open_normalized ~  sequence + light + treatment + ps_tank_end + room_end,
                  data = roach_wide,
                  family = gaussian(link="log"))
summary(mod3.1)


#Remove unwanted fixed effects
mod3.2 <- glmmTMB(c_open_normalized ~ sequence + light + treatment,
                  data = roach_wide,
                  family = gaussian(link="log"))
summary(mod3.2)


#Add random effects
mod3.3 <- glmmTMB(c_open_normalized ~ sequence + light + treatment +  (1 | time_factor/day/trial),
                  data = roach_wide,
                  family = gaussian(link="log"))
summary(mod3.3)
