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



