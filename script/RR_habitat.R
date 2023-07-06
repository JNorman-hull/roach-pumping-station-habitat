#RStudio 2023.03.1+446 "Cherry Blossom" Release
#R version 4.2.0
#Ex-situ experimentation to determine if introduced artificial habitat can provide safe alternative to shelter in hazardous anthropogenic structures: if you build it, they might come

#Required libraries

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

#### 1 Load data and prepare ####

# roach_wide.csv - wide data set where habitat counts are stored in columns c_hab, c_open, c_ps, c_both 
roach_wide=read_csv("./data/roach_wide.csv")
# roach_long - long data set where habitat counts are stacked (counted) and grouped by column hab 1 = PS, 2 = OW, 3 = AH
# hab_avail 1 = ps, 2 = AH, 3 = both
roach_long=read_csv("./data/roach_long.csv")

#### 1.1 Set up labels and factors ####

roach_wide$treatment <- factor(roach_wide$treatment, labels = c("Uncovered (A)","Covered (B)"))
roach_wide$ps_tank_end <- factor(roach_wide$ps_tank_end, labels = c("RH","LH"))
roach_wide$room_end <- factor(roach_wide$room_end, labels = c("Far","Near"))
roach_wide$light <- factor(roach_wide$light, labels = c("Day","Night"))
roach_wide$hab_avail <- factor(roach_wide$hab_avail, labels = c("AH unavailable","AH available"))
roach_wide$ps_avail <- factor(roach_wide$ps_avail, labels = c("PS unavailable","PS available"))
roach_wide$both_avail <- factor(roach_wide$both_avail, labels = c("Single available","Both available"))
roach_wide$hours_havail <- as.factor(roach_wide$hours_havail)
roach_wide$hours_lout <- as.factor(roach_wide$hours_lout)
roach_wide$sequence <- factor(roach_wide$sequence, labels = c("Baseline","I 1", "I 2", "I 3"))
roach_wide$run <- as.factor(roach_wide$run)
roach_wide$day <- as.factor(roach_wide$day)
roach_wide$trial <- as.factor(roach_wide$trial)

roach_long$treatment <- factor(roach_long$treatment, labels = c("Unsheltered (A)","Sheltered (B)"))
roach_long$ps_tank_end <- factor(roach_long$ps_tank_end, labels = c("RH","LH"))
roach_long$room_end <- factor(roach_long$room_end, labels = c("Far","Near"))
roach_long$light <- factor(roach_long$light, labels = c("Day","Night"))
roach_long$sequence <- factor(roach_long$sequence, labels = c("Baseline","I 1", "I 2", "I 3"))
roach_long$run <- as.factor(roach_long$run)
roach_long$day <- as.factor(roach_long$day)
roach_long$trial <- as.factor(roach_long$trial)
roach_long$hab <- factor(roach_long$hab, labels = c("PS","OW","AH"))
roach_long$hab_avail <- factor(roach_long$hab_avail, labels = c("PS_a","AH_a","Both_a"))
roach_long$ow_loc <- factor(roach_long$hab, labels = c("Reeds","OW center","PS screen"))
roach_long$counted_yn<-factor(roach_long$counted_yn, labels = c("No","Yes"))

#### 1.2 Create new DF with pseudo-observations to replace 0's where habitat unavailable or unused ####

# Set a specific seed value for reproducibility
set.seed(123)  

roach_wide_po <- roach_wide %>%
  mutate(c_hab = ifelse(sequence == "Baseline" & c_hab == 0,
                        ifelse(row_number() %in% sample(which(sequence == "Baseline" & c_hab == 0), 177),
                               1,
                               c_hab),
                        c_hab)) %>%
  mutate(c_ps = ifelse(sequence == "I 2" & c_ps == 0,
                       ifelse(row_number() %in% sample(which(sequence == "I 2" & c_ps == 0), 177),
                              1,
                              c_ps),
                       c_ps))

roach_long_po <- roach_long %>%
  mutate(count = ifelse((sequence == "Baseline" & hab == "AH" | sequence == "I 2" & hab == "PS") & count == 0,
                        ifelse(row_number() %in% sample(which((sequence == "Baseline" & hab == "AH" | sequence == "I 2" & hab == "PS") & count == 0), 354),
                               1,
                               count),
                        count))

#Appears glmmTMB cannot handle multinomial responses

#First consider original approach of splitting data

model1 <- glmmTMB(c_hab ~ sequence + light + treatment + (1 | run/trial) + ar1(day+0|trial), data = roach_wide_po, family=nbinom1)

summary(model1)

plot(ggpredict(model1, terms = c("sequence")))
plot(ggpredict(model1, terms = c("sequence","treatment")))

