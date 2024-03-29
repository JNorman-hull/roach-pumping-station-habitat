#RStudio 2023.03.1+446 "Cherry Blossom" Release
#R version 4.2.0
#Ex-situ experimentation to determine if introduced artificial habitat can provide alternative refuge to hazardous anthropogenic structures: success with habitat management

#Required libraries####

library(tidyverse)
library(funModeling)
library(dlookr)
library(ggeffects)
library(DHARMa)
library(ggpubr)
library(cowplot)
library(glmmTMB)
library(marginaleffects)

#1 Load data ####

# roach_wide.csv - wide data set where habitat counts are stored in columns c_hab, c_open, c_ps, c_both 
roach_wide=read_csv("./data/roach_wide.csv")

##1.1 Data discovery####

#Understanding dataset structure

nrow(roach_wide)
#6942 rows
ncol(roach_wide)
#24 columns
colnames(roach_wide)
#fish counts of interest stored in columns c_hab, c_ps, c_open, c_both. c_open further counted by c_open_cent, c_open_screen, c_open_hab depending on position in open water
glimpse(roach_wide)
#data frame status
roach_wide_status<- df_status(roach_wide, print_results = F)

#Convert necessary variables to factors and add level labels before proceeding
##1.2  Factors and labels ####
# Create a lookup table for variable labels
labels_table <- list(
  treatment = c('Covered (B)','Uncovered (A)'),
  light = c('Day', 'Night'),
  sequence = c('Baseline', 'I 1', 'I 2', 'I 3')
)

#NOTE: There is an error in the underlying data frame. Treatment 1 = covered occurs first, but should ideally start with 0 = uncovered. 
# Light 0 = Day should naturally be 0 = night and 1 = day.
# Labeling has been setup accordingly and does not need further modification

# Convert variables to factors with specific labeling using a loop
# Use the lookup table to get the labels for each variable
for (var in names(labels_table)) {
  levels <- unique(roach_wide[[var]])
  labels <- labels_table[[var]]
  roach_wide[[var]] <- factor(roach_wide[[var]], levels = levels, labels = labels)
}

# Convert other variables to factors without applying labels
other_vars <- c("hours_havail", "hours_lout", "day","trial")
roach_wide[other_vars] <- lapply(roach_wide[other_vars], as.factor)

##1.3 NA, 0, outliers, normality ####

#check NAs - 3516 present in hours_havail, 2334 in hours_lout
#Variables not considered in main analysis, NAs can be omitted later if required

#examine spread of zero counts in habitat variables
roach_wide_status %>%
  filter(variable %in% c("c_hab", "c_ps", "c_open", "c_open_cent", "c_open_screen", "c_open_hab")) %>%
  arrange(-p_zeros) %>%
  select(variable, q_zeros, p_zeros)
#high proportion of 0's in all counts, but will be confounded without consideration for light period and habitat availability

#outliers
plot_outlier(roach_wide)
#outliers not present

#normality
roach_wide%>% plot_normality(c_hab,c_ps,c_open)
#Response variables are not-normal, expected for count data. Transformation will not achieve normality.

##1.4 Summarise ####

#create minimal data set
roach_wide_sum = select(roach_wide, c_hab, c_ps, c_open, sequence, light, treatment)

#basic summary function
summary(roach_wide_sum)

#Expanded summary including SD, skew and kurtosis
profiling_num(roach_wide_sum)
#SD of c_ps and c_hab is relatively high. Suggests data points are spread across wide range of points.
#skewness >0 = right, <0 = left, 0 = symmetric.
#kurtosis >3 = sharp, <3 = flat, 3 = normal


#Sum of counts in each habitat by light period and sequence
bind_rows(
  roach_wide_sum %>%
    group_by(light) %>%
    summarise(sum_c_hab = sum(c_hab),
              sum_c_ps = sum(c_ps),
              sum_c_open = sum(c_open)) %>%
    rename(variable = light),
  roach_wide_sum %>%
    group_by(sequence) %>%
    summarise(sum_c_hab = sum(c_hab),
              sum_c_ps = sum(c_ps),
              sum_c_open = sum(c_open)) %>%
    rename(variable = sequence),
  roach_wide_sum %>%
    summarise(sum_c_hab = sum(c_hab),
              sum_c_ps = sum(c_ps),
              sum_c_open = sum(c_open),
              variable = "Total") %>%
    mutate(variable = factor(variable)))

#mean of habitat occupancy in each habitat by light period and sequence
roach_wide_sum %>%
  group_by(light) %>%
  summarise(med_c_hab = median(c_hab),
            med_c_ps = median(c_ps),
            med_c_open = median(c_open)) %>%
  mutate(variable = "Light") %>%
  bind_rows(roach_wide_sum %>% group_by(sequence) %>%
              summarise(med_c_hab = median(c_hab),
                        med_c_ps = median(c_ps),
                        med_c_open = median(c_open)) %>%
              mutate(variable = "Sequence"))

#Quick count stat summary

count_sum <- roach_wide_sum %>% 
  group_by(light, sequence) %>%
  summarise(n=n(),
            med_c_hab = median(c_hab),
            IQR_c_hab = IQR(c_hab),
            med_c_ps = median(c_ps),
            IQR_c_ps = IQR(c_ps),
            med_c_open = median(c_open),
            IQR_c_open = IQR(c_open),
            .groups = 'drop')

count_sum2 <- roach_wide_sum %>% 
  group_by(light, treatment, sequence) %>%
  summarise(n=n(),
            med_c_hab = median(c_hab),
            IQR_c_hab = IQR(c_hab),
            med_c_ps = median(c_ps),
            IQR_c_ps = IQR(c_ps),
            med_c_open = median(c_open),
            IQR_c_open = IQR(c_open),
            .groups = 'drop')

#2 Visualise main relationships ####

#Raw count data
ggplot(roach_wide_sum, aes(x = sequence, y= c_ps)) +
  geom_boxplot()

ggplot(roach_wide_sum, aes(x = sequence, y= c_hab)) +
  geom_boxplot()

ggplot(roach_wide_sum, aes(x = sequence, y= c_open)) +
  geom_boxplot()

#large variation in counts presents large IQR + outliers. Descriptive data e.g., medians hard to interperate

#Visualise rescaled habitat occupancy data

#Artificial habitat,light
ggplot(roach_wide_sum %>% filter(sequence!="Baseline")%>%
         group_by(light) %>%
         summarise(mean_c_hab = mean(c_hab_normalized),
                   se_c_hab = sd(c_hab_normalized) / sqrt(n())),
       aes(x = light, y = mean_c_hab)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#pumping station, light
ggplot(roach_wide_sum %>% filter(sequence!="I 2")%>%
         group_by(light) %>%
         summarise(mean_c_hab = mean(c_ps_normalized),
                   se_c_hab = sd(c_ps_normalized) / sqrt(n())),
       aes(x = light, y = mean_c_hab)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#open water, slight
ggplot(roach_wide_sum %>%
         group_by(light) %>%
         summarise(mean_c_hab = mean(c_open_normalized),
                   se_c_hab = sd(c_open_normalized) / sqrt(n())),
       aes(x = light, y = mean_c_hab)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#Habitat occupancy highest during the day. Open water occupancy highest during the night.
####

#Artificial habitat, sequence, light
ggplot(roach_wide_sum %>% filter(sequence!="Baseline")%>%
         group_by(sequence, light) %>%
         summarise(mean_c_hab = mean(c_hab_normalized),
                   se_c_hab = sd(c_hab_normalized) / sqrt(n())),
       aes(x = sequence, y = mean_c_hab, colour = light)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#pumping station, sequence, light
ggplot(roach_wide_sum %>% filter(sequence!="I 2")%>%
         group_by(sequence, light) %>%
         summarise(mean_c_hab = mean(c_ps_normalized),
                   se_c_hab = sd(c_ps_normalized) / sqrt(n())),
       aes(x = sequence, y = mean_c_hab, colour = light)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#open water, sequence, light
ggplot(roach_wide_sum %>%
         group_by(sequence, light) %>%
         summarise(mean_c_hab = mean(c_open_normalized),
                   se_c_hab = sd(c_open_normalized) / sqrt(n())),
       aes(x = sequence, y = mean_c_hab, colour = light)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#Day/night relationship is relatively fixed throughout the experiment with no deviations of great concern
#Drop in both day/night occupancy in open water counts, sequence I 1. 
#Possibly attributed to introducing AH
#This should be considered a an interaction term for any modelling
####

#Artificial habitat, sequence, treatment
ggplot(roach_wide_sum %>% filter(sequence!="Baseline")%>%
         group_by(sequence, treatment) %>%
         summarise(mean_c_hab = mean(c_hab_normalized),
                   se_c_hab = sd(c_hab_normalized) / sqrt(n())),
       aes(x = sequence, y = mean_c_hab, colour = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#pumping station, sequence, treatment
ggplot(roach_wide_sum %>% filter(sequence!="I 2")%>%
         group_by(sequence, treatment) %>%
         summarise(mean_c_hab = mean(c_ps_normalized),
                   se_c_hab = sd(c_ps_normalized) / sqrt(n())),
       aes(x = sequence, y = mean_c_hab, colour = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#open water, sequence, treatment
ggplot(roach_wide_sum %>%
         group_by(sequence, treatment) %>%
         summarise(mean_c_hab = mean(c_open_normalized),
                   se_c_hab = sd(c_open_normalized) / sqrt(n())),
       aes(x = sequence, y = mean_c_hab, colour = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#Treatment has an impact on habitat occupancy.
#In all sequences, habitat occupancy in AH highest un covered treatments.
#Interestingly, occupancy in PS shows opposite relationship to AH 

####

#Artificial habitat, treatment, light
ggplot(roach_wide_sum %>% filter(sequence!="Baseline")%>%
         group_by(treatment, light) %>%
         summarise(mean_c_hab = mean(c_hab_normalized),
                   se_c_hab = sd(c_hab_normalized) / sqrt(n())),
       aes(x = treatment, y = mean_c_hab, colour = light)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)
#daytime counts highest in covered treatments

#pumping station, treatment, light
ggplot(roach_wide_sum %>% filter(sequence!="I 2")%>%
         group_by(treatment, light) %>%
         summarise(mean_c_hab = mean(c_ps_normalized),
                   se_c_hab = sd(c_ps_normalized) / sqrt(n())),
       aes(x = treatment, y = mean_c_hab, colour = light)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#Daytime counts highest in uncovered treatment
#Suggests that cover is important i.e., when uncovered habitat is provided, more fish occupy the pumping station

#open water, treatment, light
ggplot(roach_wide_sum %>%
         group_by(treatment, light) %>%
         summarise(mean_c_hab = mean(c_open_normalized),
                   se_c_hab = sd(c_open_normalized) / sqrt(n())),
       aes(x = treatment, y = mean_c_hab, colour = light)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#no real relationship with treatment

#Artificial habitat, treatment, daytime
ggplot(roach_wide_sum %>% filter(sequence!="Baseline")%>% filter(light=="Day")%>%
         group_by(treatment, light) %>%
         summarise(mean_c_hab = mean(c_hab_normalized),
                   se_c_hab = sd(c_hab_normalized) / sqrt(n())),
       aes(x = treatment, y = mean_c_hab, colour = light)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#Artificial habitat, sequence, treatment (daytime)
ggplot(roach_wide_sum %>% filter(sequence!="Baseline")%>% filter(light=="Day")%>%
         group_by(sequence, treatment) %>%
         summarise(mean_c_hab = mean(c_hab_normalized),
                   se_c_hab = sd(c_hab_normalized) / sqrt(n())),
       aes(x = sequence, y = mean_c_hab, colour = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#Supports H5 - AH occupancy highest in covered treatments, regardless of sequence.
#Needs confirmation through modelling which accounts for random influence

####

#Artificial habitat, trial daytime)
ggplot(roach_wide %>% filter(sequence!="Baseline")%>% filter(light=="Day")%>%
         group_by(trial) %>%
         summarise(mean_c_hab = mean(c_hab_normalized),
                   se_c_hab = sd(c_hab_normalized) / sqrt(n())),
       aes(x = trial, y = mean_c_hab)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)
#between-trial variation suggests significant effect of within-subject observations (i.e., repeated measures)
#Potential temporal influence. Must be accounted for in analysis.

ggplot(roach_wide %>% filter(sequence!="Baseline")%>%
         group_by(time) %>%
         summarise(mean_c_hab = mean(c_hab_normalized),
                   se_c_hab = sd(c_hab_normalized) / sqrt(n())),
       aes(x = time, y = mean_c_hab)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#Some evidence of hour-to-hour variation. Incorporate into analysis

ggplot(roach_wide %>% filter(sequence!="Baseline")%>%
         group_by(seq_day) %>%
         summarise(mean_c_hab = mean(c_hab_normalized),
                   se_c_hab = sd(c_hab_normalized) / sqrt(n())),
       aes(x = seq_day, y = mean_c_hab)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

#Some evidence for temporal correlation but captured by variation in sequence

##2.1 Extra considerations ####

#When AH is introduced, does time available correlate with occupancy?

ggplot(roach_wide %>%
         filter(!is.na(hours_havail))%>% filter(sequence =="I 1")%>%
         group_by(hours_havail) %>%
         summarise(mean_c_hab = mean(c_hab_normalized),
                   se_c_hab = sd(c_hab_normalized) / sqrt(n())),
       aes(x = hours_havail, y = mean_c_hab)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

roach_wide %>%
  filter(!is.na(hours_havail)) %>%
  filter(sequence == "I 1") %>%
  mutate(hours = as.numeric(hours_havail)) %>%
  with(cor.test(hours, c_hab_normalized))

#No real relationship. Occupancy peaks 24h after introduction, as expected due to nocturnal behaviour.
#Weak positive correlation, but significant. cor = 0.11 p = <0.001

#When lights turn out, how long until open water occupancy peaks?

ggplot(roach_wide %>%
         filter(!is.na(hours_lout))%>%
         group_by(hours_lout) %>%
         summarise(mean_c_hab = mean(c_open_normalized),
                   se_c_hab = sd(c_open_normalized) / sqrt(n())),
       aes(x = hours_lout, y = mean_c_hab)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_c_hab - se_c_hab, ymax = mean_c_hab + se_c_hab),
                width = 0.2)

roach_wide %>%
  filter(!is.na(hours_lout)) %>%
  mutate(hours = as.numeric(hours_lout)) %>%
  with(cor.test(hours, c_open_normalized))

#No real relationship. Occupancy peaks within 1h of lights out.
#Weak positive correlation due to hour 0 - 1. cor = 0.13 p = <0.001.

#3 GLMM ####
##3.1 Data preparation ####

#Extract hour from time variable and store as factor for random effect modelling
roach_wide <- roach_wide %>%
  mutate(time_factor = factor(as.numeric(format(strptime(time, format = "%H:%M:%S"), "%H"))))

##3.2 Data exploration ####

#The data exploration showed
#Count data representing 0 - 12.
#U-shaped, Beta distribution poor fit.
#-Binomial distribution. Apply logit-link to account for uneven variance in sequence groups.
#-Raw counts preferred than rescaling.
# No NAs
# No outliers
# Non-normally distributed response variable, but large sample size
# Apply generalised model with relaxed assumptions
# Zeros in response but are real zeros. No zero-inflation considered.
# No continuous predictors, so no collinearity issues
# Interaction between sequence and light
# No Independence of response variables - Repeated measures design requires random effect of trial
# Temporal dependency should be accounted for with random effects

table(roach_wide$sequence) #equal data
table(roach_wide$treatment) #equal data
table(roach_wide$light) #unequal, but naturally
#Data is well balanced in grouping variables

##3.3 Build ####

#Create successes - failures structure for binomial model
#Adds extra columns with total possible count - actual count (failures)
roach_wide <- roach_wide %>%
  mutate(
    c_hab_f = 12 - c_hab,
    c_ps_f = 12 - c_ps,
    c_open_f = 12 - c_open)

###3.3.1 AH ####

#Null model
mod1 <- glmmTMB(cbind(c_hab,c_hab_f) ~ 1, data = roach_wide%>%filter(sequence!="Baseline"),
                family = binomial(link="logit"))
summary(mod1)
#AIC  57481.3

#Add fixed effects
#Now including an interaction between sequence & light after visualizing relationship and recommendation from bio-statistician 
mod1.1 <- update(mod1, . ~ sequence*light + treatment)
summary(mod1.1)
#AIC 41048.6
#Plot fit
plot(ggpredict(mod1.1, terms = c("sequence","light")))
#Model predictions good, close to observed.

#Improve by adding repeated measures and temporal dependency 
#Also add tank to account for between set-up differences
mod1.2 <- update(mod1.1, . ~ . + (1 | trial/day/time_factor)+(1|tank))
summary(mod1.2)
#AIC 17913.3
#Model predictions improved. Supported by AIC and loglik

#Plot fit
plot(ggpredict(mod1.2, terms = c("sequence","light")))+
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1), expand = c(0, 0), 
                     labels = scales::percent(seq(0, 1, 0.1), scale = 100))
#ggpredict fixes treatment. Try alternative gggaverage for overall effect.
plot(predict_response(mod1.2, terms = c("sequence","light"), margin="empirical", ci_level = 0.99))+
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1), expand = c(0, 0), 
                     labels = scales::percent(seq(0, 1, 0.1), scale = 100))

#margin=mean_mode = ggpredict. Conditional effect of the predictor on a typical grouping variable (e.g., selecting one level of treatment)
#margin=empirical = ggaverage. Marginal effect of the predictor across all levels of a grouping variable (e.g., both treatments)

#Improved fit and error distribution

#Plot simulated residuals to check fit of model
fittedmod1.2 <- mod1.2
simuout1 <- simulateResiduals(fittedModel = fittedmod1.2)
plot(simuout1, quantreg = T)

residualsmod1.2 <- residuals(mod1.2)
qqnorm(residualsmod1.2)
qqline(residualsmod1.2)
hist(residualsmod1.2, breaks = "FD", col = "lightblue")

#Observed vs expected show good relationship. Overdispersion present but unlikely to be treated in binomial model.
#quantile deviations likely a result of dispersion. Expected due to values close to 0 and 1.


#ANOVA to compare model with sequence vs without
#effectively a likelihood-ratio test
anova(mod1.2, update(mod1.2, . ~ . - sequence - sequence:light))

#report chsq, df, p
# “(GLMM: Δ log-likelihood = 849, df = 4, p < 0.001)”.


###3.3.2 PS ####

#Null model
mod2 <- glmmTMB(cbind(c_ps,c_ps_f) ~ 1, data = roach_wide%>%filter(sequence!="I 2"),
                family = binomial(link="logit"))
summary(mod2)
#AIC  76209.6 

#Add fixed effects
#Now including an interaction between sequence & light after visualizing relationship and recommendation from bio-statistician 
mod2.1 <- update(mod2, . ~ sequence*light + treatment)
summary(mod2.1)
#AIC 53654.1 
#Plot fit
plot(ggpredict(mod2.1, terms = c("sequence","light")))
#Model predictions good, close to observed.

#Improve by adding repeated measures and temporal dependency 
#Also add tank to account for between set-up differences
mod2.2 <- update(mod2.1, . ~ . + (1 | trial/day/time_factor)+(1|tank))
summary(mod2.2)
#AIC 9782.8 
#Plot fit
plot(ggpredict(mod2.2, terms = c("sequence","light")))+
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1), expand = c(0, 0), 
                     labels = scales::percent(seq(0, 1, 0.1), scale = 100))
#ggpredict fixes treatment. Try alternative gggaverage for overall effect.
plot(predict_response(mod2.2, terms = c("sequence","light"), margin="empirical", ci_level = 0.99))+
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1), expand = c(0, 0), 
                     labels = scales::percent(seq(0, 1, 0.1), scale = 100))
#Model predictions improved. Supported by AIC and loglik

#Plot simulated residuals to check fit of model
fittedmod2.2 <- mod2.2
simuout1 <- simulateResiduals(fittedModel = fittedmod2.2)
plot(simuout1, quantreg = T)

residualsmod2.2 <- residuals(mod2.2)
qqnorm(residualsmod2.2)
qqline(residualsmod2.2)
hist(residualsmod2.2, breaks = "FD", col = "lightblue")
#Observed vs expected show good relationship. Overdispersion not present.
#quantile deviations likely a result of dispersion. Expected due to values close to 0 and 1.

#ANOVA to compare model with sequence vs without
#effectively a likelihood-ratio test
anova(mod2.2, update(mod2.2, . ~ . - sequence - sequence:light))

#report chsq, df, p
# “(GLMM: Δ log-likelihood = 720, df = 4, p < 0.001)”.


###3.3.3 OW ####

#Null
mod3 <- glmmTMB(cbind(c_open,c_open_f) ~ 1, data = roach_wide,
                family = binomial(link="logit"))
summary(mod3)
#AIC  70469.0  

#Add fixed effects
mod3.1 <- update(mod3, . ~ sequence*light + treatment)
summary(mod3.1)
#AIC 54648.2  
#Plot fit
plot(ggpredict(mod3.1, terms = c("sequence","light")))
#Model predictions good, close to observed.

#Improve by adding repeated measures and temporal dependency 
mod3.2 <- update(mod3.1, . ~ . + (1 | trial/day/time_factor) +(1|tank))
summary(mod3.2)
#AIC 19764.5   
#Plot fit
plot(ggpredict(mod3.2, terms = c("sequence","light")))+
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1), expand = c(0, 0), 
                     labels = scales::percent(seq(0, 1, 0.1), scale = 100))
#ggpredict fixes treatment. Try alternative gggaverage for overall effect.
plot(ggaverage(mod3.2, terms = c("sequence","light"), margin ="empirical",ci_level = 0.99))+
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1), expand = c(0, 0), 
                     labels = scales::percent(seq(0, 1, 0.1), scale = 100))
#Model predictions improved. Supported by AIC and loglik


#Plot simulated residuals to check fit of model
fittedmod3.2 <- mod3.2
simuout1 <- simulateResiduals(fittedModel = fittedmod3.2)
plot(simuout1, quantreg = T)

residualsmod3.2 <- residuals(mod3.2)
qqnorm(residualsmod3.2)
qqline(residualsmod3.2)
hist(residualsmod3.2, breaks = "FD", col = "lightblue")
#Observed vs expected show good relationship.
#quantile deviations likely a result of dispersion. Expected due to values close to 0 and 1.

#ANOVA to compare model with sequence vs without
#effectively a likelihood-ratio test
anova(mod3.2, update(mod3.2, . ~ . - sequence - sequence:light))

#report chsq, df, p
# “(GLMM: Δ log-likelihood = 595, df = 6, p < 0.001)”.


##3.4 Plot GLMM####

###3.4.1 Theme####
theme_JN <- function(base_size=10){ 
  theme_grey() %+replace%
    theme(
      axis.text = element_text(colour="black"),
      axis.title = element_text(colour="black"),
      axis.ticks = element_line(colour="black"),
      panel.border = element_rect(colour = "black", fill=NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      strip.background = element_rect(colour = "black",fill = NA),
      panel.spacing.x = unit(12, "pt")
    ) 
}
###3.4.2 Data preparation####

#Save model predictions

c_hab_glmmm <-ggaverage(mod1.2, terms = c("sequence", "light"), ci_level = 0.99)
c_ps_glmmm <-ggaverage(mod2.2, terms = c("sequence", "light"), ci_level = 0.99)
c_open_glmmm <-ggaverage(mod3.2, terms = c("sequence", "light"), ci_level = 0.99)
c_hab_glmmm_treat <-ggaverage(mod1.2, terms = c("sequence", "treatment", "light[Day]") ,ci_level = 0.99)


#add grouping variable for habitat, bind dataframes together 
# Add the 'habitat' column to each data frame
c_hab_glmmm <- c_hab_glmmm %>% mutate(habitat = "Artificial habitat", present = "T")
c_ps_glmmm <- c_ps_glmmm %>% mutate(habitat = "Pumping station", present = "T")
c_open_glmmm <- c_open_glmmm %>% mutate(habitat = "Open water", present = "T")

# Combine the data frames into one dataframe named 'modeloutput_df'
# remove NAs
# Remove attributes and clean DF
# set variables as factors
modeloutput_df <- bind_rows(c_hab_glmmm, c_ps_glmmm, c_open_glmmm)
modeloutput_df <- na.omit(modeloutput_df)
modeloutput_df <- as.data.frame(unclass(modeloutput_df))
modeloutput_df <- modeloutput_df %>%
  add_row(x = 'Baseline', group = 'Day', habitat = 'Artificial habitat', present = "F") %>%
  add_row(x = 'Baseline', group = 'Night', habitat = 'Artificial habitat', present = "F") %>%
  add_row(x = 'I 2', group = 'Day', habitat = 'Pumping station', present = "F") %>%
  add_row(x = 'I 2', group = 'Night', habitat = 'Pumping station', present = "F")
modeloutput_df$x <- factor(modeloutput_df$x, levels = c('Baseline', 'I 1', 'I 2', 'I 3'))
modeloutput_df$habitat <- as.factor(modeloutput_df$habitat)
modeloutput_df$habitat <- factor(modeloutput_df$habitat, levels = c('Pumping station', 'Open water', 'Artificial habitat'))

c_hab_glmmm_treat <- as.data.frame(unclass(c_hab_glmmm_treat))
c_hab_glmmm_treat <- c_hab_glmmm_treat %>% mutate(present = "T")
c_hab_glmmm_treat <- c_hab_glmmm_treat %>% select(-facet)
c_hab_glmmm_treat <- c_hab_glmmm_treat %>%
  add_row(x = 'Baseline', group = 'Covered (B)', present = "F") %>%
  add_row(x = 'Baseline', group = 'Uncovered (A)', present = "F")
c_hab_glmmm_treat$x <- factor(c_hab_glmmm_treat$x, levels = c('Baseline', 'I 1', 'I 2', 'I 3'))
c_hab_glmmm_treat$group <- factor(c_hab_glmmm_treat$group, levels = c('Uncovered (A)', 'Covered (B)'))

###3.4.3 Main plot ####

model_occupancy_dn<- ggplot(data=modeloutput_df, aes(x=x, y=predicted, fill=group))+
  geom_tile(aes(x=x, y=0.5,height = Inf, fill=present), alpha = 0.3,  show.legend = FALSE) + 
  scale_fill_manual(values = c("T" = "white", "F" = "grey90"), guide = FALSE) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), size=0.2,width = 0.6 ,position = position_dodge(width = 0.7),  show.legend = FALSE) +
  geom_path(aes(group = interaction(habitat, group), linetype = group), linewidth = 0.3 ,position = position_dodge(width = 0.7), show.legend = FALSE) +
  scale_linetype_manual(values = c("Day" = "dashed", "Night" = "dotted"))+
  geom_point(aes(shape=group),size=1, position = position_dodge(width = 0.7),  show.legend = FALSE) +
  scale_shape_manual(values = c("Day" = 20, "Night" = 4))+
  scale_y_continuous(breaks = seq(0, 1,0.1), limits=c(0,1),expand=c(0.05,0),
                     labels = scales::percent(seq(0, 1, 0.1), scale = 100)) +
  scale_x_discrete(expand=c(0,0))+
  labs(x = 'Experimental sequence',y= 'Predicted probability of habitat occupancy')+
  theme_JN()+
  theme(axis.text.x=element_text(size=8),
        panel.spacing.x =unit(0, "lines") ) + 
  facet_grid(~ habitat, scales = "fixed")+
  geom_text(data = modeloutput_df %>% filter(present == "F"), aes(x = x, y = 0.5, label = "Unavailable"), size = 8/.pt, angle = 90, fontface = "italic")
model_occupancy_dn

ggsave(filename="./figures/model_occupancy_dn.svg", plot=model_occupancy_dn,device = "svg",units="cm",width=16,height=8)

###3.4.4 Treatment plot ####

model_treatment <- ggplot(data=c_hab_glmmm_treat, aes(x=x, y=predicted, fill=group))+
  geom_tile(aes(x=x, y=0.5,height = Inf, fill=present), alpha = 0.3,  show.legend = FALSE) + 
  scale_fill_manual(values = c("T" = "white", "F" = "grey90"), guide = FALSE) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), size=0.2,width = 0.6 ,position = position_dodge(width = 0.7),  show.legend = FALSE) +
  geom_point(aes(shape=group),size=1, position = position_dodge(width = 0.7),  show.legend = FALSE) +
  scale_shape_manual(values = c("Covered (B)" = 20, "Uncovered (A)" = 4))+
  scale_y_continuous(breaks = seq(0, 1,0.1), limits=c(0,1),expand=c(0.05,0),
                     labels = scales::percent(seq(0, 1, 0.1), scale = 100)) +
  scale_x_discrete(expand=c(0,0))+
  labs(x = 'Experimental sequence',y=expression(atop(NA, atop(textstyle('Predicted probabaility of'), textstyle('daytime artificial habitat occupancy')))))+
  coord_cartesian(clip="off")+
  theme_JN()+
  theme(axis.text.x=element_text(size=8),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+ 
  geom_text(data = c_hab_glmmm_treat %>% filter(present == "F"), aes(x = x, y = 0.5, label = "Unavailable"), size = 8/.pt, angle = 90, fontface = "italic")
model_treatment

ggsave(filename="./figures/model_treatment.svg", plot=model_treatment,device = "svg",units="cm",width=10,height=8)

###3.4.4 Pre/post exclusion plot ####

hab_prob_plot <- ggplot(data=modeloutput_df%>%filter(x=="I 1"|x=="I 3")%>%
                          filter(group=="Day")%>%
                          filter(habitat=="Pumping station"|habitat=="Artificial habitat"),
                          aes(x = factor(x, labels = c("Pre-exclusion", "Post-exclusion")), y=predicted, fill=group))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), size=0.2,width=.2) +
  geom_line(aes(group=habitat,linetype = habitat), linewidth = 0.3 , show.legend = FALSE)+
  geom_point(aes(shape=habitat),size=1,  show.legend = FALSE) +
  scale_shape_manual(values = c("Artificial habitat" = 20, "Pumping station" = 4))+
  scale_linetype_manual(values = c("Artificial habitat" = "dashed", "Pumping station" = "dotted"))+
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1), expand = c(0.05, 0), 
                     labels = scales::percent(seq(0, 1, 0.1), scale = 100)) +
  scale_x_discrete()+
  labs(x = 'Pumping station exclusion',y=expression(atop(NA, atop(textstyle('Predicted probabaility of'), textstyle('daytime habitat occupancy')))))+
  coord_cartesian(clip="off")+
  theme_JN()+
  theme(axis.text.x=element_text(size=8))
hab_prob_plot

ggsave(filename="./figures/hab_prob_plot2.svg", plot=hab_prob_plot,device = "svg",units="cm",width=10,height=8)

#Combine plots using cowplot

combined_mod <-plot_grid(hab_prob_plot, model_treatment,
                         ncol = 2, nrow = 1, rel_widths = c(5.6,4.4),align = "h")
combined_mod
ggsave(filename="./figures/hab_exclusion_treatment.svg", plot=combined_mod, device = "svg",units="cm", width=16,height=8)

#4 Post-hoc sensitivity analysis ####

#sensitivity analysis to confirm model results
#Used repeated measures ANOVA and paired t tests to account for grouped variables
#Effect of experimental sequence on habitat occupancy

#AH
anov1<- aov(c_hab ~ sequence + Error(trial), data = roach_wide%>%filter(sequence!="Baseline"))
summary(anov1)
#AH
anov2<- aov(c_ps~ sequence + Error(trial), data = roach_wide%>%filter(sequence!="I 2"))
summary(anov2)
#OW
anov3<- aov(c_open ~ sequence + Error(trial), data = roach_wide)
summary(anov3)

#Day night differences in standardized habitat occupancy data
#Include standard error
roach_wide %>%
  group_by(light) %>%
  summarize(mean_c_hab = mean(c_hab),
            se_c_hab = sd(c_hab) / sqrt(n()),
            mean_c_ps = mean(c_ps),
            se_c_ps = sd(c_ps) / sqrt(n()),
            mean_c_open = mean(c_open),
            se_c_open = sd(c_open) / sqrt(n()))

#Check level lengths and use min length for comparisons
table(roach_wide %>%
        filter(light == "Day") %>%
        .$sequence)

#Create new DF for paired comparison between baseline and intervention 1 pumping station occupancy

i1_base_ps<-  roach_wide %>%
  filter(light == "Day" & sequence %in% c("Baseline", "I 1")) %>%
  group_by(sequence) %>%
  mutate(row_num = row_number()) %>%
  filter(row_num <= 630) %>%
  ungroup() %>%
  select(-row_num)

t.test(c_ps~ sequence,data=i1_base_ps, paired = TRUE)

#Create new DF for paired comparison between intervention 1 artificial habitat/pumping station occupancy
#Remove rows from sequence category to allow for paired comparison
#Convert to long and add dummy variable for leveling

i1_hab_ps_long <- roach_wide %>%
  filter(light == "Day" & sequence %in% c("I 1")) %>%
  slice_head(n = 630) %>%
  pivot_longer(
    cols = c("c_hab", "c_ps"),
    names_to = "hab_count",
    values_to = "count"
  ) %>%
  mutate(hab_count = recode(hab_count, c_hab = "c_hab1", c_ps = "c_ps1"))

t.test(count~ hab_count,data=i1_hab_ps_long, paired = TRUE)

#Create new DF for paired comparison between intervention 1 and intervention 2 artificial habitat occupancy
#Remove rows from sequence category to allow for paired comparison


i1_i2_hab<- roach_wide %>%
  filter(light == "Day" & sequence %in% c("I 1", "I 2")) %>%
  group_by(sequence) %>%
  mutate(row_num = row_number()) %>%
  filter(row_num <= 630) %>%
  ungroup() %>%
  select(-row_num)

t.test(c_hab~ sequence,data=i1_i2_hab, paired = TRUE)

#Create new DF for paired comparison between intervention 1 and intervention 3 pumping station occupancy
#Remove rows from sequence category to allow for paired comparison. Match 630 rows of I 1

i1_i3_ps <- roach_wide %>%
  filter(sequence %in% c("I 1", "I 3") & light == "Day") %>%
  group_by(sequence) %>%
  filter(!(sequence == "I 1" & row_number() > 630)) %>%
  ungroup()

t.test(c_ps ~ sequence,data=i1_i3_ps, paired = TRUE)

#Create new DF for paired comparison between intervention 2 and intervention 3 artificial habitat occupancy
#Remove rows from sequence category to allow for paired comparison. Match 630 rows of I 2

i2_i3_hab<- roach_wide %>%
  filter(sequence %in% c("I 2", "I 3") & light == "Day") %>%
  group_by(sequence) %>%
  filter(!(sequence == "I 2" & row_number() > 630)) %>%
  ungroup()

t.test(c_hab~ sequence,data=i2_i3_hab, paired = TRUE)

#Create new DF for paired comparison between intervention 1 and intervention 3 artificial habitat occupancy
#Remove rows from sequence category to allow for paired comparison. Match 630 rows of I 2

i1_i3_hab<- roach_wide %>%
  filter(sequence %in% c("I 1", "I 3") & light == "Day") %>%
  group_by(sequence) %>%
  filter(!(sequence == "I 1" & row_number() > 630)) %>%
  ungroup()

t.test(c_hab~ sequence,data=i1_i3_hab, paired = TRUE)

#Create new DF for paired comparison between treatments AH
#Remove rows from sequence category to allow for paired comparison. Match 630 rows of I 2

treatment_hab<- roach_wide %>%
  filter(sequence %in% c("I 1","I 2", "I 3") & light == "Day") %>%
  group_by(sequence) %>%
  filter(!(sequence %in% c("I 1", "I 2") & row_number() > 630)) %>%
  ungroup()

t.test(c_hab_normalized ~ treatment,data=treatment_hab, paired = TRUE)

#Create new DF for paired comparison between treatments PS
#Remove rows from sequence category to allow for paired comparison. Match 630 rows of I 2

treatment_ps<- roach_wide %>%
  filter(sequence %in% c("Baseline", "I 1", "I 3") & light == "Day") %>%
  group_by(sequence) %>%
  filter(!(sequence %in% c("Baseline", "I 1") & row_number() > 630)) %>%
  ungroup()
table(treatment_ps$sequence)

t.test(c_ps_normalized ~ treatment,data=treatment_ps, paired = TRUE) 
