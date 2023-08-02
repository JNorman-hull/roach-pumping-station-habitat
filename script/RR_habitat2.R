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

#Add fixed effects
mod1.1 <- update(mod1, c_hab_normalized ~ sequence + light + treatment)
summary(mod1.1)
#Plot fit
plot(ggpredict(mod1.1, terms = c("sequence","light")))

#Model predictions good, close to observed.
#Improve by adding repeated measures and temporal dependency 

#Add random effects
mod1.2 <- update(mod1.1, . ~ . + (1 | time_factor/day/trial))
summary(mod1.2)
#Plot fit
plot(ggpredict(mod1.2, terms = c("sequence","light")))

#Model predictions improved. Supported by AIC and loglik

#Plot simulated residuals to check fit of model
fittedmod1.2 <- mod1.2
simuout1 <- simulateResiduals(fittedModel = fittedmod1.2)
plot(simuout1, quantreg = T)

#Generally follows a linear relationship
#quantile deviations likely a result of dispersion. Expected due to values close to 0 and 1.


###3.3.2 PS ####

#Null model
mod2 <- glmmTMB(c_ps_normalized ~ 1, data = roach_wide%>%filter(sequence!="I 2"),
                family = gaussian(link="log"))
summary(mod2)

#Add fixed effects
mod2.1 <- update(mod2, c_ps_normalized ~ sequence + light + treatment)
summary(mod2.1)
#Plot fit
plot(ggpredict(mod2.1, terms = c("sequence","light")))

#Model predictions good, close to observed.
#Improve by adding repeated measures and temporal dependency 

#Add random effects
mod2.2 <- update(mod2.1, . ~ . + (1 | time_factor/day/trial))
summary(mod2.2)
#Plot fit
plot(ggpredict(mod2.2, terms = c("sequence","light")))

#Model predictions improved. Supported by AIC and loglik

#Plot simulated residuals to check fit of model
fittedmod2.2 <- mod2.2
simuout2 <- simulateResiduals(fittedModel = fittedmod2.2)
plot(simuout2, quantreg = T)

#Generally follows a linear relationship
#quantile deviations likely a result of dispersion. Expected due to values close to 0 and 1.

###3.3.3 OW ####

#Null model
mod3 <- glmmTMB(c_open_normalized ~ 1, data = roach_wide,
                family = gaussian(link="log"))
summary(mod3)

#Add fixed effects
mod3.1 <- update(mod3, c_open_normalized ~ sequence + light + treatment)
summary(mod3.1)
#Plot fit
plot(ggpredict(mod3.1, terms = c("sequence","light")))

#Model predictions good, close to observed.
#Improve by adding repeated measures and temporal dependency 

#Add random effects
mod3.2 <- update(mod3.1, . ~ . + (1 | time_factor/day/trial))
summary(mod3.2)
#Plot fit
plot(ggpredict(mod3.2, terms = c("sequence","light")))

#Model predictions improved. Supported by AIC and loglik

#Plot simulated residuals to check fit of model
fittedmod3.2 <- mod3.2
simuout3 <- simulateResiduals(fittedModel = fittedmod3.2)
plot(simuout3, quantreg = T)

#Deviates from linear distribution. Confounded by daytime variation

###3.3.4 Binary ####

mod_binary_ah <- glmmTMB(c_hab_normalized ~ as.factor(binary) + (1 | time_factor/day/trial), data = roach_binary, family = binomial())

plot(ggpredict(mod_binary_ah, terms = c("binary")))
summary(mod_binary_ah)

mod_binary_ps <- glmmTMB(c_ps_normalized ~ as.factor(binary) + (1 | time_factor/day/trial), data = roach_binary, family = binomial())

plot(ggpredict(mod_binary_ps, terms = c("binary")))
summary(mod_binary_ps)

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

c_hab_glmmm <-ggpredict(mod1.2, terms = c("sequence", "light"))
c_ps_glmmm <-ggpredict(mod2.2, terms = c("sequence", "light"))
c_open_glmmm <-ggpredict(mod3.2, terms = c("sequence", "light"))
c_hab_glmmm_treat <-ggpredict(mod1.2, terms = c("sequence", "treatment", "light[Day]"))

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

#binary dataframe
ah_prob <- ggpredict(mod_binary_ah, terms = c("binary"))
ps_prob <- ggpredict(mod_binary_ps, terms = c("binary"))

ps_prob$group <- 2 
ps_prob$group <- factor(ps_prob$group)

habitat_prob_df <- bind_rows(ah_prob, ps_prob)


###3.4.3 Main plot ####

model_occupancy_dn<- ggplot(data=modeloutput_df, aes(x=x, y=predicted, fill=group))+
  geom_tile(aes(x=x, y=0.5,height = Inf, fill=present), alpha = 0.3,  show.legend = FALSE) + 
  scale_fill_manual(values = c("T" = "grey80", "F" = "grey90"), guide = FALSE) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), size=0.5,width = 0.6 ,position = position_dodge(width = 0.7),  show.legend = FALSE) +
  #geom_path(aes(group = interaction(habitat, group), linetype = group), linewidth = 0.3 ,position = position_dodge(width = 0.7), show.legend = FALSE) +
  scale_linetype_manual(values = c("Day" = "dashed", "Night" = "dotted"))+
  geom_point(aes(shape=group),size=2, position = position_dodge(width = 0.7),  show.legend = FALSE) +
  scale_shape_manual(values = c("Day" = 20, "Night" = 4))+
  scale_y_continuous(breaks = seq(0, 1,0.1), limits=c(0,1),expand=c(0.05,0)) +
  scale_x_discrete(expand=c(0,0))+
  labs(x = 'Experimental sequence',y= 'Habitat occupancy')+
  theme_JN()+
  theme(axis.text.x=element_text(size=8),
        strip.background = element_rect(fill = "grey90"),
        panel.spacing.x =unit(0, "lines") ) + 
  facet_grid(~ habitat, scales = "fixed")+
  geom_text(data = modeloutput_df %>% filter(present == "F"), aes(x = x, y = 0.5, label = "Unavailable"), size = 8/.pt, angle = 90, fontface = "italic")
model_occupancy_dn

ggsave(filename="model_occupancy_dn.svg", plot=model_occupancy_dn,device = "svg",units="cm",width=16,height=8)

###3.4.4 Treatment plot ####

model_treatment <- ggplot(data=c_hab_glmmm_treat, aes(x=x, y=predicted, fill=group))+
  geom_tile(aes(x=x, y=0.5,height = Inf, fill=present), alpha = 0.3,  show.legend = FALSE) + 
  scale_fill_manual(values = c("T" = NA, "F" = "grey90"), guide = FALSE) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), size=0.5,width = 0.6 ,position = position_dodge(width = 0.7),  show.legend = FALSE) +
  geom_point(aes(shape=group),size=1.5, position = position_dodge(width = 0.7),  show.legend = FALSE) +
  scale_shape_manual(values = c("Covered (B)" = 20, "Uncovered (A)" = 4))+
  scale_y_continuous(breaks = seq(0, 1,0.1), limits=c(0,1),expand=c(0.05,0)) +
  scale_x_discrete(expand=c(0,0))+
  labs(x = 'Experimental sequence',y= 'Daytime artifical habitat occupancy')+
  coord_cartesian(clip="off")+
  theme_JN()+
  theme(axis.text.x=element_text(size=8)) + 
  geom_text(data = c_hab_glmmm_treat %>% filter(present == "F"), aes(x = x, y = 0.5, label = "Unavailable"), size = 8/.pt, angle = 90, fontface = "italic")
model_treatment

ggsave(filename="model_treatment.svg", plot=model_treatment,device = "svg",units="cm",width=8,height=8)

###3.4.4 Probability plot ####

hab_prob_plot <- ggplot(data=habitat_prob_df, aes(x = factor(x, labels = c("Pre-exclusion", "Post-exclusion")), y=predicted, fill=group))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width=.2) +
  geom_line(aes(group=group,linetype = group),  show.legend = FALSE)+
  geom_point(aes(shape=group),size=2,  show.legend = FALSE) +
  scale_shape_manual(values = c("1" = 20, "2" = 4))+
  scale_linetype_manual(values = c("1" = "dashed", "2" = "dotted"))+
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1), expand = c(0.05, 0), 
                     labels = scales::percent(seq(0, 1, 0.1), scale = 100)) +
  scale_x_discrete()+
  labs(x = 'Pumping station exclusion',y=expression(atop(NA, atop(textstyle('Predicted probabaility of'), textstyle('daytime habitat occupancy')))))+
  coord_cartesian(clip="off")+
  theme_JN()+
  theme(axis.text.x=element_text(size=8))
hab_prob_plot


ggsave(filename="hab_prob_plot2.svg", plot=hab_prob_plot,device = "svg",units="cm",width=10,height=8)


