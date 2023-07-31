#RStudio 2023.03.1+446 "Cherry Blossom" Release
#R version 4.2.0
#Ex-situ experimentation to determine if introduced artificial habitat can provide safe alternative to shelter in hazardous anthropogenic structures: if you build it, they might come



#save this script and write a new clean one with simplified modle process





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
library(lmtest)

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

###Alternative using IfElse####
# 
# convert_vars <- c("treatment", "ps_tank_end", "room_end", "light", "hab_avail",
#                   "ps_avail", "both_avail", "sequence")
# 
# # Convert variables to factors with specific labeling using a loop
# for (var in convert_vars) {
#   levels <- unique(roach_wide[[var]])
#   labels <- levels
#   if (var == "treatment") {
#     labels <- c("Uncovered (A)", "Covered (B)")
#   } else if (var == "ps_tank_end") {
#     labels <- c("RH", "LH")
#   } else if (var == "room_end") {
#     labels <- c("Far", "Near")
#   } else if (var == "light") {
#     labels <- c("Day", "Night")
#   } else if (var == "hab_avail") {
#     labels <- c("AH unavailable", "AH available")
#   } else if (var == "ps_avail") {
#     labels <- c("PS unavailable", "PS available")
#   } else if (var == "both_avail") {
#     labels <- c("Single Available", "Both Available")
#   } else if (var == "sequence") {
#     labels <- c("Baseline", "I 1", "I 2", "I 3")
#   }
#   
#   roach_wide[[var]] <- factor(roach_wide[[var]], levels = levels, labels = labels)
# }



#1.2 Create new DF with pseudo-observations to replace 10% of 0's where habitat unavailable or unused ####

# Set a specific seed value for reproducibility
set.seed(123)  

roach_wide_po <- roach_wide %>%
  mutate(c_hab = replace(c_hab, sequence == "Baseline" & c_hab == 0 & row_number() %in% sample(which(sequence == "Baseline" & c_hab == 0), round(0.05 * n())), 1)) %>%
  mutate(c_ps = replace(c_ps, sequence == "I 2" & c_ps == 0 & row_number() %in% sample(which(sequence == "I 2" & c_ps == 0), round(0.05 * n())), 1))

# roach_long_po <- roach_long %>%
#   mutate(count = ifelse((sequence == "Baseline" & hab == "AH" | sequence == "I 2" & hab == "PS") & count == 0,
#                         ifelse(row_number() %in% sample(which((sequence == "Baseline" & hab == "AH" | sequence == "I 2" & hab == "PS") & count == 0), 354),
#                                1,
#                                count), count))
#need to update this to use replace instead of ifelse


#2 GLMM ####

#Model 1 - predict c_hab####
#trial day cannot be used as a fixed effect as this assumes independence between observations
#can only be considered as a random effect to account for temporal dependence
#baseline sequence should be removed as fish can never be in AH here
#therefore, pseudo observations would not be required

model1 <- glmmTMB(c_hab ~ sequence + light + treatment +  ps_tank_end + (1 | run/trial), data = roach_wide_po, family=nbinom1)

summary(model1)
#AIC 27205

plot(ggpredict(model1, terms = c("sequence")))
plot(ggpredict(model1, terms = c("sequence","light")))

#Model predictions follow same pattern as observed, count predictions only slightly lower
#Update model to consider nbinom2 distribution (e.g., add shape consideration)

model1.1 <- update(model1, family = nbinom2())

# Print the updated model summary
summary(model1.1)
plot(ggpredict(model1.1, terms = c("sequence","light")))
#AIC 28339
#change in AIC indicates worst fit? > perform LRT

lrtest(model1, model1.1)

#Significant LRT suggests improved fit
#At this stage, it is unclear which provides a better fit as LRT suggests nbinom2, but AIC suggests nbinom1
#continue with nbinom1 and consider changing at end of selection process

#Drop ps_tank_end to determine influence

model1.2 <- update(model1, . ~ . - ps_tank_end)

summary(model1.2)
#AIC 27215
plot(ggpredict(model1.2, terms = c("sequence","light")))

lrtest(model1, model1.2)

#Suggests model without ps_tank_end is  better, possibly because tank effects are captured in random effect of trial/run?
#inflates errors in predictions slightly, does this matter?

##Currently the best model for predicting counts close to real values

#Next, consider adding effect of day to account for autoregression
#also need to consider zero inflation

model1.3 <- update(model1, . ~ . + (1|day))

summary(model1.3)
plot(ggpredict(model1.3, terms = c("sequence","light")))

#Poor fit for model. Do not use term 'day' as random factor for autoregression
#consider AR1 structure after next approach

#update model with sequential experimental days

model1.4 <- update(model1, . ~ . + (1|seq_day))

summary(model1.4)
#AIC 25233
#Significantly reduces AIC
plot(ggpredict(model1.4, terms = c("sequence","light")))
#Adds a lot of uncertainity to model predictions when attempting to account for experimental days
#Possibly confounded by large between-run differences
lrtest(model1, model1.4)

#AIC and lrt suggest an improved model. However, the fitted values are over fitted. The model is too complex for the available data and has poor predictions.
#Inclusion of seq_day and day both unsuitable for capturing temporal autocorrelation
#Also attempted with dropped ps_tank_end term, improves model but error still large
#Also attempted with dropped ps_tank_end term and nbinom2 family, error inflates with family change


#before progressing. Try using AR structure

roach_wide_po$seq_day <- as.factor(roach_wide_po$seq_day)

model1.5 <- update(model1, . ~ . + ar1(day+0|trial))
summary(model1.5)
#AIC 25233
#Significantly reduces AIC
plot(ggpredict(model1.5, terms = c("sequence","light")))
lrtest(model1, model1.5)


#Model 1 - Start over with omitting baseline sequence####

mod1 <- glmmTMB(c_hab ~ sequence + light + treatment + ps_tank_end + (1 | run/trial), data = roach_wide_po%>%filter(sequence!="Baseline"), family=nbinom1)
summary(mod1)
plot(ggpredict(mod1, terms = c("sequence","light")))

#AIC 24922
#Predicted values follow same relationship as observed but reduced count

#remove ps_tank_end as potentially conflicting for same information as run/trial

mod1.1 <- update(mod1, . ~ . - ps_tank_end)
summary(mod1.1)
plot(ggpredict(mod1.1, terms = c("sequence","light")))
#AIC 24932
#Predicted values closer to observed, but error range higher
lrtest(mod1, mod1.1)
#log likelihood higher on model without ps_tank_end

#Add regressive structure for temporal dependence

mod1.2 <- update(mod1.1, . ~ . + ar1(day+0|trial))
summary(mod1.2)
plot(ggpredict(mod1.2, terms = c("sequence","light")))
#AIC 22846
#Predicted values lower, error higher
lrtest(mod1.1, mod1.2)
#LRT suggests model without AR1 structure a better fit

#continue with mod1.1, consider zero-inflation

fittedmod1.1 <- mod1.1
simuout1 <- simulateResiduals(fittedModel = fittedmod1.1)
plot(simuout1)

testDispersion(simuout1) #underdispersed
testZeroInflation(simuout1) #zero inflation present

#first try switching to a poisson model

mod1.3 <- update(mod1, .~. - ps_tank_end, family=poisson())
summary(mod1.3)
#29167
plot(ggpredict(mod1.3, terms = c("sequence","light")))

lrtest(mod1, mod1.3)
#LRT suggests switching to poisson family and dropping tank end improves model

fittedmod1.3 <- mod1.3
simuout1 <- simulateResiduals(fittedModel = fittedmod1.3)
plot(simuout1)

testDispersion(simuout1) #dispersion issue from mod1 (nbinom + ps_tank_end) treated!
testZeroInflation(simuout1) #zero inflation present

#Add zero inflation component

mod1.4 <- update(mod1.3,ziformula=~1)
summary(mod1.4)
#24543
plot(ggpredict(mod1.4, terms = c("sequence","light")))

lrtest(mod1.3, mod1.4)
#LRT suggests model is not improved?

fittedmod1.4 <- mod1.4
simuout1 <- simulateResiduals(fittedModel = fittedmod1.4)
plot(simuout1)

testDispersion(simuout1) #dispersion issue treated
testZeroInflation(simuout1) #zero inflation treated

#try improve model prediction 

mod1.5 <- glmmTMB(c_hab ~ sequence+light + treatment + (1 | run/trial), ziformula=~sequence, data = roach_wide_po%>%filter(sequence!="Baseline"), family=poisson())
summary(mod1.5)
plot(ggpredict(mod1.5, terms = c("sequence","light")))

fittedmod1.5 <- mod1.5
simuout1 <- simulateResiduals(fittedModel = fittedmod1.5)
plot(simuout1)

testDispersion(simuout1) #dispersion issue treated
testZeroInflation(simuout1) #zero inflation treated

#Accepted model, although predictions for Intervention1 are suspiciously high.
#cannot add AR1 structure as this under disperses data




# Consider normalizing counts to a relative scale and modelling with Gaussian

max_count <- max(roach_wide_po$c_hab)
roach_wide_po <- roach_wide_po %>%
  mutate(c_hab_normalized = c_hab / max_count,
         c_hab_normalized = ifelse(c_hab_normalized == 0, c_hab_normalized + 0.0000000001, c_hab_normalized))

ggplot(roach_wide_po, aes(x = c_hab_normalized_arcsine)) +
  geom_histogram(binwidth = 0.05, color = "black", fill = "lightblue") +
  labs(title = "Histogram of c_hab_normalized_arcsine",
       x = "c_hab_normalized",
       y = "Frequency")
qqnorm(roach_wide_po$c_hab_normalized)
qqline(roach_wide_po$c_hab_normalized, col = "red")
shapiro.test(roach_wide_po$c_hab_normalized)



mod1.6 <- glmmTMB(c_hab_normalized ~ sequence + light + treatment +  (1 | trial), 
                  data = roach_wide_po%>%filter(sequence!="Baseline"), family=gaussian(link="log"))

summary(mod1.6)
plot(ggpredict(mod1.6, terms = c("sequence", "light")))
c_hab_glmmm <-ggpredict(mod1.6, terms = c("sequence", "light"))

fittedmod1.6 <- mod1.6
simuout1 <- simulateResiduals(fittedModel = fittedmod1.6)
plot(simuout1)
testDispersion(simuout1) #no dispersion issue
testZeroInflation(simuout1) # no zero inflation 

#No zero inflation
#residual vs predicted not ideal, but accepted as expect clumpiness
#Adding autoregresison underdisperses data 

##Accepted model
#Model with sequence, light, treatment, random effect of run/trial and gaussian (identity)


mod1.6.1 <- glmmTMB(c_hab_normalized ~ sequence + light + treatment + (1 | run/trial)+ ar1(seq_day+0|trial), data = roach_wide_po%>%filter(sequence!="Baseline"), family=gaussian(link="identity"))
summary(mod1.6.1)
plot(ggpredict(mod1.6.1, terms = c("sequence", "light")))

fittedmod1.6.1 <- mod1.6.1
simuout1 <- simulateResiduals(fittedModel = fittedmod1.6.1)
plot(simuout1, quantreg = T)

testDispersion(simuout1) 

##Adding autoregression term overfits the data and causes underdispersion

mod1.7 <- glmmTMB(count_normalized ~ sequence + light + treatment + ps_tank_end + (1 | run/trial), data = roach_wide_po%>%filter(sequence!="Baseline"), family=gaussian(link="log"))

summary(mod1.7)
plot(ggpredict(mod1.7, terms = c("sequence", "light")))

fittedmod1.7 <- mod1.7
simuout1 <- simulateResiduals(fittedModel = fittedmod1.7)
plot(simuout1)

testDispersion(simuout1) #dispersion issue treated

#Adding ps_tank_end reduces AIC, but introduces clumpiness in residuals vs predicted. stick to without
#Normalizing the counts to an occupancy scale eliminates zero inflation issue and provides prediction closer to observed





#Model 2 - predict c_ps####
#I2 should be removed as fish can never be in the PS here

max_count <- max(roach_wide_po$c_ps)
roach_wide_po <- roach_wide_po %>%
  mutate(c_ps_normalized = c_ps / max_count,
         c_ps_normalized = ifelse(c_ps_normalized == 0, c_ps_normalized + 0.0000000001, c_ps_normalized))


mod2 <- glmmTMB(c_ps_normalized ~ sequence + light + treatment  + (1 |trial), 
                data = roach_wide_po%>%filter(sequence!="I 2"), family=gaussian(link="log"))
summary(mod2)
plot(ggpredict(mod2, terms = c("sequence", "light")))
c_ps_glmmm <-ggpredict(mod2, terms = c("sequence", "light"))

fittedmod2 <- mod2
simuout1 <- simulateResiduals(fittedModel = fittedmod2)
plot(simuout1, quantreg = T)

testDispersion(simuout1) #no dispersion issue


#Model 3 - predict open water####
max_count <- max(roach_wide_po$c_open)
roach_wide_po <- roach_wide_po %>%
  mutate(c_open_normalized = c_open / max_count,
         c_open_normalized = ifelse(c_open_normalized == 0, c_open_normalized + 0.0000000001, c_open_normalized))

ggplot(roach_wide_po, aes(x = c_open_normalized)) +
  geom_histogram(binwidth = 0.05, color = "black", fill = "lightblue") +
  labs(title = "Histogram of c_open_normalized",
       x = "c_hab_normalized",
       y = "Frequency")


mod3 <- glmmTMB(c_open_normalized ~ sequence + light + treatment + (1 | trial), 
                data = roach_wide_po, family=gaussian(link="log"))
summary(mod3)

table1 <-ggpredict(mod3, terms = c("sequence","light"))
plot(ggpredict(mod3, terms = c("sequence","light")))
c_open_glmmm <-ggpredict(mod3, terms = c("sequence", "light"))

fittedmod2 <- mod3
simuout1 <- simulateResiduals(fittedModel = fittedmod2)
plot(simuout1, quantreg = T)

testDispersion(simuout1) #no dispersion issue


#Final model GLMM + hour####
roach_wide_po <- roach_wide_po %>%
  mutate(time_numeric = as.numeric(format(strptime(time, format = "%H:%M:%S"), "%H")),
         time_factor = factor(time_numeric))

mod1.8 <- glmmTMB(c_hab_normalized ~ sequence + light + treatment +  (1 | time_factor/day/trial),
                  data = roach_wide_po%>%filter(sequence!="Baseline"), family=gaussian(link="log"))

summary(mod1.8)
plot(ggpredict(mod1.8, terms = c("day")))
plot(ggpredict(mod1.8, terms = c("sequence[I 1, I 3]")))

roach_binary <- roach_wide_po %>%filter (sequence=="I 1"|sequence=="I 3")%>%filter(light=="Day")
roach_binary$binary <- ifelse(roach_binary$sequence =="I 1", 0,1)

binary_model <- glmmTMB(c_hab_normalized ~ as.factor(binary) + (1 | time_factor/day/trial), data = roach_binary, family = binomial())

plot(ggpredict(binary_model, terms = c("binary")))
hab_prob <- ggpredict(binary_model, terms = c("binary"))

summary(binary_model)

binary_model2 <- glmmTMB(c_ps_normalized ~ as.factor(binary) + (1 | time_factor/day/trial), data = roach_binary, family = binomial())

plot(ggpredict(binary_model2, terms = c("binary")))
hab_prob2 <- ggpredict(binary_model2, terms = c("binary"))

summary(binary_model2)




ggplot(roach_wide_po%>%filter(sequence!="Baseline")%>%filter(light=="Day"), aes(x = treatment, y = c_hab_normalized)) +
  geom_boxplot() 

fittedmod1.8 <- mod1.8
simuout1 <- simulateResiduals(fittedModel = fittedmod1.8)
plot(simuout1)
testDispersion(simuout1) #no dispersion issue
testZeroInflation(simuout1) # no zero inflation 

mod2.1 <- glmmTMB(c_ps_normalized ~ sequence + light + treatment  + (1 |time_factor/day/trial), 
                data = roach_wide_po%>%filter(sequence!="I 2"), family=gaussian(link="log"))
summary(mod2.1)
plot(ggpredict(mod2.1, terms = c("sequence", "light")))

fittedmod2 <- mod2.1
simuout1 <- simulateResiduals(fittedModel = fittedmod2)
plot(simuout1, quantreg = T)

testDispersion(simuout1) #no dispersion issue


mod3.1 <- glmmTMB(c_open_normalized ~ sequence + light + treatment + (1 | time_factor/day/trial), 
                data = roach_wide_po, family=gaussian(link="log"))
summary(mod3.1)

plot(ggpredict(mod3.1, terms = c("sequence","light")))

fittedmod2 <- mod3.1
simuout1 <- simulateResiduals(fittedModel = fittedmod2)
plot(simuout1, quantreg = T)

testDispersion(simuout1) #no dispersion issue


#Save model predictions

c_hab_glmmm <-ggpredict(mod1.8, terms = c("sequence", "light"))
c_ps_glmmm <-ggpredict(mod2.1, terms = c("sequence", "light"))
c_open_glmmm <-ggpredict(mod3.1, terms = c("sequence", "light"))
c_hab_glmmm_treat <-ggpredict(mod1.8, terms = c("sequence", "treatment", "light[Day]"))


#add grouping variable for habitat, bind dataframes together 
# Add the 'habitat' column to each data frame
c_hab_glmmm <- c_hab_glmmm %>% mutate(habitat = "Artificial habitat", present = "T")
c_ps_glmmm <- c_ps_glmmm %>% mutate(habitat = "Pumping station", present = "T")
c_open_glmmm <- c_open_glmmm %>% mutate(habitat = "Open water", present = "T")

hab_prob2$group <- 2 
hab_prob2$group <- factor(hab_prob2$group)

habitat_prob_df <- bind_rows(hab_prob, hab_prob2)

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

model_occupancy<- ggplot(data=modeloutput_df, aes(x=x, y=predicted))+
  geom_tile(aes(x=x, y=0.5,height = Inf, fill=present)) + 
  scale_fill_manual(values = c("T" = "grey80", "F" = "grey90"), guide = FALSE) +
  geom_point(shape=18) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  geom_path(aes(group = interaction(habitat, group)), linetype = "dotted", color = "black", linewidth = 0.5) +
  scale_y_continuous(breaks = seq(0, 1,0.1), limits=c(0,1),expand=c(0.05,0)) +
  scale_x_discrete(expand=c(0,0))+
  labs(x = 'Experimental sequence',y= 'Habitat occupancy')+
  theme_JN()+
  theme(axis.text.x=element_text(size=8),
        strip.background = element_rect(fill = "grey90")) + 
  facet_grid(group~ habitat, scales = "fixed")+
  geom_text(data = modeloutput_df %>% filter(present == "F"), aes(x = x, y = 0.5, label = "Unavailable"), size = 10/.pt, angle = 90, fontface = "italic")
model_occupancy


model_occupancy_dn<- ggplot(data=modeloutput_df, aes(x=x, y=predicted, fill=group))+
  geom_tile(aes(x=x, y=0.5,height = Inf, fill=present), alpha = 0.3,  show.legend = FALSE) + 
  scale_fill_manual(values = c("T" = "grey80", "F" = "grey90"), guide = FALSE) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), size=0.5,width = 0.6 ,position = position_dodge(width = 0.7),  show.legend = FALSE) +
  #geom_path(aes(group = interaction(habitat, group), linetype = group), linewidth = 0.3 ,position = position_dodge(width = 0.7), show.legend = FALSE) +
  scale_linetype_manual(values = c("Day" = "dashed", "Night" = "dotted"))+
  geom_point(aes(shape=group),size=1, position = position_dodge(width = 0.7),  show.legend = FALSE) +
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




fieldman tests






