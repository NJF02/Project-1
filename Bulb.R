library(dplyr)
library(ggplot2)
library(car)
library(lme4)

# Change to your own directory

setwd("C:/Users/User/Desktop/Project 1")

# Import Data and Data Cleaning

data = read.csv("Bulb.csv")
data$Bulb = factor(data$Bulb)
data$Machine = factor(data$Machine)
data$Station = factor(data$Station)

# Calculate the average of Watts for each Bulb

WattsbyMachine <- data %>%
  group_by(Bulb, Machine) %>%
  summarise(Watts = mean(Watts))

WattsbyStation <- data %>%
  group_by(Machine, Station) %>%
  summarise(Watts = mean(Watts))

# Exploratory Analysis

Station1 = filter(data, Station == '1')
Station2 = filter(data, Station == '2')
Station3 = filter(data, Station == '3')
Station4 = filter(data, Station == '4')

# Histograms

hist1 = ggplot(Station1, aes(Watts)) + 
  geom_histogram(binwidth = 0.1, color="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")
hist2 = ggplot(Station2, aes(Watts)) + 
  geom_histogram(binwidth = 0.1, color="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")
hist3 = ggplot(Station3, aes(Watts)) + 
  geom_histogram(binwidth = 0.1, color="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")
hist4 = ggplot(Station4, aes(Watts)) + 
  geom_histogram(binwidth = 0.1, color="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")

hist1
hist2
hist3
hist4

WattsbyMachine %>% ggplot(aes(x = Machine, y = Watts)) + geom_boxplot()
data %>% ggplot(aes(x = Machine, y = Watts)) + geom_boxplot()

data %>% ggplot(aes(x = Station, y = Watts)) + geom_boxplot()
WattsbyStation %>% ggplot(aes(x = Station, y = Watts)) + geom_boxplot()
anova(WattsbyStation)

# Box Plots

box0 = ggplot(data, aes(Station, Watts, colour = Station)) + geom_boxplot()
box0

box1 = ggplot(Station1, aes(Machine, Watts, colour = Machine)) + geom_boxplot()
box2 = ggplot(Station2, aes(Machine, Watts, colour = Machine)) + geom_boxplot()
box3 = ggplot(Station3, aes(Machine, Watts, colour = Machine)) + geom_boxplot()
box4 = ggplot(Station4, aes(Machine, Watts, colour = Machine)) + geom_boxplot()

box1
box2
box3
box4

# Pre Analysis

shapiro.test(Station1$Watts)
shapiro.test(Station2$Watts)
shapiro.test(Station3$Watts)
shapiro.test(Station4$Watts)

# Model Fitting

fit1 = lm(Watts ~ Machine, data = Station1)
fit2 = lm(Watts ~ Machine, data = Station2)
fit3 = lm(Watts ~ Machine, data = Station3)
fit4 = lm(Watts ~ Machine, data = Station4)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

fit5 = lm(Watts ~ Station, data = data)
summary(fit5)

aov.Station = aov(Watts ~ Station, data = data)
summary(aov.Station)
TukeyHSD(aov.Station)
plot(TukeyHSD(aov.Station))


WattsbyMachine$Machine = relevel(WattsbyMachine$Machine, ref = "D")


fit6 = lm(Watts ~ Machine, data = WattsbyMachine)
fit7 = lm(Watts ~ Station, data = WattsbyStation)
summary(fit6)
summary(fit7)


# Mixed Effects Model
Watts.null.Machine = lmer(Watts ~ (1 | Station), data = data)
Watts.Machine = lmer(Watts ~ Machine + (1 | Station), data = data)
Watts.null.Station = lmer(Watts ~ (1 | Machine), data = data)
Watts.Station = lmer(Watts ~ Station + (1 | Machine), data = data)

coef(Watts.Machine)

anova(Watts.nullM, Watts.Machine)
anova(Watts.nullS, Watts.Station)


data$Machine = relevel(data$Machine, ref = "D")

Watts.nullMPlus = lmer(Watts ~ (1 + Machine | Station), data = data)
Watts.MachinePlus = lmer(Watts ~ Machine + (1 + Machine | Station), data = data)

coef(Watts.MachinePlus)
anova(Watts.nullMPlus, Watts.MachinePlus)

# Model Diagnostic

influencePlot(fit5)

