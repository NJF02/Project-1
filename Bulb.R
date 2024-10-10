library(dplyr)
library(ggplot2)

# Change to your own directory

setwd("C:/Users/User/Desktop/Project 1")

# Import Data and Data Cleaning

data = read.csv("Bulb.csv")
data$Bulb = factor(data$Bulb)
data$Machine = factor(data$Machine)
data$Station = factor(data$Station)


# Exploratory Analysis

Station1 = filter(data, Station == '1')
Station2 = filter(data, Station == '2')
Station3 = filter(data, Station == '3')
Station4 = filter(data, Station == '4')

# Histograms

hist1 = ggplot(Station1, aes(Watts))
hist2 = ggplot(Station2, aes(Watts))
hist3 = ggplot(Station3, aes(Watts))
hist4 = ggplot(Station4, aes(Watts))

hist1 + geom_histogram(binwidth = 0.1, color="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666")

# Box Plot

box = ggplot(data, aes(Station, Watts, colour = Station)) + geom_boxplot()
box


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

