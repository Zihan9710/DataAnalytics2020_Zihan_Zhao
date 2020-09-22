rm(list = ls())
setwd('/Users/zihan/Desktop/rpi/Data\ Analytics')
EPI2020 <- read.csv('2010EPI_data 1.csv')
attach(EPI2020)

names(EPI2020)
EPI2020

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI); qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

plot(ecdf(DALY), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(DALY); qqline(DALY)

plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(WATER_H); qqline(WATER_H)

boxplot(EPI,DALY)

'''inter-compare:
EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_E,WATER_E, BIODIVERSITY''' 

boxplot(EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_E, WATER_E, BIODIVERSITY)

mm <- lm(EPI~ENVHEALTH+ECOSYSTEM+DALY+AIR_H+WATER_H+AIR_E+WATER_E+BIODIVERSITY)
summary(mm)
plot(mm)
mm$coefficients

