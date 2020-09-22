rm(list = ls())
setwd('/Users/zihan/Desktop/rpi/Data\ Analytics')
multivariate <- read.csv('multivariate.csv')
attach(multivariate)
names(multivariate)
multivariate
plot(Income,Immigrant,main = "Scatterplot")
plot(Immigrant,Homeowners)

mm <- lm(Homeowners~Immigrant)
mm
plot(Immigrant,Homeowners)
abline(mm)
abline(mm,col=5,lwd=3)
summary(mm)
attributes(mm)
mm$coefficients

