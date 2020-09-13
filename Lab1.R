EPI2020 <- read.csv('2010EPI_data 1.csv')
View(EPI2020)
attach(EPI2020)
fix(EPI2020)
EPI
tf <- is.na(EPI)
E <- EPI[!tf] 
summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.)) 
lines(density(EPI,na.rm=TRUE,bw="SJ"))
rug(EPI)

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

EPILand<-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)

EPIDense<-EPI[!High_Population_Density]
EDense <- EPIDense[!is.na(EPIDense)]
hist(EDense)
hist(EDense, seq(30., 95., 1.0), prob=TRUE)

EPI_South_Asia<-EPI[EPI_regions == "South Asia"]
EPI_SouthAsia <- EPI_South_Asia[!is.na(EPI_South_Asia)]
hist(EPI_SouthAsia)
hist(EPI_SouthAsia, seq(30., 95., 1.0), prob=TRUE)
