#--------------------------ctree1---------------------------------------
library(rpart)
swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_rpart)
text(swiss_rpart)

install.packages("party")
library(party)
treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)
cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))

install.packages("tree")
library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)
cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

#------------------------ctree2------------------------------
# Conditional Inference Tree for Mileage
fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
# plot tree
plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")
text(fit2M, use.n=TRUE, all=TRUE, cex=.8)

#------------------------ctree3------------------------------
fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosis")
plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")






