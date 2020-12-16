#-------------------svm9------------------------
# test data
xtest <- rbind(matrix(rnorm(20),,2),matrix(rnorm(20,mean=3),,2))
# test kernel matrix i.e. inner/kernel product of test data with
# Support Vectors

Ktest <- as.kernelMatrix(crossprod(t(xtest),t(x[SVindex(svp2), ])))

predict(svp2, Ktest)


#### Use custom kernel 

k <- function(x,y) {(sum(x*y) +1)*exp(-0.001*sum((x-y)^2))}
class(k) <- "kernel"

data(promotergene)

## train svm using custom kernel
gene <- ksvm(Class~.,data=promotergene[c(1:20, 80:100),],kernel=k,
             C=5,cross=5)

gene


#### Use text with string kernels
data(reuters)
is(reuters)
tsv <- ksvm(reuters,rlabels,kernel="stringdot",
            kpar=list(length=5),cross=3,C=10)
tsv


## regression
# create data
x <- seq(-20,20,0.1)
y <- sin(x)/x + rnorm(401,sd=0.03)

# train support vector machine
regm <- ksvm(x,y,epsilon=0.01,kpar=list(sigma=16),cross=3)
plot(x,y,type="l")
lines(x,predict(regm,x),col="red")

#-------------------svm10------------------------
#### Use custom kernel 

k <- function(x,y) {(sum(x*y) +1)*exp(-0.001*sum((x-y)^2))}
class(k) <- "kernel"

data(promotergene)

## train svm using custom kernel
gene <- ksvm(Class~.,data=promotergene[c(1:20, 80:100),],kernel=k,
             C=5,cross=5)

gene
#-------------------svm11------------------------
#### Use text with string kernels
data(reuters)
is(reuters)
tsv <- ksvm(reuters,rlabels,kernel="stringdot",
            kpar=list(length=5),cross=3,C=10)
tsv


#-------------------svm12------------------------
# load the kernlab package
library(kernlab)
# train the SVM
svp <- ksvm(xtrain,ytrain,type="C-svc",kernel='vanilladot',C=100,scaled=c())
Look and understand what svp contains
# General summary
svp
# Attributes that you can access
attributes(svp)
# For example, the support vectors
alpha(svp)
alphaindex(svp)
b(svp)
# Use the built-in function to pretty-plot the classifier
plot(svp,data=xtrain)


#-------------------svm13------------------------
cv.folds <- function(n,folds=3)
  ## randomly split the n samples into folds
{
  split(sample(n),rep(1:folds,length=length(y)))
}
cv.ksvm <- function(x,y,folds=3,...)
print(cross(svp))





