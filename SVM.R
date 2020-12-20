library(ISLR)
attach(Carseats)
High=ifelse(Sales>8,1,-1)
Carseats=data.frame(Carseats,High)
x=matrix(c(Price, Income), ncol=2)
y=c(High)
x[y==1 ,]=x[y==1 ,] + 1
plot(x,col=(3-y))

dat = data.frame(x=x, y=as.factor(y))
library(e1071)
svmfit=svm(y~.,dat,kernel="linear",cost=10,scale=FALSE)
ypred=predict(svmfit,dat)
table(predict=ypred,truth=dat$y)

set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)

svmfit.radial=svm(y~.,dat,kernel="radial",cost=10,gamma=1)
ypred.radial=predict(svmfit.radial,dat)
table(predict=ypred.radial,truth=dat$y)

set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="radial",ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)),gamma=1)
summary(tune.out)
