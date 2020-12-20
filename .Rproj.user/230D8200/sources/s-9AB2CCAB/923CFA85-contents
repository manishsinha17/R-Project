wage_cubic_splines <- function() {
  library(splines)
  library(ISLR)
  #attach(Wage)
  fit = lm(wage~bs(Wage$age,knots=c(30,50,60)), data=Wage)
  pred1.1 = predict(fit,data.frame(Wage$age=30),se=T)
  plot(Wage$age,Wage$wage, col="grey")
  print(pred1.1$fit)
}

wage_smooth_splines <- function() {
  library(splines)
  library(ISLR)
  attach(Wage)
  fit2 = smooth.spline(age,wage,cv=TRUE)
  pred2.1 = predict(fit2,data.frame(age=30))
  plot(age,wage, col="darkgrey")
  pred2.1

  library(ISLR)
  library(splines)
  library(gam)
  data <- Wage
  plot(data$age ,data$wage,cex =.5,col=" darkgrey ")
  model_2=smooth.spline(data$age,data$wage,cv=TRUE)
  lines(model_2 ,col="blue",lwd=2)
  predict_smooth = predict(model_2, newdata=data.frame(age = 30))
  predict_smooth
  
  
}

wage_GAM <- function() {
  library(splines)
  library(ISLR)
  attach(Wage)
  gam1 = lm(wage~ns(age,4) + maritl, data=Wage)
  pred3 = predict(gam1,data.frame(age = 30, maritl='2. Married'),se=T)
  print(pred3$fit)
  par(mfrow=c(1,3)) #to partition the Plotting Window
  plot.Gam(gam1, se=TRUE, col="red")


  
  library(ISLR)
  library(splines)
  library(gam)
  attach(Wage)
  model_3=lm(wage~ns(age,df=4)+maritl ,data = Wage)
  par(mfrow=c(1,3)) #to partition the Plotting Window
  plot.Gam(model_3, se=TRUE, col="red")
  predict_GAM = predict(model_3, data.frame(age = 30, maritl='2. Married'))
  predict_GAM
  
  
}

library(ISLR)
library(splines)
library(gam)
attach(Wage)
age.grid<-seq(from=agelims[1], to = agelims[2])
model= lm(wage~bs(age, knots=c(30,50,60)), data = Wage)
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(model,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
abline(v=c(30,50,60),lty=2,col="blue")
predict_cubic_spline = predict(model, data.frame(age = 30))
predict_cubic_spline


library(ISLR)
library(splines)
library(gam)
data <- Wage
plot(data$age ,data$wage,cex =.5,col=" darkgrey ")
model_2=smooth.spline(data$age,data$wage,cv=TRUE)
lines(model_2 ,col="blue",lwd=2)
predict_smooth = predict(model_2, data.frame(age = 30))
predict_smooth


library(ISLR)
library(splines)
library(gam)
attach(Wage)
model_3=lm(wage~ns(age,df=4)+maritl ,data = Wage)
par(mfrow=c(1,3)) #to partition the Plotting Window
plot.Gam(model_3, se=TRUE, col="red")
predict_GAM = predict(model_3, data.frame(age = 30, maritl='2. Married'))
predict_GAM


