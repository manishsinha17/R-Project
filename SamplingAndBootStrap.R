logistic_regression_default_dataset_model1 <- function(){
  library(boot)
  library(ISLR)
  fit.logit = glm(formula = default ~ income + balance, 
                family = binomial, data = Default)

  summary(fit.logit)
    cv.error = cv.glm(Default, fit.logit)
    cv.error$delta
}

logistic_regression_default_dataset_model1()


logistic_regression_default_dataset_model2 <- function(){
  library(boot)
  library(ISLR)
  fit.logit = glm(formula = default ~ income + balance + student, 
                  family = binomial, data = Default)
  
  cv.error = cv.glm(Default, fit.logit)
  cv.error$delta

}


logistic_regression_default_dataset_model2()


boot.estimates=function(data, index)
+ coefficients(glm(default ~ income + balance + student,
                   family = binomial, data = data, subset = index))


boot.estimates_Default_dataset <- function() {
  library(ISLR)

  boot.estimates(Default,1:dim(Default)[1])
  
  set.seed(1)
  
  boot(Default, boot.estimates, 1000)
}



