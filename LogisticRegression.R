logistic_regression_weekly_dataset <- function(){
  library(ISLR)
  train=(Weekly$Year<2010)
  Weekly.2010=Weekly[!train,]
  dim(Weekly.2010)
  Direction.2010=Weekly$Direction[!train]
  glm.fit = glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
                  Volume, family = binomial, data = Weekly, subset=train)
  glm.probs = predict(glm.fit,Weekly.2010,type="response")
  contrasts(Weekly$Direction)
  glm.pred=rep("Down",dim(Weekly.2010)[1])
  glm.pred[glm.probs > 0.5] = "Up"
  table(glm.pred,Direction.2010)
  mean(glm.pred==Direction.2010)
}


logistic_regression_weekly_dataset()



lda_weekly_dataset <- function(){
  library(ISLR)
  library(MASS)
  train=(Weekly$Year<2010)
  Weekly.2010=Weekly[!train,]
  dim(Weekly.2010)
  Direction.2010=Weekly$Direction[!train]
  lda.fit = lda(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
                  Volume, family = binomial, data = Weekly, subset=train)
  lda.pred=predict(lda.fit,Weekly.2010)
  names(lda.pred)
  lda.class = lda.pred$class
  table(lda.class,Direction.2010)
  mean(lda.class==Direction.2010)
}

lda_weekly_dataset()


gradiant_derivative <- function(alpha, epsilon, x, y){
  #Create two blank matrix's
  beta <- matrix(c(0,0))
  points <- matrix(c(0,0))
  
  #Create a repeating loop that breaks when epsilon is above a threshold
  repeat{
    #Create beta variables
    #beta_1 = beta[1] - alpha * (2 * lambda * beta[1] - 2 * (sum(x[,1] * y) - sum(x[,1]^2 * beta[1]) - sum(x[,1]*x[,2]*beta[2])))
    #beta_2 = beta[2] - alpha * (2 * lambda * beta[2] - 2 * (sum(x[,2] * y) - sum(x[,2]^2 * beta[2]) - sum(x[,1]*x[,2]*beta[1])))
    
    beta_1 = beta[1] + alpha * ( sum(y) 
                                 - sum( ((2.71)^(beta[1] + beta[2]*x))
                                 /(1+((2.71)^(beta[1] + beta[2]*x)))))
    

    beta_2 = beta[2] + alpha * ( sum(x*y) 
                                 - sum( x* ((2.71)^(beta[1] + beta[2]*x))
                                        /(1+((2.71)^(beta[1] + beta[2]*x)))))
    
    #Put betas into single matrix
    beta_new <- matrix(c(beta_1,beta_2))
    
    #Record the data points
    points <- cbind(points,beta_new)
    
    #Calculate the change from old beta to new
    beta_change <- beta_new - beta
    
    if (is.nan(beta_change[1]) & is.nan(beta_change[2])) {break}
    
    #If statement with break point
    if(abs(beta_change[1]) <= epsilon & abs(beta_change[2])<= epsilon){
      break}
    
    #Update Beta point
    beta <- beta_new
  }
  print(beta)
  #plot
  x_plot <- points[1,]
  y_plot <- points[2,]
  
  plot(x_plot,y_plot)
}

df <- read.csv('E:/Manish/R-Project/Data/logit_ex.csv')

y <- df[[1]]  # by column number
x <- df[[2]]  # by column number


#run Function
gradiant_derivative(.01, .0001, x, y)

