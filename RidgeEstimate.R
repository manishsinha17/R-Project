#Create X Matrix
x <- matrix(c(2,4,-1,0,5,3,5,10,9,-1),ncol=2)



#Create Y Matrix
y <- matrix(c(10,21,-4,1,25))


#This is the function that uses the derivative from homework part 1
gradiant_derivative <- function(alpha, epsilon, lambda, x, y){
  #Create two blank matrix's
  beta <- matrix(c(0,0))
  points <- matrix(c(0,0))
  
  #Create a repeating loop that breaks when epsilon is above a threshold
  repeat{
    #Create beta variables
    beta_1 = beta[1] - alpha * (2 * lambda * beta[1] - 2 * (sum(x[,1] * y) - sum(x[,1]^2 * beta[1]) - sum(x[,1]*x[,2]*beta[2])))
    beta_2 = beta[2] - alpha * (2 * lambda * beta[2] - 2 * (sum(x[,2] * y) - sum(x[,2]^2 * beta[2]) - sum(x[,1]*x[,2]*beta[1])))

    print(beta_1)
    print(beta_2)
        
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
  
  #plot
  x_plot <- points[1,]
  y_plot <- points[2,]
  
  plot(x_plot,y_plot)
}

#run Function
gradiant_derivative(.001, .0001, 2, x, y)







#This is using the linear algebra technique

gradiant_linear_algebra <- function(alpha, epsilon, lambda, x, y){
  beta <- matrix(c(0,0))
  points <- matrix(c(0,0))
  
  repeat{
    beta_new = beta - alpha * (-2*t(x) %*% y + 2*t(x) %*% x %*% beta + 2*lambda*beta)
    
    points <- cbind(points,beta_new)
    
    beta_change <- beta_new - beta
    
    if(abs(beta_change[1]) <= epsilon & abs(beta_change[2])<= epsilon){break}
    
    beta <- beta_new
    
  }
  
  x_plot <- points[1,]
  y_plot <- points[2,]
  
  plot(x_plot,y_plot)
  
}

gradiant_linear_algebra(.001, .0001, 2, x, y)



linear_regression_auto_dataset <- function(){

      auto <- read.csv("https://trevorhastie.github.io/ISLR/Auto.csv", 
                       stringsAsFactors = FALSE,
                       na.strings = "?")
      
      
      # Fit Linear Regression Model
      mlr <- lm(
        mpg ~ 
          cylinders + 
          displacement + 
          horsepower + 
          weight + 
          acceleration + 
          year + 
          origin, 
        data = auto, 
        na.action = na.omit
      )
      
      # View Regression Estimates
      summary(mlr)
      

}

linear_regression_auto_dataset()