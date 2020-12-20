gradiant_derivative <- function(alpha, epsilon, lambda, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, y){
  beta <- matrix(c(0,0,0,0,0,0,0,0,0,0,0))

  #Create a repeating loop that breaks when epsilon is above a threshold
  repeat{
    #Create beta variables
    
    exponent_term = exp(beta[1] + beta[2]*x1 + beta[3]*x2 + beta[4]*x3+ beta[5]*x4 + beta[6]*x5 + beta[7]*x6 + beta[8]*x7 
                        + beta[9]*x8 + beta[10]*x9 + beta[11]*x10)

    beta_1 = beta[1] + alpha * ( sum(y) - sum(exponent_term/(1+exponent_term)))
    beta_2 = beta[2] + alpha * ( sum(x1*y)- sum( x1 * (exponent_term)/(1+exponent_term)) - 2*lambda*beta[2])
    beta_3 = beta[3] + alpha * ( sum(x2*y)- sum( x2 * (exponent_term)/(1+exponent_term)) - 2*lambda*beta[3])
    beta_4 = beta[4] + alpha * ( sum(x3*y)- sum( x3 * (exponent_term)/(1+exponent_term)) - 2*lambda*beta[4])
    beta_5 = beta[5] + alpha * ( sum(x4*y)- sum( x4 * (exponent_term)/(1+exponent_term))  - 2*lambda*beta[5])
    beta_6 = beta[6] + alpha * ( sum(x5*y)- sum( x5 * (exponent_term)/(1+exponent_term)) - 2*lambda*beta[6])
    beta_7 = beta[7] + alpha * ( sum(x6*y)- sum( x6 * (exponent_term)/(1+exponent_term)) - 2*lambda*beta[7])
    beta_8 = beta[8] + alpha * ( sum(x7*y)- sum( x7 * (exponent_term)/(1+exponent_term)) - 2*lambda*beta[8])
    beta_9 = beta[9] + alpha * ( sum(x8*y)- sum( x8 * (exponent_term)/(1+exponent_term)) - 2*lambda*beta[9])
    beta_10 = beta[10] + alpha * ( sum(x9*y)- sum( x9 * (exponent_term)/(1+exponent_term)) - 2*lambda*beta[10])
    beta_11 = beta[11] + alpha * ( sum(x10*y)- sum( x10 * (exponent_term)/(1+exponent_term)) - 2*lambda*beta[11])
    
    #Put betas into single matrix
    beta_new <- matrix(c(beta_1,beta_2,beta_3,beta_4,beta_5,beta_6,beta_7,beta_8,beta_9,beta_10,beta_11))
    
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
  return(beta)
}

df <- read.csv('E:/Manish/R-Project/Data/logit_ridge.csv')

testing <- df[1:10,]
training <- df[11:nrow(df),]
training_y <- training[[1]]  # by column number
training_x1 <- training[[2]]  # by column number
training_x2 <- training[[3]]  # by column number
training_x3 <- training[[4]]  # by column number
training_x4 <- training[[5]]  # by column number
training_x5 <- training[[6]]  # by column number
training_x6 <- training[[7]]  # by column number
training_x7 <- training[[8]]  # by column number
training_x8 <- training[[9]]  # by column number
training_x9 <- training[[10]]  # by column number
training_x10 <- training[[11]]  # by column number

#run Function
beta_result = gradiant_derivative(.01, .0001, 1 , training_x1, training_x2, training_x3, training_x4, training_x5, training_x6
                                  , training_x7, training_x8, training_x9, training_x10, training_y)

print(beta_result)

avg_prediction_error<- function(beta, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, y) {
  exponent_term = exp(beta[1] + beta[2]*x1 + beta[3]*x2 + beta[4]*x3+ beta[5]*x4 + beta[6]*x5 + beta[7]*x6 + beta[8]*x7 
                      + beta[9]*x8 + beta[10]*x9 + beta[11]*x10)
  e2 <- matrix(c(0,0,0,0,0,0,0,0,0,0,0))
  p <- matrix(c(0,0,0,0,0,0,0,0,0,0,0))
  p = exponent_term/(1 +exponent_term)
  
  #print(p)  
  
  e2 = (y-p)^2
  
  print(e2)
  
  mean(e2)

}

testing_y <- testing[[1]]  # by column number
testing_x1 <- testing[[2]]  # by column number
testing_x2 <- testing[[3]]  # by column number
testing_x3 <- testing[[4]]  # by column number
testing_x4 <- testing[[5]]  # by column number
testing_x5 <- testing[[6]]  # by column number
testing_x6 <- testing[[7]]  # by column number
testing_x7 <- testing[[8]]  # by column number
testing_x8 <- testing[[9]]  # by column number
testing_x9 <- testing[[10]]  # by column number
testing_x10 <- testing[[11]]  # by column number


avg_prediction_error(beta_result, testing_x1, testing_x2, testing_x3, testing_x4, testing_x5, testing_x6, testing_x7
                     , testing_x8, testing_x9, testing_x10, testing_y)
