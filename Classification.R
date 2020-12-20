df <- read.csv('E:/Manish/R-Project/Data/tree_ex.csv')

j = 2
s = 0

dfR1 <- df[which(df[,(j+1)] <= s),]
dfR2 <- df[which(df[,(j+1)] > s),]

y_R1 = dfR1[,1] #233
y_R2 = dfR2[,1] #266

y_hat_R1 = mean(y_R1)
y_hat_R2 = mean(y_R2)

RSS=sum((y_R1 - y_hat_R1)^2) +  sum((y_R2 - y_hat_R2)^2)

RSS


#run Function
RSS_Value(df, 2, 0)

RSS_Value <- function(df, j, s){

  dfR1 <- df[which(df[,(j+1)] < s),]
  dfR2 <- df[which(df[,(j+1)] >= s),]
  
  y_R1 = dfR1[,1]
  y_R2 = dfR2[,1]
  
  y_hat_R1 = mean(y_R1)
  y_hat_R2 = mean(y_R2)
  
  RSS=sum((y_R1 - y_hat_R1)^2) +  sum((y_R2 - y_hat_R2)^2)
  
  return(RSS)
}

#run Function
rss_value = RSS_Value(df, 2, 0)

rss_value


x1.grid <- seq(0, 10, length.out=100)

x2.grid <- seq(-5, 5, length.out=100)



for ( val in x1.grid) {
  
  print (val)
  print(RSS_Value(df, 1, val))
  #print('=======')
}

for ( val in x2.grid) {
  
  print (val)
  print(RSS_Value(df, 2, val))
  print('=======')
}


