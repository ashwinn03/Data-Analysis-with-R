
attach(Boston)
set.seed(2506)
new <- data.frame(medv,rm,lstat)
#dividing data into half
division=(dim(new)[1])/2
train <- sample(1:nrow(new), division)
Boston.train<-new[train,]
Boston.test<-new[-train,]

lstat_s<-seq(1.8, 37.9, by=0.1)
rm_s<-seq(3.6, 8.7, by=0.1)

names(Boston.train)
lstat_train=Boston.train[,3]
medv_train=Boston.train[,1]
rm_train=Boston.train[,2]
lstat_test=Boston.test[,3]
medv_test=Boston.test[,1]
rm_test=Boston.test[,2]


#DS algorithm

decision_stump <- function(X_train,X_test,y_train,y_test,s)
{
  y_hat_less<-c()
  y_hat_great<-c()
  x1<-c()
  x2<-c()
  for (i in 1:length(s))
    
  {
    if (is.nan(mean(y_train[X_train<s[i]])))
    {y_hat_less[i]<-0    }
    else
    {y_hat_less[i]<-mean(y_train[X_train<s[i]])}
    if (is.nan(mean(y_train[X_train>=s[i]])))
    {y_hat_great[i]<-0    }
    else
    {y_hat_great[i]<-mean(y_train[X_train>=s[i]])}
    
    
    x1[i]<-sum((y_train-y_hat_less[i])**2)
    x2[i]<-sum((y_train-y_hat_great[i])**2)
  }
  
  train_RSS<-min(x1+x2)
  optimal_s<-s[which.min(x1+x2)]
  train_MSE<-train_RSS/length(X_train)
  y_hat_less<-mean(y_train[X_train<optimal_s])
  y_hat_great<-mean(y_train[X_train>=optimal_s])
  x1<-sum((y_test-y_hat_less)**2)
  x2<-sum((y_test-y_hat_great)**2)
  test_MSE<-(x1+x2)/length(X_test)
  return(c(test_MSE))
 
  
}


