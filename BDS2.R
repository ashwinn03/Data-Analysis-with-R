
boosted_decision_stump <- function(X_train,X_test,y_train,y_test,B,s,n)
{
  
  y_hat_less<-c()
  y_hat_great<-c()
  x1<-c()
  x2<-c()
  train_RSS<-c()
  f<- matrix(nrow=B,ncol=length(X_train),byrow = TRUE)
  f1<- matrix(nrow=1,ncol=length(X_train),byrow = TRUE)
  for (j in 1:B)
  {
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
      
      
      x1[i]<-sum((y_train-y_hat_less[i])^2)
      x2[i]<-sum((y_train-y_hat_great[i])^2)
    }
    
    
    train_RSS[j]<-min(x1+x2)
    optimal_s<-s[which.min(x1+x2)]
    optimal_y1<-y_hat_less[which.min(x1+x2)]
    optimal_y2<-y_hat_great[which.min(x1+x2)]
    f1[X_train<optimal_s]<-optimal_y1
    f1[X_train>=optimal_s]<-optimal_y2
    
    
    f[j,]<-f1
    y_train<-y_train - n*f[j,]
  }
  
  test_RSS <- 0
  for(i in 1:length(X_test)){
    
    test_RSS <- test_RSS+(y_test[i]- (sum(0.01*f[,i])))^2
  }
  return(test_RSS/length(X_test))


}
