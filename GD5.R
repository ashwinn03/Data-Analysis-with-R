


logistic_regression <- function(X_train,X_test,y_train,y_test,learning_rate,no_of_steps)
{
  
  sigmoid <- function(z)
  {    1/(1+exp(-z))  }
  
  sigmoid_derivative<- function(z)
  {    exp(z)/((exp(z)+1)**2)  }
  
  n1<-length(y_train)
  n2<-length(y_test)
  a<-matrix(1,n1,1)
  b=(dim(new)[2]) +1
  train_MSE<-c()
  test_MSE<-c()
  
  c<-runif(b, -0.7, 0.7)
  
  y_train<-matrix(as.numeric(y_train),1,n)
  y_test<-matrix(as.numeric(y_test),1,n)
  X_train<-data.matrix(X_train)
  X_test<-data.matrix(X_test)
  average="TRUE"
  
  if(average=="TRUE")
  {
    for (j in 1:no_of_steps  )
      {
      p_train<-sigmoid(c[1] + tcrossprod(c[2:b],X_train) )
      p_test<-sigmoid(c[1] + tcrossprod(c[2:b],X_test) )
      train_MSE<-c(train_MSE,(rowSums((y_train-p_train)**2))/n1) 
      test_MSE<-c(test_MSE,(rowSums((y_test-p_test)**2))/n2) 
      d<-2*(y_train-p_train)*sigmoid_derivative (c[1] + tcrossprod(c[2:b],X_train))
      c[1:b]<-c[1:b]+(learning_rate*colSums(c(d)*cbind(a,X_train)))/n1
      if(j>10)
        {
        if(mean(train_MSE[j-10:j-1])==train_MSE[j])
        {average="FAlSE"}
        }
        }
  }
  
  
  
  print("test_MSE")
  print(train_MSE[j])
}



