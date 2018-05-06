library(MASS)
library(nnet)

load_data <- function(){
  
  XTrain = read.csv("Data/vowel.train.txt")
  XTrain$row.names = NULL
  yTrain = XTrain[,1]
  XTrain$y = NULL
  
  XTest = read.csv("Data/vowel.test.txt")
  XTest$row.names = NULL
  yTest = XTest[,1]
  XTest$y = NULL
  
  return(list(train_data = XTrain, test_data = XTest, train_y = yTrain, test_y = yTest))
}

indicator_matrix <- function(XTrain, yTrain){
  K = max(yTrain) # number of classes
  N = dim(XTrain)[1]
  
  Y = mat.or.vec(N, K)
  for(i in 1:N){
    Y[i, yTrain[i]] = 1
  }
  Y = as.matrix(Y)
  
  ones = as.matrix(mat.or.vec(N, 1) + 1.0)
  Xm   = as.matrix(cbind(ones, XTrain))
  
  Bhat = solve(t(Xm)%*%Xm) %*% t(Xm) %*% Y
  return(Bhat)
}

predict_lm <- function(Bhat, X){
  N = dim(X)[1]
  ones = as.matrix(mat.or.vec(N, 1) + 1.0)
  Xm   = as.matrix(cbind(ones, X))
  
  Yhat = Xm %*% Bhat
  gHat = apply(Yhat, 1, 'which.max')
  return(gHat)
}

calculate_error_rates <- function(X, Y, model, type=1){
  
  if(type == 0)
    pred = predict_lm(model, X)
  else if(type == 1)
    pred = predict(model, X)$class
  else
    pred = predict(model, X, "class")
  pred = as.double(pred)
  
  error = length(pred) - sum(pred == Y)
  error_rate = error / length(pred)
  
  return(error_rate)
}

duplicate_table_4.1 <- function(){

  X = load_data()
  
  models <- c("linear", "lda", "qda", "logistic")
  
  for(i in 1:length(models)){
    
    if(models[i] == "linear"){
      model = indicator_matrix(X$train_data, X$train_y) # Bhat
      type  = 0
    }
    else if(models[i] == "logistic"){
      train_data       = data.frame( cbind(X$train_data, X$train_y) )
      model            = multinom( X$train_y ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + x.7 + x.8 + x.9 + x.10, data=train_data, trace=FALSE)
      type             = 2
    }else{
      predictor        = get(models[i])
      model            = predictor( X$train_data, X$train_y)
      type             = 1
    }
    
    error_rate_train = calculate_error_rates(X$train_data, X$train_y, model, type)
    error_rate_test  = calculate_error_rates(X$test_data, X$test_y, model, type)
    print(sprintf("%10s: %6.2f; %6.2f", models[i], error_rate_train, error_rate_test))
  }
}