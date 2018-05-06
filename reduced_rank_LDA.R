reduced_rank_LDA <- function(XTrain, yTrain, XTest, yTest){
  # compute the class dependent probabilities and class dependent centroids
  
  K    = length(unique(yTrain))
  N    = dim(XTrain)[1]
  p    = dim(XTrain)[2]
  
  pi_k = matrix(data = 0, nrow = K, ncol = 1)
  M    = matrix(data = 0, nrow = K, ncol = p)
  
  for(i in 1:K){
    is_K    = yTrain == i
    pi_k[i] = sum(is_K) / N
    M[i, ]  = t(as.matrix(colMeans(XTrain[ is_K, ])))
  }
  
  W = cov( XTrain )
  
  e = eigen(W)
  V = e$vectors
  W_Minus_One_Half = V %*% diag( 1./sqrt(e$values) ) %*% t(V)
  MStar = M %*% W_Minus_One_Half
  
  BStar = cov( MStar )
  e = eigen(BStar)
  VStar = - e$vectors
  
  V = W_Minus_One_Half %*% VStar # the full projection matrix
  
  # Project the data into the invariant subspaces:
  #
  XTrainProjected = t( t(V) %*% t(XTrain) )
  XTestProjected = t( t(V) %*% t(XTest) )
  MProjected = t( t(V) %*% t(M) ) # the centroids projected
  
  # Classify the training/testing data for each possible projection dimension:
  # 
  TrainClassification = matrix( data=0, nrow=N, ncol=p ) # number of samples x number of projection dimensions 
  
  discriminant = matrix( data=0, nrow=1, ncol=K )
  for( si in 1:N ){ # for each sample
    for( pi in 1:p ){ # for each projection dimension 
      for( ci in 1:K ){ # for each class centroid 
        discriminant[ci] = 0.5 * sum( ( XTrainProjected[si,1:pi] - MProjected[ci,1:pi] )^2 ) - log( pi_k[ci] )
      }
      TrainClassification[si,pi] = which.min( discriminant )
    }
  } 
  
  N = dim(XTest)[1]
  TestClassification = matrix( data=0, nrow=N, ncol=p ) # number of samples x number of projection dimensions 
  
  discriminant = matrix( data=0, nrow=1, ncol=K )
  for( si in 1:N ){ # for each sample 
    for( pi in 1:p ){ # for each projection dimension 
      for( ci in 1:K ){ # for each class centroid
        discriminant[ci] = 0.5 * sum( ( XTestProjected[si,1:pi] - MProjected[ci,1:pi] )^2 ) - log( pi_k[ci] )
      }
      TestClassification[si,pi] = which.min( discriminant )
    }
  } 
  
  return( list(XTrainProjected,XTestProjected,MProjected,TrainClassification,TestClassification) )
}