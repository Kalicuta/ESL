source("subset_selection.R")

X = load_data("Data/prostate.data")

train_data = X$train_data
test_data  = X$test_data
n          = X$num_of_train_sample
p          = X$num_of_predictors

for(k in 0:p){
  
  cat("Testing", k, "-predictor models \n\n")
  
  stats = best_subset(k, p, train_data, 10)
  
  complexity_parameter <- if(k==0) k else cbind(complexity_parameter, k)
  cv_results  <- if(k==0) stats[[1]] else cbind(cv_results, stats[[1]])
  best_formula <- if(k==0) stats[[3]] else c(best_formula, stats[[3]])
}

OSE = one_standard_error_rule(complexity_parameter, cv_results)
complexity_parameter_idx = OSE$index

predictor_formula = best_formula[complexity_parameter_idx]

result = predicting(predictor_formula, train_data, test_data, 8)
y = result$y_test
pdt = result$pdt
n = result$n_test

mse = mean((y - pdt)^2)
sErr = sqrt( var((y - pdt)^2) / n)

print(result$coef)
print(mse)
print(sErr)