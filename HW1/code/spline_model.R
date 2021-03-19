# ieri_domani -------------------------------------------------------------

load("HW1/data/ieri_domani.RData")
str(df)
head(data.frame(x.train = df$x, y.train = df$y.yesterday, x.test = df$x, y.test=NA))

# Train/Test
train <- data.frame(x = df$x, y = df$y.yesterday)
test  <- data.frame(x = df$x, y = df$y.tomorrow)

# First step is to generate the 3 functions we will be fitting our training data over
# For this purpose we are given 2 parameters in each case: (d=3, q=3), (d=3, q=5), 
# (d=3, q=10)
q_vals = c(3, 5, 10)

d = 3

# Generate the part of the general design matrix that is common to all

generate_gdm_func <- function(train) {
  general_design_matrix = matrix(NA, nrow = nrow(train), ncol = (d+1))
  general_design_matrix[,1] = 1
  general_design_matrix[,2] = train$x
  general_design_matrix[,3] = (train$x)^2
  general_design_matrix[,4] = (train$x)^3
  return(general_design_matrix)
}

create_matrix <- function(q, train) {
  design_matrix = matrix(NA, nrow = nrow(train), ncol = q)
  knots = quantile(train$x, probs = seq(0, 1, length.out = q+2))[1:q+1]
  for (i in 1:q) {
    design_matrix[,i] = (pmax(0, train$x-knots[i]))^3
  }
  return (design_matrix)
}

fhat_func <- function(q, train_coef=train, predict_sample=train) {
  y = train$y
  beta = lm(y ~ generate_gdm_func(train)[,2:4] + create_matrix(q, train))
  fhat <- predict(beta, newdata = list(x = predict_sample$x))
  return ('fhat' = fhat)
}

fhat_plot <- function(q, train) {
  y = train$y
  fhat <- fhat_func(q, train)
  plot( y ~ x , train)
  lines(fhat ~ x, train)
}


q_3 <- fhat_func(3, train_coef=train, predict_sample=train)
q_5 <- fhat_func(5, train_coef=train, predict_sample=train)
q_10 <-fhat_func(10, train_coef=train, predict_sample=train)

fhat_plot(3, train)
fhat_plot(5, train)
fhat_plot(10, train)

# Now that we have the fit over our Training data we can go ahead and compute some 
# quantities to predict which of the q values (hyperparameters) gives us the best 
# fit

# CP: cp = MSE_Tr + 2*sigma^2_hat*(p/n). This quantity only looks at the fit over
# the training data and therefore can lead to a large overoptimism

mean_squared_error <- function(y_pred, f_hat) mean((y_pred-f_hat)^2)

fhats = list(q_3, q_5, q_10)
cp = rep(NA, 3)

for (i in 1:3) {
  MSE_val = mean_squared_error(y_pred = train$y, f_hat = fhats[[i]])
  p = 4+q_vals[i]
  n = length(train$y)
  sigma_hat = sum((train$y - fhats[[i]])^2)/(n-p)
  second_term = 2*sigma_hat*(p/n)
  cp[i] = MSE_val + second_term
}

plot(cp, type = 'b')
# CP is over-optimist with regards to the CV approaches (LOOCV and K-fold)


# CV: cv = 1/K * sum(MSE_Te).

k_fold_cv <- function(q, train, K=5) {
  n = length(train$y)
  folds = sample(rep(1:K, length=n))
  kcv = sapply(1:K, function(k) {
    train_set = train[which(folds != k), ]
    test_set = train[which(folds == k), ]
    return(mean_squared_error(y_pred = test_set$y, 
                              f_hat = fhat_func(q, train_coef = train_set, 
                                                predict_sample = test_set)))
  })
  return(mean(kcv))
}

cv <- sapply(q_vals, k_fold_cv, train=train, K=30)

plot(cv, type='b')





