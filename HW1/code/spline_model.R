# ieri_domani -------------------------------------------------------------

load("/path/to/ieri_domani.RData")
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
general_design_matrix = matrix(NA, nrow = nrow(train), ncol = (d+1))
general_design_matrix[,1] = 1
general_design_matrix[,2] = train$x
general_design_matrix[,3] = (train$x)^2
general_design_matrix[,4] = (train$x)^3

# Generate the design matrices that are specific for each q value
create_matrix <- function(q) {
  design_matrix = matrix(NA, nrow = nrow(train), ncol = q)
  knot = quantile(train$x, probs = seq(0, 1, length.out = q+2))[1:q+1]
  for (i in 1:q) {
    design_matrix[,i] = (pmax(0, train$x-knots[[match(q,q_vals)]][i]))^3
  }
  return (design_matrix)
}

y = train$y

fhat_plot <- function(q) {
  beta = lm(y ~ 1 + general_design_matrix[,2] + general_design_matrix[,3] + 
            general_design_matrix[,4] + create_matrix(q)[,1] + 
            create_matrix(q)[,2] + create_matrix(q)[,3])
  fhat <- predict(beta, newdata = train)
  plot( y ~ x , train)
  lines(fhat ~ x, train)
  output <- list("beta" = beta, "fhat" = fhat)
  return (output)
}

q_3 <- fhat_plot(3)
q_5 <- fhat_plot(5)
q_10 <-fhat_plot(10)


# Now that we have the fit over our Training data we can go ahead and compute some 
# quantities to predict which of the q values (hyperparameters) gives us the best 
# fit

# CP: cp = MSE_Tr + 2*sigma^2_hat*(p/n). This quantity only looks at the fit over
# the training data and therefore can lead to a large overoptimism

mean_squared_error <- function(y_pred, f_hat) mean((y_pred-f_hat)^2)

fhats = list(q_3$fhat, q_5$fhat, q_10$fhat)
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

