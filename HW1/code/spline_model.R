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

knots = list()
for (i in 1:length(q_vals)) {
  knots[[i]] = quantile(train$x, 
                        probs = seq(0, 1, 
                                    length.out = q_vals[i]+2))[1:q_vals[i]+1]
  
}

d = 3
# Generate the part of the general design matrix that is common to all
general_design_matrix = matrix(NA, nrow = nrow(train), ncol = (d+1))
general_design_matrix[,1] = 1
general_design_matrix[,2] = train$x
general_design_matrix[,3] = (train$x)^2
general_design_matrix[,4] = (train$x)^3

# Generate the design matrices that are specific for each q value
design_matrix_q_3 = matrix(NA, nrow = nrow(train), ncol = 3)
design_matrix_q_5 = matrix(NA, nrow = nrow(train), ncol = 5)
design_matrix_q_10 = matrix(NA, nrow = nrow(train), ncol = 10)

for (i in 1:ncol(design_matrix_q_3)) {
  design_matrix_q_3[,i] = (pmax(0, train$x-knots[[1]][i]))^3
}

for (i in 1:ncol(design_matrix_q_5)) {
  design_matrix_q_5[,i] = (pmax(0, train$x-knots[[2]][i]))^3
}

for (i in 1:ncol(design_matrix_q_10)) {
  design_matrix_q_10[,i] = (pmax(0, train$x-knots[[3]][i]))^3
}

y = train$y

beta_q_3 = lm(y ~ 1 + general_design_matrix[,2] + general_design_matrix[,3] + 
                general_design_matrix[,4] + design_matrix_q_3[,1] + 
                design_matrix_q_3[,2] + design_matrix_q_3[,3])

fhat_q_3 <- predict(beta_q_3, newdata = train)
plot( y ~ x , train)
lines(fhat_q_3 ~ x, train)

beta_q_5 = lm(y ~ 1 + general_design_matrix[,2] + general_design_matrix[,3] + 
                general_design_matrix[,4] + design_matrix_q_5[,1] + 
                design_matrix_q_5[,2] + design_matrix_q_5[,3] + 
                design_matrix_q_5[,4] + design_matrix_q_5[,5])

fhat_q_5 <- predict(beta_q_5, newdata = train)
plot( y ~ x , train)
lines(fhat_q_5 ~ x, train)

beta_q_10 = lm(y ~ 1 + general_design_matrix[,2] + general_design_matrix[,3] + 
                general_design_matrix[,4] + design_matrix_q_10[,1] + 
                design_matrix_q_10[,2] + design_matrix_q_10[,3] + 
                design_matrix_q_10[,4] + design_matrix_q_10[,5] + 
                design_matrix_q_10[,6] + design_matrix_q_10[,7] + 
                design_matrix_q_10[,8] + design_matrix_q_10[,9] + 
                design_matrix_q_10[,10])

fhat_q_10 <- predict(beta_q_10, newdata = train)
plot( y ~ x , train)
lines(fhat_q_10 ~ x, train)

# Now that we have the fit over our Training data we can go ahead and compute some 
# quantities to predict which of the q values (hyperparameters) gives us the best 
# fit

# CP: cp = MSE_Tr + 2*sigma^2_hat*(p/n). This quantity only looks at the fit over
# the training data and therefore can lead to a large overoptimism

mean_squared_error <- function(y_pred, f_hat) mean((y_pred-f_hat)^2)

fhats = list(fhat_q_3, fhat_q_5, fhat_q_10)
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

