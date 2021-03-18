require(viridis)
par(mfrow = c(1,1))

#function that shows n functions belonging to G, truncated power functions
#of order d over q knots
g <- function(q=3, d=1, n=3){
  #first of all we define the knots and define the colours of the functions
  knots <- seq(0,1, by =  1/(q+1))
  knots <- knots[1:q+1]
  mycol <- viridis(length(knots))
  
  #then we define the cardinality and select which functions we will plot
  card_G <- d+q+1
  chosen_funcs <- sample(x = 1:card_G, replace = FALSE, size = n)
  
  #next we plot each function
  j <- 0
  for (i in chosen_funcs) {
    j <- j + 1
    if (i <= d + 1){
      #functions in the first half, they are simply x to the power of something
      if (j == 1) {curve(x^(i-1), from = 0, to = 1, col = mycol[j], ylim = (0:1))
      } else {curve(x^(i-1), from = 0, to = 1, add = TRUE, col = mycol[j])}
      
    } else {
      #functions in the second half, the difference between x and knot, to the power of something
      if (j == 1) {curve(((x-knots[i-q])^(d)), from = 0, to = 1, col = mycol[j], ylim = (0:1))
      } else {curve((x-knots[i-q])^(d), from = 0, to = 1, add = TRUE, col = mycol[j])}
    }
  }
}

#i think plotting with these numbers is the best option, we get many functions
#and they all appear pretty clear
g(4,3,4)
