# *********************************************************************** #
#                         ** Statistical Learning **                      #
 #                       ** HW-01 (Spring semester) **                    # 
# *********************************************************************** #

#' ---
#' title: "Statistical Learning"
#' date: "HW-01 (Spring semester)"
#' ---

# Part 0 ------------------------------------------------------------------

# Packages
# url: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
# install.packages("viridis") # install from CRAN first if needed
require(viridis) # Load the package >> just a color-scale

# Part 1 ------------------------------------------------------------------

# Cosine-basis
cos.basis = function(x, j = 4) 1*(j == 0) + sqrt(2)*cos(pi*j*x)*(j > 0)
# Plot the first 6 basis functions
j.max = 5
mycol = viridis(j.max + 1, alpha = .5)
# Open the graphical device
curve(cos.basis(x,0), n = 501,  ylim = c(-2,2), col = mycol[1], lwd = 3,
      main = " ", xlab = "", ylab = expression(phi[j](x)))
# Add the other basis functions
for (idx in 1:j.max) curve(cos.basis(x, j = idx), n = 501, add = T, col = mycol[idx + 1], lwd = 3)
legend("bottom", paste("j =", 0:j.max), col = mycol, lwd = 4, cex = .7, bty = "n", horiz = T)

# Part 2 ------------------------------------------------------------------

# Doppler function scaled in [0,1]
doppler.fun <-  function(x) sqrt(x*(1 - x))*sin( (2.1*pi)/(x + 0.05) )
curve(doppler.fun(x), from = 0, to = 1, main = "", xlab = "", ylab = "m(x)",
      n = 1001, col = gray(.8), lwd = 3)

# Part 3 ------------------------------------------------------------------

# Let's now numerically evaluate the Fourier coefficients 
# of the Doppler under our cosine-basis
j.max   <- 200
f.coeff <- rep(NA, j.max+1)
for (idx in 0:j.max){
  foo = tryCatch(
    integrate(function(x, j) doppler.fun(x) * cos.basis(x,j), lower = 0, upper = 1, j = idx)$value,
    error = function(e) NA
  )
  f.coeff[idx + 1] = foo
}
# Visualize the Fourier coefficients
plot(f.coeff, type = "h", ylab = expression(beta[j]), main = "", xlab = "")

# Part 3 ------------------------------------------------------------------
# Time to rebuild/approximate our Doppler with an n-term (linear) approximation.
# Let's make a function for this purpose...
proj.cos <- function(x, f.coeff, j.max = 10){
  out = rep(0, length(x))
  for(idx in 0:j.max){
    if ( !is.na(f.coeff[idx + 1]) ) out = out + f.coeff[idx + 1] * cos.basis(x, j = idx)
  }
  return(out)
}

# Visualize some n-terms approximations
j.seq = c(5, 10, 25, 50, 100, 150)
mycol = viridis(length(j.seq), alpha = .7)

par(mfrow = c(2,3)) # split the graphical device in a 2 x 3 matrix
for (idx in 1:length(j.seq)){
  # Original function
  curve(doppler.fun(x), from = 0, to = 1, 
        main = paste(j.seq[idx], "-term approximation", sep = ""),
        xlab = "", ylab = expression(m[J](x)),
        n = 1001, col = gray(.8), lwd = 3)
  # Add approximation
  curve(proj.cos(x, f.coeff = f.coeff, j.seq[idx]),
        n = 1001, col = mycol[idx], lwd = 4,
        add = TRUE)
}
par(mfrow = c(1,3)) # back to default


# ieri_domani -------------------------------------------------------------

load("ieri_domani.RData")
str(df)
head(data.frame(x.train = df$x, y.train = df$y.yesterday, x.test = df$x, y.test=NA))

# Train/Test
train <- data.frame(x = df$x, y = df$y.yesterday)
test  <- data.frame(x = df$x, y = df$y.tomorrow)


