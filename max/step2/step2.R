#' -------------------------------------------------
#' For an illustration generate X_n with n = 100,000 from the standard normal distribution, and set Y_i = X_i^2 . Compute both the Pearson correlation cor(X_n , Y_n ) and dCor(X_n , Y_n ) . Also construct two other situations where univariate variables X and Y are dependent but have cor(X, Y) = 0, and compute cor and dCor for large samples of (X, Y).
#' 
#' Author: mwelz
#' Last changed: Mar 31, 2021
#' -------------------------------------------------
rm(list = ls()) ; cat("\014")
set.seed(6382132)

# draw base data from N(0,1), n = 1,000 to save computation time
n <- 100000
x <- rnorm(n, mean = 0, sd = 1) 

# use random sample for the plots (saves storage)
idx <- sample(1:n, size = 1000)

# case 1:
y.1    <- x^2 
rho.1  <- cor(x, y.1, method = "pearson") # 0.01
dcor.1 <- energy::dcor2d(x, y.1) # 0.289

pdf(file = paste0(getwd(), "/max/step2/case1.pdf"))
plot(x[idx], y.1[idx], ylab = "x^2", xlab = "x")
dev.off()

# case 2
y.2    <- sqrt(abs(x)) 
rho.2  <- cor(x, y.2, method = "pearson") # ~ 0
dcor.2 <- energy::dcor2d(x, y.2) # 0.3

pdf(file = paste0(getwd(), "/max/step2/case2.pdf"))
plot(x[idx], y.2[idx], ylab = "sqrt{|x|}", xlab = "x")
dev.off()

# case 3
y.3    <- dnorm(x, mean = 0, sd = 1) 
rho.3  <- cor(x, y.3, method = "pearson") # ~ 0 
dcor.3 <- energy::dcor2d(x, y.3) # 0.3

pdf(file = paste0(getwd(), "/max/step2/case3.pdf"))
plot(x[idx], y.3[idx], ylab = "phi(x)", xlab = "x")
dev.off()
