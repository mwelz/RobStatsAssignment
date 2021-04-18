rm(list = ls()) ; cat("\014")
n=100000
set.seed(6382132)


x <- rnorm(n, mean = 0, sd = 1) 

# use random sample for the plots (saves storage)
idx <- sample(1:n, size = 10000)

# case 1:
y.1    <- x^2 
rho.1  <- cor(x, y.1, method = "pearson") # 0.01
dcor.1 <- energy::dcor2d(x, y.1) # 0.289
rho.1
dcor.1

pdf(file = paste0(getwd(), "/step2/case1.pdf"))
plot(x[idx], y.1[idx], ylab = expression(x^2), xlab = "x", main = 'Square function',
     cex = 1.5, cex.lab = 1, cex.main = 1.3, cex.axis = 1.3)
dev.off()

# case 2
y.2    <- dnorm(x, mean = 0, sd = 1) 
rho.2  <- cor(x, y.2, method = "pearson") # ~ 0 
dcor.2 <- energy::dcor2d(x, y.2) # 0.3

pdf(file = paste0(getwd(), "/step2/case2.pdf"))
plot(x[idx], y.2[idx], ylab = expression(varphi(x)), xlab = "x",main = 'Density function of N(0.1)',
     cex = 1.5, cex.lab = 1, cex.main = 1.3, cex.axis = 1.3)
dev.off()


# case 3
Xn=rnorm(n)
XnAbs=abs(Xn)
rho.3  <- cor(Xn, XnAbs, method = "pearson") # ~ 0 
dcor.3 <- energy::dcor2d(Xn, XnAbs) # 0.3

pdf(file = paste0(getwd(), "/step2/case3.pdf"))
plot(Xn[idx], XnAbs[idx], ylab = expression(abs(x)), xlab = "x", main='Absolute value function',
     cex = 1.5, cex.lab = 1, cex.main = 1.3, cex.axis = 1.3)
dev.off()
