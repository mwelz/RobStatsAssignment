#' ----------------------------------------------------------------
#' Revise Figure 16 in the second paper with different settings 
#' 
#' Author: mwelz
#' Last changed: Apr 7, 2021
#' ----------------------------------------------------------------
rm(list = ls()) ; gc() ; cat("\014")

# equation (25) in second paper
g <- function(x){
  mu    <- median(x)
  sigma <- mad(x)
  return(mu + sigma * tanh((x - mu) / sigma))
}


## 0. simulation initialization ----
set.seed(1)
n <- 50000 # number of samples
m <- 500   # simulation runs

# for revision of panel 1
a.arr.panel1          <- seq(from = 0, to = 2e+5, by = 1e4)
dcor.arr.panel1       <- matrix(NA_real_, nrow = m, ncol = length(a.arr.panel1), 
                                dimnames = list(paste0("run", 1:m), 
                                                paste0("a=", a.arr.panel1)))
dcor.arr.trans.panel1 <- dcor.arr.panel1

# for revision of panel 2
a.arr.panel2          <- seq(from = 0, to = 1e+6, by = 5e4)
dcor.arr.panel2       <- matrix(NA_real_, nrow = m, ncol = length(a.arr.panel2), 
                                dimnames = list(paste0("run", 1:m), 
                                                paste0("a=", a.arr.panel2)))
dcor.arr.trans.panel2 <- dcor.arr.panel2


## 1. simulate! ----

for(i in 1:m){
  
  # sample independent Cauchy random variables X, Y
  x <- rt(n, df = 1)
  y <- rt(n, df = 1)
  
  # sample position of the outlier
  position <- sample(1:n, size = 1)
  
  
  # panel 1: expose spurious dependence
  for(j in 1:length(a.arr.panel1)){
    
    a <- a.arr.panel1[j]
    x.contam <- x
    x.contam[position] <- a
    y.contam <- y
    y.contam[position] <- a
    
    dcor.arr.panel1[i,j]       <- energy::dcor2d(x.contam, y.contam)
    dcor.arr.trans.panel1[i,j] <- energy::dcor2d(g(x.contam), g(y.contam))
  } # FOR
  
  
  # panel 2: expose spurious independence
  for(j in 1:length(a.arr.panel2)){
    
    a <- a.arr.panel2[j]
    x.contam <- x
    x.contam[position] <- a
    y.contam <- x
    y.contam[position] <- 0
    
    dcor.arr.panel2[i,j]       <- energy::dcor2d(x.contam, y.contam)
    dcor.arr.trans.panel2[i,j] <- energy::dcor2d(g(x.contam), g(y.contam))
  } # FOR
} # FOR


## 2. plot the first simulation run ----
pdf(file = paste0(getwd(), "/max/step3/extension/fig16a-extended.pdf"))
plot(a.arr.panel1, dcor.arr.panel1[1,], type = "l", xlab = "a", ylab = "dCor", main = "dCor of Cauchy data with 1 outlier at (a,a)")
lines(a.arr.panel1, dcor.arr.trans.panel1[1,], type = "l", col = "blue")
dev.off()

pdf(file = paste0(getwd(), "/max/step3/extension/fig16b-extended.pdf"))
plot(a.arr.panel2, dcor.arr.panel2[1,], type = "l", xlab = "a", ylab = "dCor", main = "dCor of linear data with 1 outlier at (a,0)")
lines(a.arr.panel2, dcor.arr.trans.panel2[1,], type = "l", col = "blue")
dev.off()

## 3. save the results ----
save(a.arr.panel1, a.arr.panel2, dcor.arr.panel1, dcor.arr.panel2, dcor.arr.trans.panel1, dcor.arr.trans.panel2, file = paste0(getwd(), "/max/step3/extension/step3-simdata.Rdata"))
