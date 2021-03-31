#' ----------------------------------------------------------------
#' Replicate Figure 16 in the second paper 
#' 
#' Author: mwelz
#' Last changed: Mar 31, 2021
#' ----------------------------------------------------------------
rm(list = ls()) ; gc() ; cat("\014")
set.seed(873241)

# equation (25) in second paper
g <- function(x){
  mu    <- median(x)
  sigma <- mad(x)
  return(mu + sigma * tanh((x - mu) / sigma))
}

### 1. Exactly replicate Figure 16A ----
## Panel A: expose spurious correlation
# sample independent Gaussian X, Y
n <- 100000 
x <- rnorm(n)
y <- rnorm(n)

# sample position of the outlier
position <- sample(1:n, size = 1)

# initialize simulation
a.arr          <- seq(from = 0, to = 2e+5, by = 1e4)
dcor.arr       <- rep(NA_real_, length(a.arr))
dcor.trans.arr <- dcor.arr

# run simulation
for(i in 1:length(a.arr)){
  
  a <- a.arr[i]
  x.contam <- x
  x.contam[position] <- a
  y.contam <- y
  y.contam[position] <- a
  
  dcor.arr[i]       <- energy::dcor2d(x.contam, y.contam)
  dcor.trans.arr[i] <- energy::dcor2d(g(x.contam), g(y.contam))
} # FOR

pdf(file = paste0(getwd(), "/max/step3/replication/fig16a.pdf"))
plot(a.arr, dcor.arr, type = "l", xlab = "a", ylab = "dCor", main = "dCor of Gaussian data with 1 outlier at (a,a)")
lines(a.arr, dcor.trans.arr, type = "l", col = "blue")
dev.off()


### 1. Exactly replicate Figure 16B ----
## Panel B: expose spurious independence
a.arr          <- seq(from = 0, to = 1e+6, by = 5e4)
dcor.arr       <- rep(NA_real_, length(a.arr))
dcor.trans.arr <- dcor.arr


# run simulation
for(i in 1:length(a.arr)){
  
  a <- a.arr[i]
  x.contam <- x
  x.contam[position] <- a
  y.contam <- x
  y.contam[position] <- 0
  
  dcor.arr[i]       <- energy::dcor2d(x.contam, y.contam)
  dcor.trans.arr[i] <- energy::dcor2d(g(x.contam), g(y.contam))
} # FOR

pdf(file = paste0(getwd(), "/max/step3/replication/fig16b.pdf"))
plot(a.arr, dcor.arr, type = "l", xlab = "a", ylab = "dCor", main = "dCor of linear data with 1 outlier at (a,0)")
lines(a.arr, dcor.trans.arr, type = "l", col = "blue")
dev.off()
