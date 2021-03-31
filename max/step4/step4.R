#' ----------------------------------------------------------------
#' Revise Figure 8 in the second paper with different settings 
#' 
#' Author: mwelz
#' Last changed: Mar 31, 2021
#' ----------------------------------------------------------------
rm(list = ls()) ; gc() ; cat("\014") 


## 0. initialize ----
n.set <- c(20, 30) # seq(from = 10, to = 80, by = 10) # uncomment for choices in paper
d.set <- c(1,2)    # 1:30 # uncomment for choices in paper
m     <- 3         # number of repetitions (set to 500 later)
alpha <- 0.1       # significance level
set.seed(1)


# helper objects
tests.ls <- list() # helper object
R.arr    <- rep(NA_integer_, length(n.set))

# equation (25) in second paper
g <- function(x){
  mu    <- median(x)
  sigma <- mad(x)
  return(mu + sigma * tanh((x - mu) / sigma))
}


## 1. simulate ----

for(i in 1:length(n.set)){
  
  n        <- n.set[i]
  R        <- floor(200 + 5000/n) # number replications for permutation test test
  R.arr[i] <- R
  
  for(j in 1:length(d.set)){
    
    d <- d.set[j]
    
    # helper object
    test.mat <- matrix(NA_real_, nrow = m, ncol = 6)
    colnames(test.mat) <- c(paste0("classic_", c("pval", "reject", "stat")),
                            paste0("robust_", c("pval", "reject", "stat")))

    for(k in 1:m){
      
      # sample d-dimensional data from t(2)-distribution
      x <- sapply(1:d, function(...) rt(n, df = 2))
      y <- sapply(1:d, function(...) rt(n, df = 2))
      
      # implement the desired dependence in first dimension
      y[,1] <- x[,1]
      
      # perform the test following the suggested number of replicates on p.2788 of the Annals paper
      test.classic <- energy::dcor.test(x = x, y = y, R = R)
      test.robust  <- energy::dcor.test(x = g(x), y = g(y), R = R)
      
      # store results
      test.mat[k, "classic_pval"]   <- test.classic$p.value
      test.mat[k, "classic_reject"] <- ifelse(test.classic$p.value < alpha, 1, 0)
      test.mat[k, "classic_stat"]   <- test.classic$statistic
      test.mat[k, "robust_pval"]    <- test.robust$p.value
      test.mat[k, "robust_reject"]  <- ifelse(test.robust$p.value < alpha, 1, 0)
      test.mat[k, "robust_stat"]    <- test.robust$statistic
      
    } # FOR repetitions
    
    # store results
    tests.ls[[paste0("n=", n, "_", "d=", d)]] <- test.mat
    
  } # FOR dimensions
} # FOR sample size


## 2. store ----
tests.ls$parameters <- list(m = m, R.values = R.arr, alpha = alpha)
save(tests.ls, file = paste0(getwd(), "/max/step4/step4-simdata.Rdata"))
