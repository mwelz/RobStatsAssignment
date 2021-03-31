#' ----------------------------------------------------------------
#' Revise Figure 8 in the second paper with different settings 
#' 
#' Author: mwelz
#' Last changed: Mar 31, 2021
#' Revised by Femke
#'
#' ----------------------------------------------------------------
# # install.packages("energy")
# library(energy)
# rm(list = ls()) ; gc() ; cat("\014") 


## 0. initialize ----
n.set <- seq(from = 10, to = 80, by = 10) # uncomment for choices in paper
d.set <- 1:30 # uncomment for choices in paper
m     <- 500         # number of repetitions (set to 500 later)
alpha <- 0.1       # significance level



## equation (25) in second paper ----
g <- function(x){
  mu    <- median(x)
  sigma <- mad(x)
  return(mu + sigma * tanh((x - mu) / sigma))
}


## 1. simulate ----
simulate <- function(n.set,d.set,m,alpha,seed=1){
  set.seed(seed)
  # helper objects
  tests.ls <- list() # helper object
  R.arr    <- rep(NA_integer_, length(n.set))
  counters_classic=matrix(0,nrow=length(n.set),ncol=length(d.set))
  counters_robust=counters_classic
  for(i in 1:length(n.set)){
    n        <- n.set[i]
    R        <- floor(200 + 5000/n) 
    # number replications for permutation test test
    R.arr[i] <- R
      for(j in 1:length(d.set)){
        d <- d.set[j]
        # helper object
        test.mat <- matrix(NA_real_, nrow = m, ncol = 6)
        colnames(test.mat) <- c(paste0("classic_", 
                                       c("pval", "reject", "stat")),
                                paste0("robust_", c("pval", "reject", "stat")))
        for(k in 1:m){
          # sample d-dimensional data from t(2)-distribution
          x <- sapply(1:d, function(...) rt(n, df = 2))
          y <- sapply(1:d, function(...) rt(n, df = 2))
          
          # implement the desired dependence in first dimension
          y[,1] <- x[,1]
          
          # perform the test following the suggested number of replicates 
            # on p.2788 of the Annals paper
          test.classic <- energy::dcor.test(x = x, y = y, R = R)
          test.robust  <- energy::dcor.test(x = g(x), y = g(y), R = R)
      
          # store results
          test.mat[k, "classic_pval"]   <- test.classic$p.value
          test.mat[k, "classic_reject"] <- ifelse(test.classic$p.value 
                                                  < alpha, 1, 0)
          test.mat[k, "classic_stat"]   <- test.classic$statistic
          test.mat[k, "robust_pval"]    <- test.robust$p.value
          test.mat[k, "robust_reject"]  <- ifelse(test.robust$p.value 
                                                  < alpha, 1, 0)
          test.mat[k, "robust_stat"]    <- test.robust$statistic
          } # FOR repetitions
        # store results
        tests.ls[[paste0("n=", n, "_", "d=", d)]] <- test.mat
        } # FOR dimensions
    } # FOR sample size
  ## 2. store ----
  tests.ls$parameters <- list(m = m, R.values = R.arr, alpha = alpha)
  return(tests.ls)
}

# In function of samplesize, d=2 ----
tests.ls1=simulate(n.set,2,m,alpha)
# In function of dimension, n=20 ----
tests.ls2=simulate(20,d.set,m,alpha)

## x. analysis? ---

d.set=2
empow_classic = matrix(0,length(n.set),length(d.set))
empow_robust =empow_classic
for(i in 1:length(n.set)){
  for(d in d.set){
    empow_classic[i]=mean(tests.ls1[[paste0("n=", n.set[i], "_", "d=", d)]][,'classic_reject'])
    empow_robust[i]=mean(tests.ls1[[paste0("n=", n.set[i], "_", "d=", d)]][,'robust_reject'])
  }
}
