# equation (25) in second paper
g <- function(x){
  mu    <- median(x)
  sigma <- mad(x)
  return(mu + sigma * tanh((x - mu) / sigma))
}



#' performs _m_ simulation runs as in Figure 8 in Jakob and Peter's paper.
#' 
#' @param n.set Vector of sample sizes that are to be considered
#' @param d.set Vector of dimensions that are to be considered
#' @param m Number of simulation runs
#' @param alpha Significance level for the permutation test
#' @param seed Random seed
#' @return A list with the summary statistics of the test for each combination
#' 
#' @export
simulate <- function(n.set, d.set, m = 1000, alpha = 0.1, seed = 1, df = 2){
  
  
  # random seed
  set.seed(seed)
  
  # helper objects
  tests.ls <- list() # helper object
  R.arr    <- rep(NA_integer_, length(n.set))
  
  ## simulate
  
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
        x <- sapply(1:d, function(...) rt(n, df = df))
        y <- sapply(1:d, function(...) rt(n, df = df))
        
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
  
  
  # append the parameters used
  tests.ls$parameters <- list(m = m, n.set = n.set, d.set = d.set, R.values = R.arr, alpha = alpha)
  
  return(tests.ls)
  
}
                     
                     
