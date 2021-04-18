#' ----------------------------------------------------------------
#' Revise Figure 8 in the second paper with different settings 
#' 
#' Author: mwelz & femke
#' Last changed: Apr 18, 2021
#' ----------------------------------------------------------------
rm(list = ls()) ; gc() ; cat("\014") 


## 0. initialize ----
# load helper functions
source(paste0(getwd(), "/step4/extension/step4-functions.R"))

# initialize simulation parameters
n.set <- 80 
d.set <- 1:30 
m     <- 1000
alpha <- 0.1 # significance level

# simulate
tests.ls <- simulate(n.set = n.set, d.set = d.set, m = m, seed = 1, df = 1)

# make plots ----

scenarios <- names(tests.ls)
scenarios <- scenarios[-which(scenarios == "parameters")]


tests.ave <- matrix(NA_real_, nrow = length(scenarios), ncol = 8)
rownames(tests.ave) <- scenarios
colnames(tests.ave) <- c("n", "d", colnames(tests.ls$`n=80_d=1`))

# average over the _m_ simulation runs
for(i in 1:length(scenarios)){
  
  # description of the scenario
  scenario <- scenarios[i]
  
  # sample size and dimension of the scenario
  n <- as.integer(gsub(".*=", "", (gsub("\\_.*", "", scenario))))
  d <- as.integer(gsub(".*=", "", gsub(".*_", "", scenario)))
  
  
  # averages of the summary statistics of the test
  tests.ave[i,] <- c(n, d, apply(tests.ls[[scenario]], 2, mean))
  
} # FOR



# right panel: fix n
for(n in n.set){
  
  # extract the relevant observations
  idx <- unname(tests.ave[, "n"] == n)
  
  pdf(file = paste0(getwd(),
                    "/step4/replication/plot_n=", n, "_cauchy.pdf"))
  plot(tests.ave[idx, "d"], tests.ave[idx, "classic_reject"], 
       xlab = "dimension d", ylab = "power", type = "l", lty = 2,
       main = paste0("Empirical power at 0.1 significance; n = ", n),
       ylim = c(0,1),cex=1.5,cex.lab=1.6, cex.main=1.8)
  lines(tests.ave[idx, "d"], tests.ave[idx, "robust_reject"], col = "blue")
  legend("bottomleft", legend = c("classic", "robust"),
         col=c("black", "blue"), lty = c(2,1),cex=1.6) # add legend 
  dev.off()
  
} # FOR
