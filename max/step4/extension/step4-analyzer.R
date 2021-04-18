#' ----------------------------------------------------------------
#' Revise Figure 8 in the second paper with different settings:
#' Analyze the simulation results 
#' 
#' Author: mwelz 
#' Last changed: Apr 1, 2021
#' ----------------------------------------------------------------
rm(list = ls()) ; cat("\014")

# load the list with the simulation results: "tests.ls"
load(paste0(getwd(), "/max/step4/extension/step4-simdata.Rdata"))

scenarios <- names(tests.ls)
scenarios <- scenarios[-which(scenarios == "parameters")]

n.set <- tests.ls$parameters$n.set
d.set <- tests.ls$parameters$d.set


tests.ave <- matrix(NA_real_, nrow = length(scenarios), ncol = 8)
rownames(tests.ave) <- scenarios
colnames(tests.ave) <- c("n", "d", colnames(tests.ls$`n=10_d=1`))

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


# left panel: fix d
for(d in d.set){
  
  # extract the relevant observations
  idx <- unname(tests.ave[, "d"] == d)
  
  pdf(file = paste0(getwd(),
                    "/max/step4/extension/plot-fixed-d/plot_d=", d, ".pdf"))
  plot(tests.ave[idx, "n"], tests.ave[idx, "classic_reject"], 
       xlab = "sample size n", ylab = "power", type = "l", lty = 2,
       main = paste0("Empirical power at 0.1 significance; d = ", d),
       ylim = c(0,1), 
       cex = 1.5, cex.lab = 1.8, cex.main = 1.8, cex.axis = 1.6)
  lines(tests.ave[idx, "n"], tests.ave[idx, "robust_reject"], col = "blue")
  legend("bottomleft", legend = c("classic", "robust"),
         col=c("black", "blue"), lty = c(2,1), cex = 1.6) # add legend 
  dev.off()
  
} # FOR


# right panel: fix n
for(n in n.set){
  
  # extract the relevant observations
  idx <- unname(tests.ave[, "n"] == n)
  
  pdf(file = paste0(getwd(),
                    "/max/step4/extension/plot-fixed-n/plot_n=", n, ".pdf"))
  plot(tests.ave[idx, "d"], tests.ave[idx, "classic_reject"], 
       xlab = "dimension d", ylab = "power", type = "l", lty = 2,
       main = paste0("Empirical power at 0.1 significance; n = ", n),
       ylim = c(0,1),
       cex = 1.5, cex.lab = 1.8, cex.main = 1.8, cex.axis = 1.6)
  lines(tests.ave[idx, "d"], tests.ave[idx, "robust_reject"], col = "blue")
  legend("bottomleft", legend = c("classic", "robust"),
         col=c("black", "blue"), lty = c(2,1), cex = 1.6) # add legend 
  dev.off()
  
} # FOR
