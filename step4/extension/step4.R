#' ----------------------------------------------------------------
#' Revise Figure 8 in the second paper with different settings 
#' 
#' Author: mwelz & femke
#' Last changed: Apr 1, 2021
#' ----------------------------------------------------------------
rm(list = ls()) ; gc() ; cat("\014") 


## 0. initialize ----
# load helper functions
source(paste0(getwd(), "/step4/extension/step4-functions.R"))

# initialize simulation parameters
n.set <- seq(from = 10, to = 80, by = 10) 
d.set <- 1:30 
m     <- 1000
alpha <- 0.1 # significance level

# simulate
tests.ls <- simulate(n.set = n.set, d.set = d.set, m = m, seed = 1, df = 2)

# store
save(tests.ls, file = paste0(getwd(), "/step4/extension/step4-simdata.Rdata"))
