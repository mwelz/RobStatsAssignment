#' ----------------------------------------------------------------
#' Revise Figure 16 in the second paper with different settings:
#' Analyzer for the simulation results generated in 
#' "step3-extension.R" 
#' 
#' Author: mwelz
#' Last changed: Apr 7, 2021
#' ----------------------------------------------------------------
rm(list = ls()) ; cat("\014")

#' load simulation results; explanation of the objects:
#' panel 1 exposes spurious dependence (non-robust test signals dependence although there is none)
#' panel 2 exposes spurious independence (non-robust test signals independence although we have dependence)
load(paste0(getwd(), "/step3/extension/step3-simdata.Rdata"))

# take averages across the _m_ simulation runs to obtain average value of dCor
panel1.classic <- colMeans(dcor.arr.panel1)
panel1.robust  <- colMeans(dcor.arr.trans.panel1)
panel2.classic <- colMeans(dcor.arr.panel2)
panel2.robust  <- colMeans(dcor.arr.trans.panel2)


# plot the averages
pdf(file = paste0(getwd(), "/step3/extension/fig16-panel1-extended.pdf"))
plot(a.arr.panel1, panel1.classic, type = "l", xlab = "a", ylab = "dCor", 
     main = "dCor of Cauchy data with 1 outlier at (a,a)", 
     ylim = c(0,1), lty = 2,
     cex = 1.5, cex.lab = 1.8, cex.main = 1.8, cex.axis = 1.6)
lines(a.arr.panel1, panel1.robust, type = "l", col = "blue")
legend("topleft", legend = c("classic", "robust"),
       col=c("black", "blue"), lty = c(2,1), cex = 1.6)
dev.off()

pdf(file = paste0(getwd(), "/step3/extension/fig16-panel2-extended.pdf"))
plot(a.arr.panel2, panel2.classic, type = "l", xlab = "a", ylab = "dCor", 
     main = "dCor of linear data with 1 outlier at (a,0)", 
     ylim = c(0,1), lty = 2,
     cex = 1.5, cex.lab = 1.8, cex.main = 1.8, cex.axis = 1.6)
lines(a.arr.panel2, panel2.robust, type = "l", col = "blue")
legend("bottomleft", legend = c("classic", "robust"),
       col=c("black", "blue"), lty = c(2,1), cex = 1.6) 
dev.off()
