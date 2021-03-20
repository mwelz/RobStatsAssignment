# Part 4 # Femke

# 1 degree of freedom -----------------------------------------------------
## Jakob Raymaekers Cauchy distribution

# Figure 8 in section 5.3 in [2] carries out a different experiment, where the power of
# the permutation test implemented in dcor.test is plotted as a function of the sample
# size n (left) and the dimension d (right).

# 2 degrees of freedom ----------------------------------------------------
## Analogue student t-distibutions 2 degrees of freedom

# number of replication keep m < 1000 
m = 3 # trial run
# sample size
n = seq(10,80,by=10)

# sample of student t distribution df=2
sample = rt(n,df=2)  #ncp non-centrality parameter omitted => central t distribution

# significance level
alpha = 0.1

# running dcor on sample
counter=0
# determin fraction rejecting the null hypothesis
empow=counter/n
# plotting in function of sample size
# and in function of dimension
