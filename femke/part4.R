# install.packages("energy")
library(energy)
# Part 4 # Femke

# significance level
alpha = 0.1
# number of replication keep m < 1000 
m = 500 # trial run 




# left --------------------------------------------------------------------
# sample size
n = seq(10,80,by=10)
# dimension
d = 2


results=matrix(0,length(n),m)
counters=matrix(0,8,1)
# loop over sample size
for(k in 1:8){
  # loop over replications
  counter=0
  for(j in 1:m){
    # create data for specific setting
    # samples of student t distribution df=2
    X=matrix(0,n[k],d)
    X[,1]= rt(n[k],df=2)
    Y=X
    for(i in 2:d){
      X[,i]= rt(n[k],df=2)
      Y[,i]= rt(n[k],df=2)
    }
    result=dcor.test(X,Y,R=10) 
    # asks for number of replications?
    # pvalue = NA? 
    # dcor(X,Y)
    results[k,j]=result[["p.value"]]
    
    if(result[["p.value"]] < alpha){ #reject
    counter=counter+1
    }
  }
  counters[k]=counter
  # # determine fraction rejecting the null hypothesis
  # results[k]= empow
}
empow=counters/m

# plotting empow in function of sample size
# and in function of dimension

# Try something different --------------------------------------------------

