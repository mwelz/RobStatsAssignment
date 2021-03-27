install.packages("energy")
library(energy)
# Part 4 # Femke

# significance level
alpha = 0.1
# number of replication keep m < 1000 
m = 3 # trial run 




# left --------------------------------------------------------------------
# sample size
n = seq(10,80,by=10)
# dimension
d = 2


results=matrix(0,length(n),m)
# results=rep(0,k)
# loop over sample size
for(k in 1:8){
  # loop over replications
  for(j in 1:m){
    #create data for specific setting
    # samples of student t distribution df=2
    X=matrix(0,d,n[k])
    X[1,]= rt(n[k],df=2)
    Y=X
    for(i in 2:d){
      X[i,]= rt(n[k],df=2)
      Y[i,]= rt(n[k],df=2)
    }
    result=dcor.test(X,Y,m) 
    # asks for number of replications?
    # pvalue = NA? 
    # dcor(X,Y)
    results[k,j]=result[["p.value"]]
    
    # if(result<alpha){
    # counter=counter+1
    # }
  }
  # # determine fraction rejecting the null hypothesis
  # empow=counter/n
  # results[k]= empow
  }



# plotting empow in function of sample size
# and in function of dimension
