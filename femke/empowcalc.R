# install.packages("energy")
library(energy)

# Femke Verreycken

# significance level
alpha = 0.1
# number of replication keep m < 1000 
m = 500#3 # 500

# Equation 25
g <- function(x){ # As implemented by Max
  mu    <- median(x)
  sigma <- mad(x)
  return(mu + sigma * tanh((x - mu) / sigma))
}

# calculation of empow for given dim and sample size
empowcalc = function(dim, ssize, m, alpha=0.1, robust=FALSE, df=2){
  # manual debug after running into problem
  #dim=2 
  #ssize=n
  counters=matrix(0,length(ssize),length(dim))
  for(i in 1:length(ssize)){ # loop over sample size
    # i=1
    n = ssize[i]
    R = floor(200 + 5000/n)
    for(j in 1:length(dim)){ # loop over dimensions
      # j=1
      d = dim[j]
      counter=0
      for(k in 1:m){ # loop over replications
        # k=1
        # samples of student t distribution df=2
        X=matrix(0,n,d)
        X[,1]= rt(n,df=df)
        Y=X
        if(d > 1){
          for(i in 2:d){
            X[,i]= rt(n,df=df)
            Y[,i]= rt(n,df=df)
          }
        }
        if(robust==TRUE){
          X=g(X)
          Y=g(Y)
        }
        result=dcor.test(X,Y,R=R)
        pValue=result[["p.value"]]
        # results[k,j]=result
        if(pValue < alpha){ #reject
          counter=counter+1
        }
      }
      counters[i,j]=counter/m
    }
  }    
  # determine fraction rejecting the null hypothesis
  return(counters)
}

  
# data set 1a: power in func. of samplesize
# sample size
n = seq(10,80,by=10)
# dimension
d = 2
empow1a=empowcalc(d,n,m)




# plotting empow in function of sample size
# and in function of dimension

