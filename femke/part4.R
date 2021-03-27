install.packages("energy")
library(energy)
# Part 4 # Femke
# significance level
alpha = 0.1
# number of replication keep m < 1000 
m = 3 # trial run
# sample size
n = 10 #seq(10,80,by=10)
# dimension
d = 2 #seq(1,30,by=1)

#counter=0
#for(j in m){
X=zeros(d,n)
Y=zeros(d,n)
X[1]= rt(n,df=2)
Y[1]=X[1]
# sample of student t distribution df=2
for(i in 2:d){
  X[i]= rt(n,df=2)
  Y[i]= rt(n,df=2)
}



# running dcor on sample
counter=0
# determin fraction rejecting the null hypothesis
empow=counter/n
# plotting in function of sample size
# and in function of dimension
