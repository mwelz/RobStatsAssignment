n=100000

## quadratic case
Xn=rnorm(n)
XnSquared=Xn**2
cor(Xn,XnSquared)
energy::dcor2d(Xn,XnSquared)
plot(Xn,XnSquared)


##absolute value
XnAbs=abs(Xn)
cor(Xn,XnAbs)
energy::dcor2d(Xn,XnAbs)
plot(Xn,XnAbs)

##Upper haf of a circle
XnCirc=runif(n,-1,1)
YnCirc=sqrt(1-XnCirc**2)
cor(XnCirc,YnCirc)
energy::dcor2d(XnCirc,YnCirc)
plot(XnCirc,YnCirc)