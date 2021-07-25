n<-50

x<-0
y<-0
w<-0

# 50 sorted normal random variables
x<-rnorm(n)
x<-sort(x)

# set coefficients
a<-0.5
b<--1

calculate 
y<-rpois(n,exp(a+b*x))

yb<-mean(y)
z<-sum(x*y)

l<-0
u<-0
u[1]<-a
u[2]<-b

for(k in 1:10)
{

l[1]<-n*yb-exp(a)*sum(exp(b*x))
l[2]<-z-exp(a)*sum(x*exp(b*x))

m<-matrix(nrow=2,ncol=2)
m[1,1]<--exp(a)*sum(exp(b*x))
m[2,2]<--exp(a)*sum(x^2*exp(b*x))
m[1,2]<--exp(a)*sum(x*exp(b*x))
m[2,1]<-m[1,2]

u<-u-solve(m)%*%l

a<-u[1]
b<-u[2]

}
ah<-a
bh<-b

tt<-0
tt<-exp(ah+bh*x)
w<-matrix(nrow=2,ncol=2)
w[1,1]<-sum(tt)
w[2,2]<-sum(x^2*tt)
w[1,2]<-sum(x*tt)
w[2,1]<-w[1,2]

c<-matrix(nrow=2,ncol=2)
c<-solve(m)%*%w%*%solve(m)




