p<-3
n<-4

#p<-5
#n<-100

xx<-0
xi<-0
bh<-0
y<-0
yh<-0
eh<-0
rs<-0
t<-0
f<-0
m<-100
ht<-0
es<-0
hd<-0
l<-0

b<-c(2,-1,3)

xx<-matrix(nrow=n,ncol=p)
xi<-matrix(nrow=p,ncol=p)
ht<-matrix(nrow=n,ncol=n)

# initialize x vector with uniform random numbers betweeb 0 and 1
for(i in 1:n)
{
  for(j in 1:p)
  {
    xx[i,j]<-runif(1)
  } 
}

print(xx)

#calculate y
for(i in 1:n)
{
  # here we are multiplying ith row of x with vector B and adding an error term which is a normal random variable
  y[i]<-xx[i,] %*% b + rnorm(1)
}

print(y)

#this is transpose of X multiplied by x and inverse
xi<-solve(t(xx)%*%xx)

print(xi)

#this Beta Hat, the hat matrix multipled by y matrix
bh<-xi %*%t (xx) %*% y

print(bh)

# calculated yh using the estimated paremetere
yh<-xx %*% bh

print(yh)

# calculate residual vector
eh <- y-yh

print(eh)

# calculate hat matrix -- x multiple by xi(x transpose * x and inverse) multiple by x matrix
ht <- xx %*% xi %*% t(xx)

# take the diagonal elements of hat matrix, which will give us the leverage scores
hd<-diag(ht)

print(hd)

# this will give the studetized residual matrix
for(i in 1:n)
{
  es[i] <- eh[i]/sqrt(1-hd[i])
}

print(es)



