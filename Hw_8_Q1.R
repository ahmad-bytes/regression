n<-10

x<-0
y<-0
w<-0

# 50 sorted normal random variables
i <- c(1,2,3,4,5,6,7,8,9,10)

# set coefficients
x <- -3 + (6 * i / n)
z <- 1551

t <- 1
l <- 0
for(k in 1:100)
{
  # first derivative
  l<- z - sum(x * exp(t*x))
  # jacobian second derivative
  m<-matrix(nrow=1,ncol=1)
  # second  derivative
  m[1,1] <- - sum(x^2 * exp(t*x))
  t <- t - solve(m) %*% l
}

lambda <- sum(x * exp(as.vector(t) * x))

j <- solve(m) * sum(x^2 * exp(as.vector(t) * x)) * solve(m)



