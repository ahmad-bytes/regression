n <- 10
params <- 1
x <- 0
y <- 0
w <- matrix(nrow=n,ncol=n)
hh <- matrix(nrow=n,ncol=n)
ww <- matrix(nrow=n,ncol=n)


v <- 0
z <- 0
pp <- 0
bht <- 0

l <- 0
d <- 0
lt <- 0
kk <- 1

# x is data filled up of 100 normal random variables
x <- matrix(nrow=n,ncol=params)
x[,1]<- c(1:10) / 10


#calculate mean
bh <- -0.34
pp <- exp(x%*%bh)/(1+exp(x%*%bh))


v <- pp*(1-pp)
w <- diag(c(v))

var_b = solve( t(x) %*% w %*% x )

# y is a random binomial variable
y <- x * exp(x %*% bh)/(1 + exp(x%*%bh)) 

bz <- 0
ppt <- exp(x%*%bz)/(1+exp(x%*%bz))

# deviance calculation
l <- sum(y * log(pp/(1-pp))+log(1-pp))
lt <- sum(y * log(ppt/(1-ppt))+log(1-ppt))
d <- 2*(l-lt)

