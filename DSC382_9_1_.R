n<-100
x<-0
y<-0
b<-0
p<-0
w<-matrix(nrow=n,ncol=n)
hh<-matrix(nrow=n,ncol=n)
ww<-matrix(nrow=n,ncol=n)
cc<-matrix(nrow=3,ncol=3)
v<-0
z<-0
pp<-0
bh1<-0
bh2<-0
bh3<-0
bht<-0
l<-0
d<-0
lt<-0
kk<-1

x1<-0
x2<-0
x3<-0
x1<-rnorm(n)
x2<-rnorm(n)
x3<-rnorm(n)

# x is data filled up of 100 normal random variables
x<-matrix(nrow=n,ncol=3)
x[,1]<-x1
x[,2]<-x2
x[,3]<-x3

# true values of beta ,2,1 and 0
b[1]<-2
b[2]<-1
b[3]<-0


# exp (xB)/ 1 + exp(xB), this is mean as well,
#we really don't need this, this is just to generate y
p<-exp(x%*%b)/(1+exp(x%*%b))

# y is a random binomial variable
y<-rbinom(n,1,p)

# estimators for beta
bh<-c(0,0,0)
bht<-c(0,0,0)

for(k in 1:40)
{
  #calculate mean
	pp<-exp(x%*%bh)/(1+exp(x%*%bh))
	#calculate z which is calculated values for y for given B xB + y - pp / pp (1-pp)
	z<-x%*%bh+(y-pp)/(pp*(1-pp))
	# construct diagonal matrix for weights
	v<-pp*(1-pp)
	w<-diag(c(v))
	# calculate beta for next iteration
	bh<-solve(t(x)%*%w%*%x)%*%t(x)%*%w%*%z

}


# set beta 3 to 0 is each iteration
for(k in 1:40)
{
	ppt<-exp(x%*%bht)/(1+exp(x%*%bht))
	z<-x%*%bht+(y-ppt)/(ppt*(1-ppt))
	v<-ppt*(1-ppt)
	w<-diag(c(v))
	bht<-solve(t(x)%*%w%*%x)%*%t(x)%*%w%*%z
  bht[3]<-0
}

# deviance calculation
l<-sum(y*log(pp/(1-pp))+log(1-pp))
lt<-sum(y*log(ppt/(1-ppt))+log(1-ppt))
d<-2*(l-lt)





