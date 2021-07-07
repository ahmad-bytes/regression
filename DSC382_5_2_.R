n<-100
x<-0
y<-0
p<-2
z<-0

# coefficients
a<-1
b<-3

bh<-0

bb<-0
# possibly not used
bl<-0
xvi<-matrix(nrow=n-1,ncol=2)
yi<-0

gg<-0
qqq<-0

yh<-0
eh<-0

qqq<-0
bbb<-0
yhh<-0
ehh<-0

# this is svd decomposition of p
pp<-matrix(nrow=n,ncol=p)
qq<-matrix(nrow=p,ncol=p)
dd<-matrix(nrow=n,ncol=p)


for(i in 1:n)
{
	for(j in 1:p)
	{
		dd[i,j]<-0
	}
}
d<-0

# initialization of X and Y vectors
x<-runif(n)
xv<-matrix(nrow=n,ncol=2)
xv[,1]<-x
xv[,2]<-x+0.2*runif(n)
y<-a*xv[,1]+b*xv[,2]+rnorm(n)


xx<-matrix(nrow=2,ncol=2)
xx1<-matrix(nrow=2,ncol=2)
xi<-matrix(nrow=2,ncol=2)

xx1[1,1]<-n
xx1[1,2]<-n*mean(x)
xx1[2,1]<-xx1[1,2]
xx1[2,2]<-sum(x^2)


xx<-t(xv)%*%xv

xi<-solve(xx)

bh<-xi%*%t(xv)%*%y

pp<-t(svd(xv,nu=n,nv=p)$u)
qq<-t(svd(xv,nu=n,nv=p)$v)
d<-svd(xv,nu=n,nv=p)$d

for(i in 1:p)
{
	dd[i,i]<-d[i]
}	

gg<-0
z<-pp%*%y

# calculating gammas
for(j in 1:p)
{
gg[j]<-z[j]/d[j]
}

# beta estimator from pca
bb<-t(qq)%*%gg

# calculate y using beta pca
yh<-xv%*%bb
# error residual using beta pca
eh<-y-yh


qqq<-qq[1,]
bbb<-qqq*gg[1]

yhh<-xv%*%bbb

ehh<-y-yhh





