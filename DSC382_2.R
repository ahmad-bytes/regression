p<-5
n<-100
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
b<-c(2,-1,3,-2,0)
xx<-matrix(nrow=n,ncol=p)
xi<-matrix(nrow=p,ncol=p)
ht<-matrix(nrow=n,ncol=n)


for(i in 1:n)
{
	for(j in 1:p)
	{
		xx[i,j]<-runif(1)
	} 
}

for(i in 1:n)
{
	y[i]<-xx[i,]%*%b+rnorm(1)
}

xi<-solve(t(xx)%*%xx)
bh<-xi%*%t(xx)%*%y
yh<-xx%*%bh
eh<-y-yh


ht<-xx%*%xi%*%t(xx)
hd<-diag(ht)

for(i in 1:n)
{
es[i]<-eh[i]/sqrt(1-hd[i])
}




