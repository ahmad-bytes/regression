n<-100
m<-0
x<-0
w<-0
d<-0
mh<-0
y<-0
kk<-100
r<-0
t<-0
b<-100
v<-0
db<-0
mhb<-0
xb<-0
yb<-0
ma<-0
va<-0
vv<-0

mhm<-matrix(nrow=n,ncol=kk)

for(i in 1:n)
{
x[i]<-i/n
y[i]<-sin(x[i]*2*pi)+0.1*rnorm(1)
}

for(k in 1:kk)
{
h<-k*0.1/kk
for(i in 1:n)
{
d[i]<-sum(exp(-0.5*(x-x[i])^2/h^2))
mh[i]<-(sum(y*exp(-0.5*(x-x[i])^2/h^2))-y[i])/(d[i]-1)
}
r[k]<-sum((y-mh)^2)
}

l<-which.min(r)
hh<-l*0.1/kk

for(i in 1:n)
{
	d[i]<-sum(exp(-0.5*(x-x[i])^2/hh^2))
  mh[i]<-sum(y*exp(-0.5*(x-x[i])^2/hh^2))/d[i]
}

for(i in 1:n)
{
	ma[i]<-0
	va[i]<-0
}

for(j in 1:b)
{

v<-sample(c(1:n),n,replace=TRUE)
v<-sort(v)
xb<-x[v]
yb<-y[v]

for(i in 1:n)
{
	db[i]<-sum(exp(-0.5*(xb-x[i])^2/hh^2))
    mhb[i]<-sum(yb*exp(-0.5*(xb-x[i])^2/hh^2))/db[i]
    ma[i]<-ma[i]+mhb[i]
    va[i]<-va[i]+mhb[i]^2
}


}


for(i in 1:n)
{
	vv[i]<-(va[i]-ma[i]^2/n)/(n-1)
}






