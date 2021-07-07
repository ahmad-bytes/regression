n<-100
y<-0
v<-0.01

b1<-0
b2<-0.5

q<-0.1
s<-1

t1<-0
t2<-3

z1<-1
z2<-1

x1<-0
x2<-0
x1<-rnorm(n)
x2<-rnorm(n)
y<-b1*x1+b2*x2+s*rnorm(n)

r<-sum(x1*x2)
r1<-sum(y*x1)
r2<-sum(y*x2)
xx1<-sum(x1^2)
xx2<-sum(x2^2)
zz1<-0
zz2<-0
tt1<-0
tt2<-0

for(k in 1:1000)
{

if(z1==0)
{
	t1=0
}
else
{
	t1<-(r1-r*z2*t2)/(v+xx1)+rnorm(1)/sqrt(v+xx1)
}
if(z2==0)
{
	t2=0
}
else
{
	t2<-(r2-r*z1*t1)/(v+xx2)+rnorm(1)/sqrt(v+xx2)
}

f1<-sum(-0.5*(y-x1*t1-x2*t2*z2)^2/s^2)
f2<-sum(-0.5*(y-x2*t2*z2)^2/s^2)
p<-exp(f1)*dnorm(t1,0,1/sqrt(v))/(exp(f1)*dnorm(t1,0,1/sqrt(v))+exp(f2))
u<-runif(1)
if(u<p)
{
	z1=1
}
else
{
	z1=0
}

f1<-sum(-0.5*(y-x1*t1*z1-x2*t2)^2/s^2)
f2<-sum(-0.5*(y-x1*t1*z1)^2/s^2)
p<-exp(f1)*dnorm(t1,0,1/sqrt(v))/(exp(f1)*dnorm(t1,0,1/sqrt(v))+exp(f2))
u<-runif(1)
if(u<p)
{
	z2=1
}
else
{
	z2=0
}

zz1[k]<-z1
zz2[k]<-z2
tt1[k]<-t1
tt2[k]<-t2


}











