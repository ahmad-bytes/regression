n<-500

v<-10
s<-100

x1<-0
x2<-0
x3<-0
x1<-rnorm(n)
x2<-rnorm(n)
x3<-rnorm(n)
y<-0
p<-0
bh<-0
b<-0
bc<-0
bb<-0
b1<-0
b2<-0
b3<-0

x<-matrix(nrow=n,ncol=3)
x[,1]<-x1
x[,2]<-x2
x[,3]<-x3

b[1]<-2
b[2]<-0
b[3]<--1

p<-exp(x%*%b)/(1+exp(x%*%b))

bc<-c(0,0,0)

y<-rbinom(n,1,p)

for(k in 1:1000)
{

bh<-bc+v*rnorm(3)
lau<--0.5*sum(bh^2)/s
lad<--0.5*sum(bc^2)/s
for(i in 1:n)
{
lau<-lau+y[i]*x[i,]%*%bh-log(1+exp(x[i,]%*%bh))
lad<-lad+y[i]*x[i,]%*%bc-log(1+exp(x[i,]%*%bc))
}
la<-lau-lad

if(log(runif(1))<la)
{
	bc<-bh
}

b1[k]<-bc[1]
b2[k]<-bc[2]
b3[k]<-bc[3]

}





