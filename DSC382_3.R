n<-100
x1<-0
x2<-0
y<-0
b1<-1
b2<--3
b3<-0
eh<-0
yh<-0
bh<-0
seh<-0
bhh<-0
yhh<-0
ehh<-0
f<-0

xx<-matrix(nrow=n,ncol=2)
xxx<-matrix(nrow=n,ncol=3)
xxi<-matrix(nrow=3,ncol=3)


x1<-2+rnorm(n)
x2<-rnorm(n)


y<-b1*x1+b2*x2+b3*x1*x2+rnorm(n)

xx[,1]<-x1
xx[,2]<-x2

bh<-solve(t(xx)%*%xx)%*%t(xx)%*%y
yh<-xx%*%bh
eh<-y-yh


xxx[,1]<-x1
xxx[,2]<-x2
xxx[,3]<-x1*x2
xxi<-solve(t(xxx)%*%xxx)
bhh<-xxi%*%t(xxx)%*%y
yhh<-xxx%*%bhh
ehh<-y-yhh


f<-(n-3)*sum((y-yh)^2-(y-yhh)^2)/sum((y-yhh)^2)



