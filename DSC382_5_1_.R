n<-20
x<-0
y<-0

a<-4
b<-3
bh<-0
bb<-0
bl<-0
xvi<-matrix(nrow=n-1,ncol=2)
yi<-0
gg<-0

x<-runif(n)

xv<-matrix(nrow=n,ncol=2)
xv[,1]<-x
xv[,2]<-x+0.01*runif(n)

y<-a*xv[,1]+b*xv[,2]+rnorm(n)


xx<-matrix(nrow=2,ncol=2)
xi<-matrix(nrow=2,ncol=2)

xx<-t(xv)%*%xv
xi<-solve(xx)

bh<-xi%*%t(xv)%*%y

m<-50

mm<-20
for(i in 1:mm)
{
la<-(i-1)/(2*mm)	
r<-0
for(ii in 1:n)
{
yi<-y[-ii]
xvi[,1]<-xv[-ii,1]
xvi[,2]<-xv[-ii,2]

q<-exp(20)
for(j in 1:m)
{
for(k in 1:m)
{
	bb[1]<-bh[1]*(j-1)/m
	bb[2]<-bh[2]*(k-1)/m
	f<-t(yi-xvi%*%bb)%*%(yi-xvi%*%bb)/n+la*(abs(bb[1])+abs(bb[2]))
    if(f<q)
    {
    	q=f
    	bl[1]<-bb[1]
    	bl[2]<-bb[2]
    }
}
}

r<-r+(y[ii]-xv[ii,1]*bl[1]-xv[ii,2]*bl[2])^2
}

gg[i]<-r

}


