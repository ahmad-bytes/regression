n<-100

l<-0
c<-0
x<-0
y<-0
t<-0
tt<-0
v1<-0
v2<-0
z<-0
zz<-0
v3<-0
yh<-0
eh<-0
yy<-0
xx<-0
q<-0
b<-0
ttt<-0
bb<-0
yh<-0
eh<-0

# x is uniform from 0-2
x<-2*runif(n)
# function is log 2x
y<-log(2+x)+0.1*rnorm(n)

# theta is 3 to begin with
t<-3

for(k in 2:100)
{
  # first derivative
	u<--sum(y/(t+x)-(log(t+x)/(t+x)))
	# second derivative
	d<-sum((1+y)/(t+x)^2-(log(t+x))/(t+x)^2 )
	t<-t-u/d
}

yh<-log(t+x)
eh<-y-yh

# boot strapping

for(kk in 1:100)
{
q<-c(1:n)
qq<-sample(q,replace=TRUE)
xx<-x[qq]
yy<-y[qq]
yyy<-log(t+x)+0.1*rnorm(n)

tt[1]<-3
ttt[1]<-3
for(k in 2:10)
{
	u<--sum(yy/(tt[k-1]+xx)-(log(tt[k-1]+xx)/(tt[k-1]+xx)))
	d<-sum((1+yy)/(tt[k-1]+xx)^2-(log(tt[k-1]+xx))/(tt[k-1]+xx)^2 )
	tt[k]<-tt[k-1]-u/d
	uu<--sum(yyy/(ttt[k-1]+x)-(log(ttt[k-1]+x)/(ttt[k-1]+x)))
	dd<-sum((1+yyy)/(ttt[k-1]+x)^2-(log(ttt[k-1]+x))/(ttt[k-1]+x)^2 )
	ttt[k]<-ttt[k-1]-uu/dd
	
}

b[kk]<-log(tt[10]+1)
bb[kk]<-log(ttt[10]+1)


}

sd(b)
sd(bb)



