# cross validation approach for LASSO

# number of data points
n<-20

# initialize
x<-0
y<-0

# coefficients
# a + bx
a<-4
b<-3

#bhat matrix
bh<-0

# temporary vectors to store values
bb<-0
bl<-0

# possibly used to eliminate 1 data point for cross validation
xvi<-matrix(nrow=n-1,ncol=2)
# possibly used to calculate y vector after eliminating ith data point
yi<-0

#values of least square over lambda
gg<-0

# 20 uniform numbers for x vector
x<-runif(n)

# initialize x matrix, n rows 2 columns
xv<-matrix(nrow=n,ncol=2)
# first column of x matrix
xv[,1]<-x
# second column is correlated with first column
xv[,2]<-x+0.01*runif(n)

# calculate y with error
y<-a*xv[,1]+b*xv[,2]+rnorm(n)

# initialize xx and xi
xx<-matrix(nrow=2,ncol=2)
xi<-matrix(nrow=2,ncol=2)

# calculate xx, xi and bhat
xx<-t(xv)%*%xv
xi<-solve(xx)

bh<-xi%*%t(xv)%*%y

m<-50

# do 20 iterations
mm<-20

for(i in 1:mm)
    {
    # how is lambda calculated
    la<-(i-1)/(2*mm)	
    r<-0
    for(ii in 1:n)
    {
        # drop -ii index from the data and setup the y and xvi vectors 
        # without the ii data point
        yi<-y[-ii]
        xvi[,1]<-xv[-ii,1]
        xvi[,2]<-xv[-ii,2]
        # what is q doing here?
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


