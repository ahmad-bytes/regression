# read the file
HW5_data <- read.csv("C:/Users/bilal/Dropbox/MS/RPractice/HomeWork/HW5_data.csv")
#print summary so we can verify file is loaded
summary(HW5_data)

# initialize 
# ht<-0
# es<-0
# load the file as matrix
m = as.matrix(HW5_data)

# cut the y vector which is the first column
y <- m[,1]
# cut the x matrix which are columns 2 to 4
xx <- m[,2:6]

pp<-matrix(nrow=n,ncol=n)
qq<-matrix(nrow=p,ncol=p)
dd<-matrix(nrow=n,ncol=p)

for(i in 1:n)
{
  for(j in 1:p)
  {
    dd[i,j]<-0
  }
}

bb<-0
gg<-0

# get number of data points from xx, which are the rows in matrix
n = nrow(xx)
# get number of parameters from xx
p = ncol(xx)

#calculate xi which is transpose of Xx multiplied by xx and inverse (X`X)^-1
xv<-t(xx) %*% xx
xi<-solve(xv)

#find svd for xx
pp<-t(svd(xx,nu=n,nv=p)$u)
qq<-t(svd(xx,nu=n,nv=p)$v)
d<-svd(xx,nu=n,nv=p)$d

# put d in a matrix (d is a vector)
for(i in 1:p)
{
  dd[i,i]<-d[i]
}	

z<-pp%*%y

# calculating gammas
for(j in 1:p)
{
  gg[j]<-z[j]/d[j]
}

# beta estimator from pca
bb<-t(qq)%*%gg

sum_ds = sum( d ^ 2)
m = 0
for(j in 1:p)
{
  temp <- sum(d[1:j]^2)
  print(sprintf('temp %f' , temp))
  if (temp / sum_ds > 0.9)
  {
    m = j
    break
  }
}

# m rows of Q transpose and gamma 

qqq<-qq[1:m,]
bbb<-t(qqq) %*% gg[1:m]

