# read the file
HW2_data <- read.csv("C:/Users/bilal/Dropbox/MS/RPractice/HomeWork/HW2_data.csv")
#print summary so we can verify file is loaded
summary(HW2_data)

# initialize 
# ht<-0
# es<-0
# load the file as matrix
m = as.matrix(HW2_data)

# cut the y vector which is the first column
y <- m[,1]
# cut the x matrix which are columns 2 to 4
xx <- m[,2:4]

# get number of data points from xx, which are the rows in matrix
n = nrow(xx)
# get number of parameters from xx
p = ncol(xx)

#calculate xi which is transpose of Xx multiplied by xx and inverse (X`X)^-1
xi<-solve(t(xx) %*% xx)

#1.1a calculate beta hat, (X`X)^-1 * transpose X * y
bh<-xi %*% t(xx) %*% y
sprintf('1.1a Estimator of beta %f',bh)

#1.1b for distribution of beta 3 

# calculated yh using the estimated parameters
yh<-xx %*% bh
# calculate residual vector
eh <- y - yh
# calculate sigma
sigma <- (t(eh) %*% eh) / (n-p)

index = 3
# get element 3,3 or xi which is (X`X)^-1
cindex <- xi[index,index]
sprintf('1.1b Distribution of beta3 N(B3,sigma^2 * C3) = N(%f , %f)',bh[index,1], sigma * cindex )


#1.1c marginal distribution of beta 3 
# residual sum of squares transpose of eh * eh where eh is y - yh calculated before
resss = t(eh) %*% eh
# cut reduced x matrix by ignoring last column in the matrix
xxreduced <- m[,2:3]
# new number of q or parameters 
q = ncol(xxreduced)
# recalculate all again
# xi reduced
xireduced <- solve(t(xxreduced) %*% xxreduced)
# bh reduced
bhreduced <- xireduced %*% t(xxreduced) %*% y
# y reduced
yhreduced <- xxreduced %*% bhreduced
# calculate residual vector for reduced
ehreduced <- y - yhreduced
# calculate residual sum of square of reduced
resssreduced = t(ehreduced) %*% ehreduced
# calculate FStats 
# (difference in resiudual sum of squares between reduced and original / p-q)
# divide by
# (residual sum of squares between reduced and original / n-p)
Fstats = ((resssreduced - resss) / (p-q)) / ((resss) / (n-p))
df1 <- p-q
df2 <- n-p
Fsignificance <- 0.01
Fcriticalval <- qf(Fsignificance, df1, df2, lower.tail=FALSE)
#degree of freedom 1 : 1, 27, F stats 1.35, critical value: 7.674 - hypothesis is not rejected
sprintf('1.2 Test Statistic %f, Critical Value %f (hypothesis is not rejected))', Fstats, Fcriticalval)

#1.3 marginal distribution of b3 , index is 3 as initialized before
bindex <- bh[index,1]
# sigmas cancel out
# b3 divide by root of [3,3]rd element of (X`X)^-1 which is already calculated in cindex
# divide by
# residual sum of squares divide by  root of degrees of freedom
Tstats <- (bindex / (cindex)^0.5) / ((resss / (n-p))^0.5)
Tcriticalval <- qt(Tstats, n-p , lower.tail=FALSE)
# Tstats is 0.266 - p value is 0.39 - hypothesis is not rejected
# Tstats is 1.66 - p value is 0.39 - hypothesis is not rejected
sprintf('1.3 Test Statistic %f, P-Value %f (hypothesis is not rejected)) ', Tstats, Tcriticalval) 

#1.4 calculating leverage score
# calculate hat matrix -- x multiple by xi(x transpose * x and inverse) multiple by x matrix
ht <- xx %*% xi %*% t(xx)
# take the diagonal elements of hat matrix, which will give us the leverage scores
h11 <- ht[1,1]
h22 <- ht[2,2]
#h11 = 0.05, h22 = 0.10
#h22 is greater, high leverage is high influence
sprintf('1.4 Leverage score for first obs.= %f, Leverage score for Second obs.= %f. Second observation is more influential', h11, h22) 

#1.5 predict 
# set new x vector
xnew <- c(1,0.12,0.56)
# calculate ypred
ypred <- t(xnew) %*% bh
# we already have sigma which was unknown and we have estimated earlier
#t = 0.025 at 27 confidence interval
talpha = 2.052
interval <- talpha * (sigma * t(xnew) %*% xi %*% xnew)^0.5
sprintf('1.5 predicted value %f - confidence interval (%f, %f)', ypred, ypred - interval,ypred + interval)
