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

zi <- 0.01
zi_denom <- 1 + zi

# get number of data points from xx, which are the rows in matrix
n = nrow(xx)
# get number of parameters from xx
p = ncol(xx)

#calculate xi which is transpose of Xx multiplied by xx and inverse (X`X)^-1
xi<-solve(t(xx) %*% xx)

#1.1a calculate beta hat, (X`X)^-1 * transpose X * y
bh<-xi %*% t(xx) %*% y

bh_bayes <- bh / zi_denom


sprintf('1.1a Estimator of beta %f',bh_bayes)

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

T_bayes <- (t(bh) %*% t(xx) %*% xx %*% bh)  - (t(bhreduced) %*% t(xxreduced) %*% xxreduced %*% bhreduced)
kappa <- -log(zi/zi_denom)

logB01 = (T_bayes/(2 * zi_denom)) - kappa/2

#1.5 predict 
# set new x vector
xnew <- c(1,0.12,0.56)

y_predict_bar <- (xnew %*% bh_bayes)


variance = (1 + (t(xnew) %*% xi %*% xnew) / zi_denom)


