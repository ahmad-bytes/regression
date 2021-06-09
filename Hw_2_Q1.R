HW2_data <- read.csv("C:/Users/bilal/Dropbox/MS/RPractice/HomeWork/HW2_data.csv")
summary(HW2_data)

ht<-0
es<-0
hd<-0

m = as.matrix(HW2_data)
print(m)

# get the y vector
y <- m[,1]
#print(y)

# get the x vector
xx <- m[,2:4]
#print(x)

n = nrow(xx)
p = ncol(xx)


#this is transpose of X multiplied by x and inverse
xi<-solve(t(xx) %*% xx)
#print(xi)

#this Beta Hat, the hat matrix multiplied by y matrix
bh<-xi %*%t (xx) %*% y
#1.1a
print(bh)

# for distribution of beta 3 
index = 3
#View(xi)
cindex <- xi[index,index]
#1.1b
print(cindex)

# calculated yh using the estimated parameters
yh<-xx %*% bh
#print(yh)

# calculate residual vector
eh <- y - yh
#print(eh)
ehTeh = t(eh) %*% eh

# lets calculate sigma, we need this for 
# 1/(n-p) * e
sigma <- (t(eh) %*% eh) / (n-p)
print(sigma)

xxreduced <- m[,2:3]
q = ncol(xxreduced)
xireduced <- solve(t(xxreduced) %*% xxreduced)
bhreduced <- xireduced %*% t(xxreduced) %*% y
yhreduced <- xxreduced %*% bhreduced
ehreduced <- y - yhreduced
ehreducedTehreduced = t(ehreduced) %*% ehreduced
Fstats = ((ehreducedTehreduced - ehTeh) / (p-q)) / ((ehTeh) / (n-p))
df1 = p-q
df2 = n-p
#degree of freedom 1 : 1, 27, F stats 1.35, critical value: 7.674 - hypothesis is not rejected

#marginal distribution of b3
#1.3
bindex <- bh[index,1]
Tstats <- (bindex / (cindex)^0.5) / (ehTeh / (n-p)^0.5)
# Tstats is 0.266 
# p value is 0.39 - hypothesis is not rejected

#calculating leverage score

# calculate hat matrix -- x multiple by xi(x transpose * x and inverse) multiple by x matrix
ht <- xx %*% xi %*% t(xx)
h11 <- ht[1,1]
h22 <- ht[2,2]

# take the diagonal elements of hat matrix, which will give us the leverage scores
#hd<-diag(ht)
#print(hd)
#h11 = 0.05, h22 = 0.10
#h22 is greater, high leverage is high influence







# this will give the studentized residual matrix
for(i in 1:n)
{
  es[i] <- eh[i]/sqrt(1-hd[i])
}

#print(es)

