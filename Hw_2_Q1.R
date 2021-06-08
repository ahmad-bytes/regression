HW2_data <- read.csv("C:/Users/bilal/Dropbox/MS/RPractice/HomeWork/HW2_data.csv")
summary(HW2_data)

ht<-0
es<-0
hd<-0

m = as.matrix(HW2_data)
print(m)

n = nrow(m)

# get the y vector
y <- m[,1]
#print(y)

# get the x vector
xx <- m[,2:4]
#print(x)


#this is transpose of X multiplied by x and inverse
xi<-solve(t(xx) %*% xx)
#print(xi)

#this Beta Hat, the hat matrix multiplied by y matrix
bh<-xi %*%t (xx) %*% y
print(bh)

# for distribution of beta 3 
index = 3
View(xi)
c3 <- xi[index,index]
print(c3)


# calculated yh using the estimated parameters
yh<-xx %*% bh
#print(yh)

# calculate residual vector
eh <- y - yh

#print(eh)

# calculate hat matrix -- x multiple by xi(x transpose * x and inverse) multiple by x matrix
ht <- xx %*% xi %*% t(xx)

# take the diagonal elements of hat matrix, which will give us the leverage scores
hd<-diag(ht)

#print(hd)

# this will give the studetized residual matrix
for(i in 1:n)
{
  es[i] <- eh[i]/sqrt(1-hd[i])
}

#print(es)

