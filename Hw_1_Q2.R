x <- c(1.1, 0.3, 1, 0.75, 1.8)     
y <- c(4.5, 2,   5, 2.4 , 4.8) 
n=5

xbar <- mean(x)
ybar <- mean(y)

sx <- sum(x^2)
sy <- sum(y^2)
sXiSquared <- sum((x - xbar)^2)

bh <- (sum(x*y) - n*xbar*ybar)/(sx - n*xbar^2)
ah <- ybar - bh*xbar              

#2.a

#from 1.c
sigmah <- (sum((y - ah - bh*x) ^ 2)) / (n-2)
sigmahroot <- sigmah ^ 0.5

chisquared = (sum((y - ah - bh*x) ^ 2)) / sigmah
numerator <- bh * (sXiSquared / sigmahroot)^0.5
denominator <- chisquared / (n-2)
sprintf('2.a Chisquared value is %f', numerator / denominator)

#2.b e1 = y1 - Yhat1
index = 1
Varei = (1 - 1/n - ((x[index]-xbar)^2/sXiSquared))
sprintf('2.b Vare1 is %f', Varei)

#2.c studentized residual for first observation
index = 1
e1 = y[index] - ah - bh * x[index]
studentizede1 = e1 / (sigmahroot * Varei^0.5)
sprintf('2.c studentized e1 is %f', studentizede1)

#2.d maximum influence
hii <- 1/n + ((x-xbar)^2/sXiSquared)
valii <- which.max(hii)
sprintf('2.d highest influence is %f at index %d', hii[valii] , valii )

#2.e confidence interval
xinput <- 0.5
ypred <- ah + bh * xinput

# include x to Sxx
newsigmahroot <- 0.8^0.5
interval <- newsigmahroot * (1/n + ((xinput-xbar)^2/sXiSquared))^0.5 * qnorm(0.025,lower.tail = FALSE)
sprintf('2.e Range is %f and %f', ypred - interval, ypred + interval)


plot(x,y)
abline(a=ah, b=bh)
abline(a=ah - interval, b=bh,  col="blue")
abline(a=ah + interval, b=bh,  col="blue")

