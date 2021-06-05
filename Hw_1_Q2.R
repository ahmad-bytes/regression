
x <- c(1.1, 0.3, 1, 0.75, 1.8)     
y <- c(4.5, 2,   5, 2.4 , 4.8) 
n=5

xbar <- mean(x)
ybar <- mean(y)

sx <- sum(x^2)
sy <- sum(y^2)
sXiSquared <- sum((x - xbar)^2)

bh <- (n*sum(x*y) - n^2*xbar*ybar)/(n*sx - n^2*xbar^2)
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
Varei = (1 - 1/n - ((1.1-xbar)^2/sXiSquared))
sprintf('2.b Vare1 is %f', Varei)


#2.c studentized residual for first observation
e1 = 4.5 - ah - bh * 1.1
studentizede1 = e1 / (sigmahroot * Varei^0.5)
sprintf('2.c studentized e1 is %f', studentizede1)

#2.d maximum influence
h11 = 1/n + ((1.1-xbar)^2/sXiSquared)
h22 = 1/n + ((0.3-xbar)^2/sXiSquared)
h33 = 1/n + ((1-xbar)^2/sXiSquared)
h44 = 1/n + ((0.75-xbar)^2/sXiSquared)
h55 = 1/n + ((1.8-xbar)^2/sXiSquared)

sprintf('2.d highest influence is %f', h55)

#2.e confidence interval
xinput <- 0.5
ypred <- ah + bh * xinput
interval <- sigmahroot * (1/n + ((xinput)^2/sx))^0.5 * 1.645

sprintf('2.e Range is %f and %f', ypred - interval, ypred + interval)


