
x <- c(1.1, 0.3, 1, 0.75, 1.8)     
y <- c(4.5, 2,   5, 2.4 , 4.8) 
n=5


#1.a
sprintf('1.a Correlation between x and y is %f', cor(x,y))

#1.b

xbar <- mean(x)
ybar <- mean(y)

sx <- sum(x^2)
sy <- sum(y^2)

bh <- (n*sum(x*y) - n^2*xbar*ybar)/(n*sx - n^2*xbar^2)
ah <- ybar - bh*xbar              

sprintf('1.b yh = %f + %fx',ah, bh)
plot(x,y)
abline(a=ah, b=bh)

#1.c
sigmah <- (sum((y - ah - bh*x) ^ 2)) / (n-2)
sprintf('1.c sigmah = %f',sigmah)

#1.d
#distribution of Normal
#sigmasquare / (sum((xi-Xbar)^2)
sprintf('1.d Normal distribution with mean b and variance signa^2/%f', sum((x - xbar)^2))

#1.e
variance <- 0.8
sd <- (variance)^0.5
multiple <- (1/sum((x - xbar)^2))^0.5
z <- bh/( sd * multiple)
pval <- 2 * pnorm(z,lower.tail = FALSE)
sprintf('1.e Z value = %f p-value = %f', z, pval)

