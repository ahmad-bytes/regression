n <- 100
xbar <- 5
variance <- 1


bhatvector <- replicate(1000, {
  x <- rnorm(n,xbar,variance)
  xbarCalc <- mean(x)
  y <- 10 + 0.3*(x-xbarCalc) + rnorm(n)
  ybar <- mean(y)
  sx <- sum(x^2)
  bhatvector <- (sum(x*y) - n * xbarCalc * ybar)/( sx - n * xbarCalc^2)
})

hist(bhatvector)


mean(bhatvector)
sd(bhatvector)


x <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
y <- c(20.66, 20.50, 13.16, 10.09, 7.44, -0.80, -1.55, -7.23, -12.31, -13.47, -16.58)
n <- length(x)

x.bar = mean(x)
y.bar = mean(y)

# Page 20 (Lecture 1)
b.hat <- (sum(x*y)-n*x.bar*y.bar)/(sum(x*x)-n*x.bar^2)
a.hat <- y.bar-b.hat*x.bar
y.hat <- b.hat*x+a.hat

alpha = 0.05

# Critical Value
c_alpha_2 <- qnorm(1-alpha/2)

# for the sake of plotting the lines, choosing sigma
# Page 60 (Lecture 1)
sigma = 2
xs <- seq(min(x), max(x), length = 200)
y.upper = (b.hat*xs+a.hat)+sigma*c_alpha_2*sqrt(1/n+xs^2/sum(x^2))
y.lower = (b.hat*xs+a.hat)-sigma*c_alpha_2*sqrt(1/n+xs^2/sum(x^2))

plot(x,y,pch=19,xlim=c(-6,6),ylim=c(-24,24))
lines(x,y.hat,lwd=2,col='blue')
lines(xs,y.upper,lty=5,col='red',lwd=2)
lines(xs,y.lower,lty=5,col='green',lwd=2)
legend('bottomleft',legend=c('Data',
                             'Prediction',
                             'Prediction: Upper Band',
                             'Prediction: Lower Band'),
       col = c('black','blue','red','green'),
       pch = c(19,NA,NA,NA),
       lty = c(0,1,5,5),
       lwd=2
)