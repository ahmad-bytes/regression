# read the file
HW7_data <- read.csv("C:/Users/bilal/Dropbox/MS/RPractice/HomeWork/HW7_data.csv")
#print summary so we can verify file is loaded
summary(HW7_data)

class(HW7_data)

# function is y = e(tx)
# first derivative is te(tx)
# second derivative is t^2e(tx)

# cut x and y vectors from data

y <- 0
xx <- 0

#m = as.matrix(HW7_data)
# cut the y vector which is the first column
#y <- m[,1]
# cut the x matrix which is the second column
#xx <- m[,2]

y <- HW7_data$y
xx <- HW7_data$x


n = length(y)

t <- 1
u <- 0
d <- 0

for(k in 2:10)
{
  # numerator
  u <- sum(xx * exp(t * xx) * (exp(t * xx) - y))
  # denominator
  d <- sum(xx^2 * exp(t * xx) * (2 * exp(t * xx) - y))
  t <- t - u/d
}


yh<-exp(t * xx)
eh <- y - yh

sigma_squared = sum ((y - yh)^2) / (n-1)
sigma = sqrt(sigma_squared)


ttt <- 0
bbb <- 0
yyy <- 0

xxx <- xx
for(kk in 1:1000)
{
  yyy <- exp(t * xx) + (sigma * rnorm(n))
  ttt <- 1
  
  for(k in 2:10)
  {
    # numerator
    u <- sum(2 * xx * exp(ttt * xx) * (exp(ttt * xx) - yyy))
    # denominator
    d <- sum(2 * xx^2 * exp(ttt * xx) * (2 * exp(ttt * xx) - yyy))
    ttt <- ttt - u/d
  }
  bbb[kk] <- ttt
}

var(bbb, na.rm = TRUE)
summary(bbb)

