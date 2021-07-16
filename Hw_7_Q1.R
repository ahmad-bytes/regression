# read the file
HW7_data <- read.csv("C:/Users/bilal/Dropbox/MS/RPractice/HomeWork/HW7_data.csv")
#print summary so we can verify file is loaded
summary(HW7_data)

# function is y = e(tx)
# first derivative is te(tx)
# second derivative is t^2e(tx)

# cut x and y vectors from data

y <- 0
xx <- 0

m = as.matrix(HW7_data)
# cut the y vector which is the first column
y <- m[,1]
# cut the x matrix which is the second column
xx <- m[,2]

n = length(xx)


l_theta <- function (theta, x_i)
{
  l_theta <- as.vector(exp(theta * x_i))
}


l_theta_dash <- function (theta, x_i)
{
  l_theta_dash  <- as.vector(theta * exp(theta * x_i))
}

l_theta_dash_dash <- function (theta, x_i)
{
  l_theta_dash_dash <- as.vector(theta^2 * exp(theta * x_i))
}

t <- 1

for(k in 2:100)
{
  # numerator
  u <- sum((y - l_theta(t, xx)) %*% l_theta_dash(t,xx))
  # denominator
  d <- sum((y %*% l_theta_dash_dash(t, xx)) - (l_theta_dash(t,xx))^2 - (l_theta(t,xx) %*% l_theta_dash_dash(t,xx)))
  t <- t - u/d
}

yh<-l_theta(t, xx)

sigma_squared = (sum(y - yh)^2) / (n-1)
sigma = sqrt(sigma_squared)
sigma_theta = sigma_squared * sum (l_theta_dash(t, xx)^2)/  sum(l_theta_dash_dash(t,xx)^2)

ttt <- 0
bbb <- 0
yyy <- 0
xxx <- 0

xxx <- xx
for(kk in 1:1000)
{
  yyy <- 0
  #yyy <- l_theta(t, xx) + rnorm(n, 0, sigma)
  #yyy <- l_theta(t, xx) + 0.1 * rnorm(n, 0, sigma)
  yyy <- l_theta(t, xx) + (sigma * 0.1 * rnorm(n))
  
  ttt <- 1
  
  for(k in 1:100)
  {
    # numerator
    u <- sum((yyy - l_theta(ttt, xxx)) %*% l_theta_dash(ttt,xxx))
    # denominator
    d <- sum((yyy %*% l_theta_dash_dash(ttt, xxx)) - (l_theta_dash(ttt,xxx))^2 - (l_theta(ttt,xxx) %*% l_theta_dash_dash(ttt,xxx)))
    ttt <- ttt - u/d
  }
  
  #bbb[kk] <- l_theta(ttt, xxx)
  bbb[kk] <- ttt
}

var(bbb, na.rm = TRUE)
summary(bbb)