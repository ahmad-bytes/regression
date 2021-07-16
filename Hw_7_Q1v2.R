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
  l_theta <- exp(theta * x_i)
}


l_theta_dash <- function (theta, x_i)
{
  l_theta_dash <- theta * exp(theta * x_i)
}

l_theta_dash_dash <- function (theta, x_i)
{
  l_theta_dash_dash <- theta^2 * exp(theta * x_i)
}

t <- 0.99

yh <- l_theta(t, xx)

sigma_squared = (sum(y - yh)^2) / (n-1)
sigma = sqrt(sigma_squared)

ttt <- 0
bbb <- 0
yyy <- 0

mean_xx = mean(xx)


for(kk in 1:1000)
{
  yyy <- l_theta(t, mean_xx) + sigma * rnorm(n)
  
  ttt[1] <- 1
  
  for(k in 2:10)
  {
    # numerator
    u <- sum((yyy - l_theta(ttt[k-1], xx)) * l_theta_dash(ttt[k-1],xx))
    # denominator
    d <- sum((yyy * l_theta_dash_dash(ttt[k-1], xx)) - (l_theta_dash(ttt[k-1],xx))^2 - (l_theta(ttt[k-1],xx) * l_theta_dash_dash(ttt[k-1],xx)))
    ttt[k] <- ttt[k-1] - u/d
  }
  
  bbb[kk] <- l_theta(ttt[k], mean_xx)
}

var(log(bbb))
sqrt(var(log(bbb)))