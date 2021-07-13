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

plot(xx, y)

model <- lm(y ~ exp ^ xx)

#view the output of the model
summary(model)


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



t <- 1

yh<-l_theta(t, xx)
eh<-y-yh

for(k in 2:100)
{
  # first derivative
  u <- sum((y - l_theta(t, xx)) %*% l_theta_dash(t,xx))
  # second derivative
  d <- sum((y %*% l_theta_dash_dash(t, xx)) - (l_theta_dash(t,xx))^2 - (l_theta(t,xx) %*% l_theta_dash_dash(t,xx)))
  t<- t - u/d
}

