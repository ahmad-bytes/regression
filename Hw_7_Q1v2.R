# read the file
HW7_data <- read.csv("C:/Users/bilal/Dropbox/MS/RPractice/HomeWork/HW7_data.csv")
#print summary so we can verify file is loaded
summary(HW7_data)

# cut x and y vectors from data

y <- 0
xx <- 0

m = as.matrix(HW7_data)
# cut the y vector which is the first column
y <- m[,1]
# cut the x matrix which is the second column
xx <- m[,2]

n = length(y)

t <- 1
u <- 0
d <- 0

t <- 0.99025
sigma <- 1.0051
sigma_squared <- 1.0103

ttt <- 0
bbb <- 0
yyy <- 0
mean_xx = mean(xx)

log_y = mean_xx * t

for(kk in 1:10000)
{
  yyy <- exp(t * mean_xx) + (1 * rnorm(n))
  ttt <- 1
  
  for(k in 2:10)
  {
    # numerator
    u <- sum(2 * xx * exp(ttt * xx) * (exp(ttt * xx) - yyy))
    # denominator
    d <- sum(2 * xx^2 * exp(ttt * xx) * (2 * exp(ttt * xx) - yyy))
    ttt <- ttt - u/d
  }
  bbb[kk] <- (ttt * mean_xx)
}

var_bbb <- var(bbb, na.rm = TRUE)
sigma_bbb = sqrt(var_bbb)

log_y
var_bbb
sigma_bbb
sd(bbb)


yyy <- 0
ddd <- 0

for(kk in 1:10000)
{
  yyy <- (t * mean_xx) + (1 * rnorm(n))
  ttt <- 1
  
  for(k in 2:10)
  {
    # numerator
    u <- sum(xx * ((ttt * xx) - yyy))
    # denominator
    d <- sum(xx^2)
    ttt <- ttt - u/d
  }
  ddd[kk] <- ttt * mean_xx
}

var_ddd <- var(ddd, na.rm = TRUE)
sigma_ddd = sqrt(var_ddd)
sd_ddd = sd(ddd)

log_y
var_ddd
sigma_ddd
