# read the file
HW10_data <- read.csv("C:/Users/bilal/Dropbox/MS/RPractice/HomeWork/HW10_data.csv")
#print summary so we can verify file is loaded
summary(HW10_data)

class(HW10_data)

# function is y = e(tx)
# first derivative is te(tx)
# second derivative is t^2e(tx)

# cut x and y vectors from data

y <- 0
x <- 0
mh <- 0
d <- 0

#m = as.matrix(HW10_data)
# cut the y vector which is the first column
#y <- m[,1]
# cut the x matrix which is the second column
#xx <- m[,2]

y <- HW10_data$y
x <- HW10_data$x

n = length(y)
hh <- 0.1


gaussian_kernel <- function(data, value, band_width)
{
  gaussian_kernel <- exp(-0.5*(data - value)^2/band_width^2)
}

uniform_kernel <- function(data, value, band_width)
{
  if (abs(value - data) < band_width)
  {
    result <- 1
  }
  else
  {
    result <- 0
  }
  
  uniform_kernel <- result
}

plot(x, y)

s <- 0
m_x <- 0

xx <- 0

for(i in 1:n)
{
  s <- s + uniform_kernel(m_x, x[i], hh)
  xx[i] <- y[i] * uniform_kernel(m_x, x[i], hh)
}

m_h <- sum(xx) / s


numerator <- 0
denominator <- 0

for(i in 1:n)
{
  denominator <- denominator + uniform_kernel(m_x, x[i], hh)
  # numerator <- uniform_kernel(m_x, x[i], hh) * abs(m_x - x[i])
  numerator <- uniform_kernel(m_x, x[i], hh) * (x[i] - m_x)
}

bias <- 1.2 * sum(numerator) / denominator

numerator_sum <- 0
denominator <- 0

for(i in 1:n)
{
  denominator <- denominator + uniform_kernel(m_x, x[i], hh)
  numerator_sum[i] <- uniform_kernel(m_x, x[i], hh)^2
}

variance_h <- sum(numerator_sum / denominator^2)
