library(sets)
# read the file
HW10_data <- read.csv("C:/Users/bilal/Dropbox/MS/RPractice/HomeWork/HW10_data.csv")
#print summary so we can verify file is loaded
summary(HW10_data)

y <- 0
x <- 0
i <- 0

y <- HW10_data$y
x <- HW10_data$x

data <- data.frame(y,x)
df <- data[order(x),]

n = length(y)

r <- 0
index <- 0
x1 <- 0
x2 <- 0

min_r = 100
split <- 0

i <- 1

debugtuple <- 0

for(i in 1:99)
{
  r <- (df$x[i] + df$x[i+1])/2
  #print(sprintf('r = %f %f %f' , r, df$x[i], df$x[i+1]))
  index <- 0
  split1 <- 0
  split2 <- 0
  for(j in 1:n)
  {
    #print(sprintf('i = %f, j= %f', i, j))
    if (df$x[j] <= r)
    {
      split1 <- split1 + df$y[j]
      index <- index + 1
    }
    else
    {
      split2 <- split2 + df$y[j]
    }
  }
  
  split1 <- split1 / index
  split2 <- split2 / (n - index)
  
  sum_split_1 <- 0
  sum_split_2 <- 0
  
  for(k in 1:n)
  {
    if (df$x[k] <= r)
    {
      sum_split_1 <- sum_split_1 + (df$y[k] - split1)^2
    }
    else
    {
      sum_split_2 <- sum_split_2 + (df$y[k] - split2)^2
    }
  }
  if (min_r > sum_split_1 + sum_split_2)
  {
    min_r <- sum_split_1 + sum_split_2
    split <- r
  }
  
  #print(sprintf('i= %f, r= %f, split1= %f, split2= %f, sum_split_1= %f, sum_split_1= %f, sum_split_1 + sum_split_2= %f', i, r, split1, split2, sum_split_1, sum_split_1, sum_split_1 + sum_split_2))
  print(sprintf('%f,%f,%f,%f,%f,%f,%f', i, r, split1, split2, sum_split_1, sum_split_2, sum_split_1 + sum_split_2))
}