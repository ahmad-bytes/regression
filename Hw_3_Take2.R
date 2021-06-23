HW3_data <- read.csv("C:/Users/bilal/Dropbox/MS/RPractice/HomeWork/HW3_data.csv")
#View(HW3_data)

# initialize 
# ht<-0
# es<-0
# load the file as matrix
m = as.matrix(HW3_data)

#setup for model 1
# cut the y vector which is the first column
y_model1 <- m[,1]
# initialize x matrix, we have to push a vector of val 1 for first col.
xx_model1 <- matrix(nrow = length(y_model1) , ncol = 2)
# set the x matrix for model 3
xx_model1[,1] = 1
xx_model1[,2] <- m[,3]
xi_model1 <- solve(t(xx_model1) %*% xx_model1)
#model 1 is resolved
bh_model1 <- xi_model1 %*% t(xx_model1) %*% y_model1
paste(as.character(bh_model1), collapse=", ")

#setup for model 2
# cut the m vector which is the second column
# Y is m is this case
y_model2 <- m[,2]
# initialize x matrix, we have to push a vector of val 1 for first col.
xx_model2 <- matrix(nrow = length(y_model2) , ncol = 2)
# set the x matrix for model 2
xx_model2[,1] = 1
xx_model2[,2] <- m[,3]
xi_model2 <- solve(t(xx_model2) %*% xx_model2)
#model 2 is resolved
bh_model2 <- xi_model2 %*% t(xx_model2) %*% y_model2
paste(as.character(bh_model2), collapse=", ")

#setup for model 3
# cut the y vector which is the first column
y_model3 <- m[,1]
# initialize x matrix, we have to push a vector of val 1 for first col.
xx_model3 <- matrix(nrow = length(y_model3) , ncol = 3)
# set the x matrix for model 3
xx_model3[,1] = 1
xx_model3[,2] <- m[,2]
xx_model3[,3] <- m[,3]
xi_model3 <- solve(t(xx_model3) %*% xx_model3)
#model 3 is resolved
bh_model3 <- xi_model3  %*% t(xx_model3) %*% y_model3
paste(as.character(bh_model3), collapse=", ")

frame <- as.data.frame(m)
multi.fit = lm(frame$y ~ frame$m + frame$x, data=frame)
summary(multi.fit)

vcov(summary(multi.fit))


