# read the file
HW61_data <- read.csv("C:/Users/bilal/Dropbox/MS/RPractice/HomeWork/HW61_data.csv")
#print summary so we can verify file is loaded
summary(HW61_data)

one_way <- aov(val ~ Treatment, data = HW61_data)

summary(one_way)