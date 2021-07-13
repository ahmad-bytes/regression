# read the file
HW61_data <- read.csv("C:/Users/bilal/Dropbox/MS/RPractice/HomeWork/HW61_data.csv")
#print summary so we can verify file is loaded
summary(HW61_data)

one_way <- aov(val ~ Treatment, data = HW61_data)

summary(one_way)


# read the file
HW6_data <- read.csv("C:/Users/bilal/Dropbox/MS/RPractice/HomeWork/HW6_data.csv")
#print summary so we can verify file is loaded
summary(HW6_data)

two_way <- aov(grade ~ program + gender + program:gender, data = HW6_data)

summary(two_way)