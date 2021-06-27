#credit_card_data <- read.delim("E:/GA course/ISYE 6501 Analytics Modeling/credit_card_data.txt", header=true)
##library(kernlab)
##read data and stored as a matrix
rm(list = ls())
#mydata <- read.delim("E:/ISYE 6501 Analytics Modeling/Homework 1/credit_card_data-headers.csv", header= TRUE, sep=",")
mydata <- read.table("E:/GA course/ISYE 6501 Analytics Modeling/Homework 1/credit_card_data-headers.csv", header= TRUE, sep=",")
data=data.matrix(mydata)
data
library(kernlab)
plot(mydata)
##ksvm(x, data = NULL, ..., subset, na.action = na.omit, scaled = TRUE)
# call ksvm.  Vanilladot is a simple linear kernel. 
model <- ksvm( data[,1:10],data[,11],type = "C-svc", kernel= "vanilladot", C=100, scaled=TRUE) 
# calculate a1.am
model
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a 
# calculate a0 
a0 <- -model@b 
a0 
# see what the model predicts 
pred <- predict(model,data[,1:10]) 
pred 
# see what fraction of the model's predictions match the actual classification 
sum(pred == data[,11]) / nrow(data) 

           