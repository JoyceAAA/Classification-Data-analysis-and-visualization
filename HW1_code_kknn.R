#kknn(formula = formula(train), train, test, na.action = na.omit(), k = 7, distance = 2, kernel = "optimal",  scale=TRUE)
##read data and stored as a matrix
mydata <- read.delim("E:/ISYE 6501 Analytics Modeling/Homework 1/credit_card_data-headers.csv", header= TRUE, sep=",")
data=data.matrix(mydata)
data=as.data.frame(data)
library(kknn)
m <- dim(data)[1]
##nrow(data)
##plot(mydata matrix)
# use 2/3 data to train, 1/3 data to test. get prediction result//scale = TRUE,
val<- sample(1:m,size = round(m/3), replace = FALSE,prob=rep(1/m,m)) 
data.learn<- data[-val,]
data.test<- data[val,]
data.kknn <- kknn(R1 ~.,data.learn,data.test, k = 4,scale = TRUE,
                  kernel = "optimal")
# see what fraction of the model's predictions match the actual classification 
##kernel = c("optimal","rectangular", "rank", "gaussian","triangular")
summary(data.kknn)
fit<- fitted(data.kknn)
table(data.test$R1,fit)
Accuracy = sum(as.numeric(fit)==as.numeric(data.test$R1))/nrow(data.test)
Accuracy

