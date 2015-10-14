#imbalanced sample
data(iris)
require(DMwR)
data <- iris[,c(1,2,5)]
data$Species <- factor(ifelse(data$Species == 'setosa','rare','common'))
newData <- SMOTE(Species~.,data,perc.over=600)
table(newData$Species)
table(data$Species)

# plot
par(mfrow=c(1,2))
plot(data[,1],data[,2],pch=19+as.integer(data[,3]),
     main='Original Data')
plot(newData[,1],newData[,2],pch=19+as.integer(newData[,3]),
     main="SMOTE'd Data")