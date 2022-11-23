#RANDOM FOREST IMPLEMENTATION ON WINE QUALITY PREDICTION
wine<-read.csv(url('https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv'), header=TRUE, sep= ';')
head(wine)
dim(wine)
barplot(table(wine$quality))

#convert the quality values to factors.
wine$taste<- ifelse(wine$quality<5, 'bad', 'good')
wine$taste[(wine$quality==5)] <- 'normal'
wine$taste[(wine$quality == 6)]<- 'normal'
wine$taste<-as.factor(wine$taste)
str(wine$taste)
table(wine$taste)

#split the dataset to train(80%) and test(20%)
set.seed(123)
samp<- sample(nrow(wine), 0.8*nrow(wine))
train<- wine[samp,]
test<- wine[-samp,]

#data visualization
library(ggplot2)
ggplot(wine, aes(fixed.acidity, volatile.acidity)) +
  geom_point(aes(color=taste))
ggplot(wine,aes(alcohol))+
  geom_histogram(aes(fill=taste), color='black', bins=50)
dim(train)
dim(test)

#install the random forest algorithm
install.packages('randomForest')
library(randomForest)

#build the random forest model and train the data
model <- randomForest(taste ~ . - quality, data = train, ntree = 1000, mtry = 5)
model #really high error.
model$confusion

#validate the model by test data
pred<- predict(model, newdata=test)
table(pred, test$taste)
pred

#display the predicted values and compare with actual values
result<- cbind(pred, test$taste)
result
colnames(result)<- c('prediction', 'reality')
result<- as.data.frame(result)
result

#calculate the accuracy of the model
sum(pred==test$taste)/nrow(test)
#90% is a good rate of accuracy.