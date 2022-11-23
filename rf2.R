install.packages('randomForest')
library(randomForest)
library(datasets)
data<-iris
data
head(data)
str(data)
summary(data)
#split the dataset
ind<- sample(2, nrow(iris), replace=TRUE, prob=c(0.7,0.3))
iris_train<-iris[ind==1,]
iris_test<-iris[ind==2,]
#implement the random forest
iris_df<-randomForest(Species~., data=iris_train, ntree=1000, proximity=TRUE)
iris_df
#error rate is 4.17%

#building the random forest algorithm for test data
iris_pred<- predict(iris_df, newdata=iris_test)
table(iris_pred, iris_test$Species)

#check the accuracy
print(length(iris_test$Species))
print(sum(iris_pred==iris_test$Species))
print(sum(iris_pred==iris_test$Species)/length(iris_test$Species))
#the accuracy is 93.33%