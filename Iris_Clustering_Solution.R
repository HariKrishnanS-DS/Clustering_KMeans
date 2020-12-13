# Installing the required packages
install.packages("tidyverse")
install.packages("reshape2")
install.packages("cluster")

library(tidyverse)
library(reshape2)
library(cluster)

#Setting the working directory
setwd("D:\\Projects\\Hari\\Personal\\Internship")
iris_data <- read.csv("Iris.csv")

#Summary of the Iris data
View(iris_data)
head(iris_data,4)
tail(iris_data,4)
summary(iris_data)

#Visualizing the sepal with species 
ggplot(iris_data)+
  geom_point(aes(x=SepalLengthCm, y=SepalWidthCm), stroke=2)+
  facet_wrap(~Species)+
  labs(x='Sepal Length', y='Sepal Width')+
  theme_bw()

# visualizing the petal with species
ggplot(iris_data)+
  geom_point(aes(x=PetalLengthCm, y=PetalWidthCm),stroke=2)+
  facet_wrap(~Species)+
  labs(x='Petal Lenght', y='Petal Width')+
  theme_bw()

# Visualizing the sepal & petal length
ggplot(iris_data)+
  geom_point(aes(x=SepalLengthCm, y=PetalLengthCm), stroke=2)+
  facet_wrap(~Species)+
  labs(x='SepalLength', y='PetalLength')+
  theme_bw()

# Visualizing the sepal & petal width
ggplot(iris_data)+
  geom_point(aes(x=SepalWidthCm, y=PetalWidthCm), stroke=2)+
  facet_wrap(~Species)+
  labs(x='SepalLength', y='PetalLength')+
  theme_bw()

#Visualizing the data in box plot
ggplot(iris_data)+
  geom_boxplot(aes(x=Species, y=SepalLengthCm, fill=Species))+
  theme_bw()

ggplot(iris_data)+
  geom_boxplot(aes(x=Species, y=SepalWidthCm, fill=Species))+
  theme_bw()

ggplot(iris_data)+
  geom_boxplot(aes(x=Species, y=PetalLengthCm, fill=Species))+
  theme_bw()

ggplot(iris_data)+
  geom_boxplot(aes(x=Species, y=PetalWidthCm, fill=Species))+
  theme_bw()

#Removing the class variable
iris_new<-iris_data[,c(2,3,4,5)]
iris_class<-iris_data[,"Species"]
head(iris_new,4)

#Normalizing the features
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))}
iris_new$SepalLengthCm<-normalize(iris_new$SepalLengthCm)
iris_new$SepalWidthCm<-normalize(iris_new$SepalWidthCm)
iris_new$PetalLengthCm<-normalize(iris_new$PetalLengthCm)
iris_new$PetalWidthCm<-normalize(iris_new$PetalWidthCm)
head(iris_new)

#K-Means clustering
result<-kmeans(iris_new,3)
result$size
result$centers
result$cluster

#Plotting the clusters
par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris_new[c(1,2)],col=result$cluster)
plot(iris_new[c(1,2)],col=iris_class)
plot(iris_new[c(3,4)],col=result$cluster)
plot(iris_new[c(3,4)],col=iris_class)
table(result$cluster,iris_class)
